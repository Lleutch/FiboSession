module Helpers =

    let sep = ":"B
    let sepEnd = "."B
    let (<+>) = Array.append

    
    type IFailure =
        abstract member Description: string

    [<NoComparison>]
    [<NoEquality>]
    type Result<'a> =
        | Success of 'a
        | Failure of IFailure
 
    [<NoComparison>]
    [<NoEquality>]
    type ResultWrapped<'a> = 
        | ResultWrapped of (unit -> Result<'a>)
        member x.Result =
            let (ResultWrapped f) = x
            f()

    type ResultBuilder() =
        member __.Bind(ResultWrapped m, f) =
            match m() with
            |Success elem -> f elem
            |Failure s -> Failure s

        member __.Return x = Success x
        member __.ReturnFrom(ResultWrapped m) = m()
        member __.Zero () = Success ()

        member __.Combine (a:Result<'a>,b)= 
            let runnedB = b()
            match a,runnedB with 
            |Success a1 , Success b1      -> Success b1
            |Success a1 , Failure b1    -> Failure b1
            |Failure a1 , Success b1    -> Failure a1
            |Failure a1 , Failure b1  -> Failure a1

        member __.Delay(f:unit -> Result<'a>) = f

        member __.Run(delayed) = ResultWrapped delayed

            
    let result = ResultBuilder()


    let createFailure (failure:#IFailure) = 
        fun() -> Failure failure
        |> ResultWrapped

    let run (oct:ResultWrapped<'a>) = 
        let (ResultWrapped oct) = oct
        oct() 

    type Failure =
        | Deserialization of string
        interface IFailure with
            member this.Description =
                match this with
                | Deserialization str -> sprintf "[Deserialization Error] : %s" str


module Types =
    type IAction = 
        | Serialize
        | Deserialize

    type Send = 
        | Send
        static member Action = Serialize
        static member inline IsSame< ^T when ^T : (static member Action : IAction)>() = typeof<'T> = typeof<Send>

    and Receive = 
        | Receive
        static member Action = Deserialize
        static member inline IsSame< ^T when ^T : (static member Action : IAction)>() = typeof< ^T> = typeof<Receive>
               
    type Role =
        | S
        | C

module Labels =
    open Types
    open System
    open Helpers
    open System.IO

    let private readData (input:byte[]) (data:byte[]) =
        result{
            let index = input.Length 
            let head,tail= Array.splitAt index data
            if input = head then
                return tail
            else
                return! Deserialization (sprintf "Expected %A but received %A" input head ) |> createFailure
        }


    let readInt (data:byte[]) = 
        result{
            let index = 4
            let head,tail= Array.splitAt index data
            return (BitConverter.ToInt32(head,0),tail)
        }


    let readHello = readData "HELLO"B        
    let readBye = readData "BYE"B 
    let readAdd = readData "ADD"B 
    let readRes = readData "RES"B 
    

    let readSep     = readData sep
    let readSepEnd  = readData sepEnd
    

    (*** *************************************************** ***)


    type System.Int32 with  
        member x.Send(stream:BinaryWriter) = stream.Write x

        static member Receive(stream:BinaryReader) = result{ return stream.ReadInt32() }
        
    type Hello = 
        | Hello of int
        member x.Send(stream:BinaryWriter) = 
            let (Hello value) = x
            stream.Write "HELLO"B 
            stream.Write sep 
            value.Send(stream) 
            stream.Write sepEnd

        static member Receive(stream:BinaryReader) =
            result{
                let data = stream.ReadBytes(11)
                printfn "%A" data
                let! data = readHello data
                let! data = readSep data
                let! value,data = readInt data
                let! data = readSepEnd data
                return Hello value
            }

    type Add = 
        | Add of int 
        member x.Send(stream:BinaryWriter) = 
            let (Add value) = x
            stream.Write "ADD"B 
            stream.Write sep 
            value.Send(stream) 
            stream.Write sepEnd

        static member Receive(stream:BinaryReader) =
            result{
                let data = stream.ReadBytes(6)

                let! data = readSep data
                let! value,data = readInt data
                let! data = readSepEnd data
                return Add value
            }
        
    type Bye = 
        | Bye
        member x.Send(stream:BinaryWriter) = 
            stream.Write "BYE"B 
            stream.Write sepEnd

        static member Receive(stream:BinaryReader) =
            result{
                let data = stream.ReadBytes(1)

                let! data = readSepEnd data
                return Bye
            }

    type Choice =
        | ChoiceBye of Bye
        | ChoiceAdd of Add
        member x.Send(stream:BinaryWriter) = 
            match x with 
            | ChoiceBye bye -> bye.Send(stream)
            | ChoiceAdd add -> add.Send(stream)

        static member Receive(stream:BinaryReader) =
            result{
                let data = stream.ReadBytes(3)
                if data = "BYE"B then
                    let! bye = Bye.Receive(stream) 
                    return bye |> ChoiceBye
                elif data = "ADD"B then
                    let! add = Add.Receive(stream)
                    return add |> ChoiceAdd
                else
                    return! Deserialization (sprintf "Expected %A or %A but received %A" "BYE"B "ADD"B data) |> createFailure
            }


    type Res = 
        | Res of int
        member x.Send(stream:BinaryWriter) = 
            let (Res value) = x
            stream.Write "RES"B 
            stream.Write sep 
            value.Send(stream) 
            stream.Write sepEnd

        static member Receive(stream:BinaryReader) =
            result{
                let data = stream.ReadBytes(9)

                let! data = readRes data
                let! data = readSep data
                let! value,data = readInt data
                let! data = readSepEnd data
                return Res value
            }

module FibServer =
    open System.Net
    open System.Net.Sockets
    open Types
    open Labels
    open Helpers 
    open System.IO
    

    let rec executeServer (previous:int) (acc:int) (writeStream:NetworkStream) (readStream:NetworkStream) =
        result{
            let reader = new BinaryReader(readStream)
            let writer = new BinaryWriter(writeStream)
            printfn "start"
            let! hello = Hello.Receive(reader)
            printfn "start1"
            let! choice = Choice.Receive(reader)
            printfn "start2"
            match choice with
            | ChoiceBye _ ->
                do Bye.Bye.Send(writer)
                return acc
            | ChoiceAdd (Add value) ->
                let acc1 = previous + value
                Res.Res(acc1).Send(writer)
                return! executeServer value acc1 writeStream readStream
         }
          
    let agentServ (port) =
        new MailboxProcessor<unit>(fun inbox ->
            let rec loop() = 
                async {
                    printfn "Start"
                    let! msg = inbox.Receive()
                    printfn "after"

                    let server = new TcpListener(IPAddress.Parse("127.0.0.1"),5000)
                    server.Start()
                    let readStream = server.AcceptTcpClient().GetStream()
                    let writeStream = 
                        let clt = new TcpClient("127.0.0.1",5001)
                        clt.GetStream()
                    printfn "test"

                    let result = executeServer 1 1 writeStream readStream
                    match run result with
                    | Failure failure -> printfn "%s" failure.Description
                    | Success success -> printfn "Server Done : Sent value %i " success
                }
            loop()
        ) 

    let agentServer = agentServ 5000
    agentServer.Start()
    agentServer.Post()

module FibClient =
    open System.Net
    open System.Net.Sockets
    open Types
    open Labels
    open Helpers 
    open System.IO
    
    let rec executeClient (iter:int) (acc:int) (writeStream:NetworkStream) (readStream:NetworkStream) =
        result{
            let reader = new BinaryReader(readStream)
            let writer = new BinaryWriter(writeStream)
            
            do Hello.Hello(0).Send(writer)
            if iter = 0 then
                do Bye.Bye.Send(writer)
                let data = reader.ReadBytes(3)
                let! data = readBye data
                let! bye = Bye.Receive(reader)
                return acc
            else
                do Add.Add(acc).Send(writer)
                let! (Res res) = Res.Receive(reader)
                return! executeClient (iter - 1) res writeStream readStream
         }


    let agentClt (port) =
        new MailboxProcessor<unit>(fun inbox ->
            let rec loop() = 
                async {
                    let! msg = inbox.Receive()

                    let writeStream = 
                        let clt = new TcpClient("127.0.0.1",5000)
                        clt.GetStream()
                    let server = new TcpListener(IPAddress.Parse("127.0.0.1"),5001)
                    server.Start()
                    let readStream = server.AcceptTcpClient().GetStream()
                    printfn "test"
                        
                    let result = executeClient 10 1 writeStream readStream
                    match run result with
                    | Failure failure -> printfn "%s" failure.Description
                    | Success success -> printfn "Client Done : Received value %i " success
                }
            loop()
        ) 

    let agentClient = agentClt 5001
    agentClient.Start()
    agentClient.Post()






[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code
