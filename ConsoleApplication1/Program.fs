
open System.IO
open System.Diagnostics

module TimeMeasure = 
    let mutable stopWatch = System.Diagnostics.Stopwatch.StartNew()
    let path = "C:/Users/rn710/Repositories/GenerativeTypeProviderExample/Evaluation/"
    let file = "C:/Users/rn710/Repositories/GenerativeTypeProviderExample/Evaluation/tempfib.txt"

    let start() = 
        stopWatch.Stop()
        stopWatch <- System.Diagnostics.Stopwatch.StartNew()

    let measureTime (step:string) = 
        stopWatch.Stop()
        let numSeconds = stopWatch.ElapsedTicks / Stopwatch.Frequency
        let curTime = sprintf "%s: %i \r\n" step stopWatch.ElapsedMilliseconds
        File.AppendAllText(file, curTime)
        stopWatch <- Stopwatch.StartNew()

module Helpers =

    let sep = ":"B
    let sepEnd = "."B
    let (<+>) = Array.append

    
    type IFailure =
        abstract member Description: string

   (* [<NoComparison>]
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

            
    let result = ResultBuilder()*)


    //let createFailure (failure:#IFailure) = 
     //   fun() -> Failure failure
        //|> ResultWrapped

    (*let run (oct:ResultWrapped<'a>) = 
        let (ResultWrapped oct) = oct
        oct() *)

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
//    open Types
    open System
    open Helpers
    open System.IO

    let private readData (input:byte[]) (data:byte[]) =
            let index = input.Length 
            let head,tail= Array.splitAt index data
            if input = head then
                tail
            else
                failwith "Expected %A but received %A" input head
                //Deserialization (sprintf "Expected %A but received %A" input head ) //|> createFailure


    let readInt (data:byte[]) = 
            let index = 4
            let head,tail= Array.splitAt index data
            (BitConverter.ToInt32(head,0),tail)


    let readHello = readData "HELLO"B        
    let readBye = readData "BYE"B 
    let readAdd = readData "ADD"B 
    let readRes = readData "RES"B 
    

    let readSep     = readData sep
    let readSepEnd  = readData sepEnd
    

    (*** *************************************************** ***)


    type System.Int32 with  
        member x.Send(stream:BinaryWriter) = stream.Write x

        static member Receive(stream:BinaryReader) = stream.ReadInt32()
        
    type Hello = 
        | Hello of int
        member x.Send(stream:BinaryWriter) = 
            let (Hello value) = x
            stream.Write "HELLO"B 
            stream.Write sep 
            value.Send(stream) 
            stream.Write sepEnd

        static member Receive(stream:BinaryReader) =
                let data = stream.ReadBytes(11)
                //printfn "%A" data
                let data = readHello data
                let data = readSep data
                let value,data = readInt data
                let data = readSepEnd data
                Hello value

    type Add = 
        | Add of int 
        member x.Send(stream:BinaryWriter) = 
            let (Add value) = x
            stream.Write "ADD"B 
            stream.Write sep 
            value.Send(stream) 
            stream.Write sepEnd

        static member Receive(stream:BinaryReader) =
                let data = stream.ReadBytes(6)
                let data = readSep data
                let value,data = readInt data
                let data = readSepEnd data
                Add value
        
    type Bye = 
        | Bye
        member x.Send(stream:BinaryWriter) = 
            stream.Write "BYE"B 
            stream.Write sepEnd

        static member Receive(stream:BinaryReader) =
                let data = stream.ReadBytes(1)
                let data = readSepEnd data
                Bye

    type Choice =
        | ChoiceBye of Bye
        | ChoiceAdd of Add
        member x.Send(stream:BinaryWriter) = 
            match x with 
            | ChoiceBye bye -> bye.Send(stream)
            | ChoiceAdd add -> add.Send(stream)

        static member Receive(stream:BinaryReader) =
                let data = stream.ReadBytes(3)
                if data = "BYE"B then
                    let bye = Bye.Receive(stream) 
                    bye |> ChoiceBye
                elif data = "ADD"B then
                    let add = Add.Receive(stream)
                    add |> ChoiceAdd
                else
                    failwith "Expected %A or %A but received %A" "BYE"B "ADD"B data
                    //Deserialization (sprintf "Expected %A or %A but received %A" "BYE"B "ADD"B data) //|> createFailure


    type Res = 
        | Res of int
        member x.Send(stream:BinaryWriter) = 
            let (Res value) = x
            stream.Write "RES"B 
            stream.Write sep 
            value.Send(stream) 
            stream.Write sepEnd

        static member Receive(stream:BinaryReader) =
            
                let data = stream.ReadBytes(9)

                let data = readRes data
                let data = readSep data
                let value,data = readInt data
                let data = readSepEnd data
                Res value

module FibServer =
    open System.Net
    open System.Net.Sockets
   // open Types
    open Labels
    open Helpers 
    open System.IO
    

    let rec executeServer (previous:int) (acc:int) (writer:BinaryWriter) (reader:BinaryReader) =
            //printfn "start"
            let hello = Hello.Receive(reader)
            //printfn "start1"
            let choice = Choice.Receive(reader)
            //printfn "start2"
            match choice with
            | ChoiceBye _ ->
                Bye.Bye.Send(writer)
                acc
            | ChoiceAdd (Add value) ->
                //let acc1 = previous + value
                let ass = if value > 0 
                          then 0  else 1  
                Res.Res(value).Send(writer)
                executeServer value value writer reader
          
    let agentServ (port) =
        new MailboxProcessor<unit>(fun inbox ->
            let rec loop() = 
                async {
                    //printfn "Start"
                    let! msg = inbox.Receive()
                    //printfn "after"
                    let server = new TcpListener(IPAddress.Parse("127.0.0.1"),5000)
                    server.Start()
                    let readStream = server.AcceptTcpClient().GetStream()
                    let writeStream = 
                        let clt = new TcpClient("127.0.0.1",5001)
                        clt.GetStream()
                    //printfn "test"
                    let reader = new BinaryReader(readStream)
                    let writer = new BinaryWriter(writeStream)
                    let hello = Hello.Receive(reader)
                    Hello.Hello(0).Send(writer)
                    let result = executeServer 1 1 writer reader
                    printfn "Server Done : Sent value %i " result
                    //match result with
                    //| Failure failure -> printfn "%s" failure.Description
                    //| Success success -> printfn "Server Done : Sent value %i " success
                }
            loop()
        ) 

    let StartServer () = 
        let agentServer = agentServ 5000
        agentServer.Start()
        agentServer.Post()

module FibClient =
    open System.Net
    open System.Net.Sockets
   // open Types
    open Labels
    open Helpers 
    open System.IO
    //NetworkStream
    let rec executeClient (iter:int) (acc:int) (writer:BinaryWriter) (reader:BinaryReader) =
            Hello.Hello(0).Send(writer)
            //printf "Iter: %i" iter
            if iter = 0 then
                do Bye.Bye.Send(writer)
                let data = reader.ReadBytes(3)
                let data = readBye data
                let bye = Bye.Receive(reader)
                acc
            else
                Add.Add(acc).Send(writer)
                let (Res res) = Res.Receive(reader)
                executeClient (iter - 1) res writer reader
         


    let agentClt (port) =
        new MailboxProcessor<unit>(fun inbox ->
            let rec loop() = 
                async {
                    let msg = inbox.Receive()

                    let writeStream = 
                        let clt = new TcpClient("127.0.0.1",5000)
                        clt.GetStream()
                    let server = new TcpListener(IPAddress.Parse("127.0.0.1"),5001)
                    server.Start()
                    let readStream = server.AcceptTcpClient().GetStream()
                    //printfn "test"
                    let reader = new BinaryReader(readStream)
                    let writer = new BinaryWriter(writeStream)
                    Hello.Hello(0).Send(writer)
                    let hello = Hello.Receive(reader)

                    TimeMeasure.start() 
                    TimeMeasure.measureTime "before TCP start"
                    let result = executeClient 10000 1 writer reader
                    //match result with
                    printfn "Client Done : Received value %i " result
                    TimeMeasure.measureTime "TCP time"
                    (*| Failure failure -> printfn "%s" failure.Description
                    | Success success -> 
                        //printfn "Client Done : Received value %i " success
                        TimeMeasure.measureTime "TCP time"*)
                }
            loop()
        ) 
    let startClinet () = 
        let agentClient = agentClt 5001
        agentClient.Start()
        agentClient.Post()



[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    if argv.[0] = "C" then 
        printfn "Starting the client"
        FibClient.startClinet ()
    else
        printfn "Starting the client" 
        FibServer.StartServer ()
    System.Console.Read() // return an integer exit code
