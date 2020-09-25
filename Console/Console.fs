module Console
open SpiderSolitare
open System
open System.Threading.Tasks
open System.IO
open System.Runtime
open Persisance
open Brain
open ImageMagick
open SpiderSolitare.MonteCarloTreeSearch
open SpiderSolitare.Game
open System.Diagnostics
open System.Drawing


type AppMove = 
    | GetMoves of AsyncReplyChannel<List<int * MoveType>>
    | PlayMove of int * AsyncReplyChannel<GameResult>
    | GetGame of AsyncReplyChannel<GameResult>

type ISaver = 
    // abstract member SaveGameMoves: bool ->  int -> (int * string * int * string) list -> unit
    abstract member SaveGameMoves: bool ->  int -> (int * string * int) list -> unit
    abstract member Finish: unit -> unit
    abstract member Format: unit -> unit


type MachineLearningApproach = 
    | RadnomMcts
    | MctsWithRestApi

type RunConfig = {
    MctsIterationCount: int 
    LoopCount: int
    MoveCount: int
    RandomMoveThreshold: float
    RandomDelta: float
    MachineLearningApproach: MachineLearningApproach
}

let parse skip data = 
    data |> Array.skip skip |> Array.take 4 |> Array.rev |> fun x -> BitConverter.ToInt32(x, 0)

let parseIdxData name data = 
    let magic = parse 0 data
    let images = parse 4 data
    let rows = parse 8 data
    let columns = parse 12 data
    printfn "name:%s -> %d %d %d %d" name magic images rows columns

    data 
    |> Array.skip 16
    |> Array.chunkBySize (rows * columns)
    |> Array.map (Array.chunkBySize rows)

let train network load epochs = 

    let args = 
        if load = "reload" then 
            sprintf "%s reload" epochs
        else 
            epochs

    let file = 
        match network with 
        | "v"
        | "value" -> "/Users/willsam100/projects/plaidML/train-value.sh"
        | "p"
        | "policy" -> "/Users/willsam100/projects/plaidML/train-policy.sh"
        | "m"
        | "mnist" -> "/Users/willsam100/projects/plaidML/mnist-value-net.sh"
        | "q"
        | "qlearn" -> "/Users/willsam100/projects/plaidML/q-learn-spider.sh"
        | "qv"
        | "qlearn-value" -> "/Users/willsam100/projects/plaidML/q-learn-spider-value.sh"
        | x -> failwithf "Invalid network to train: %s" x

    printfn "Training..."
    let train = new Process()
    train.StartInfo.FileName <- file
    train.StartInfo.Arguments <- args
    train.Start() |> ignore
    train


type QLearner(qLearning, messageFile) = 
    let mutable policy: Process = null
    let mutable value: Process = null

    member this.WaitForTraingingToStart() = 

        while File.Exists messageFile do 
            System.Threading.Thread.Sleep (TimeSpan.FromSeconds 0.5)
        File.WriteAllText (messageFile,"\n")

    member private this.CanStartTraining() = 
        if policy |> isNull |> not || value |> isNull |> not then false 
        else File.Exists qLearning        

    member this.Learn() = 
        if File.Exists messageFile |> not then 
            File.WriteAllText (messageFile,"\n")

        if this.CanStartTraining() |> not then 
            ()
        else

            if isNull policy && isNull value || policy.HasExited && value.HasExited then 
                policy <- train "q" "" ""
                value <- train "qv" "" ""
                // this.WaitForTraingingToStart()
            else 
                this.StopTraining()
                printfn "Waiting for training to complete..."
                policy.WaitForExit()
                value.WaitForExit()

                policy <- null
                value <- null
                this.Learn()

    member this.StopTraining() = 
        File.Delete messageFile


let rec run log (saver: ISaver) parallelCount config gameNumbers = 

    let updateHistory parentGame game move history = 
        let gameAndBestMove = 
            (List.length history + 1), 
            (parentGame  |> Representation.encodeKeyGame cardEncoder |> Seq.map string |> String.concat "," |> format26Stock), 
            (moveEncoder.Encode move |> encodeMove),
            (game |> Representation.encodeKeyGame cardEncoder |> Seq.map string |> String.concat "," |> format26Stock)
        gameAndBestMove :: history

    let getGames node =
        let rec loop acc (nodes: MonteCarloTreeSearch.MutableNode<'a, 'b> list) =  

            // let game = Representation.decodeKeyedGame MonteCarloTreeSearch.cardEncoder (node.Game.Split (',') |> Array.map Int32.Parse |> Array.toList)
            let children = nodes |> List.collect (fun x -> x.Children)
            loop (acc @ nodes) children

        loop [] [node]

    let playGamesForRange port range = 

        let brain = BrainMoverServer(port)
        let searcher = 
            match config.MachineLearningApproach with 
            | MctsWithRestApi ->    
                
                brain.StartServer()
                let brainsMover = BrainsMoverClient(port) :> IBransMover
                SearcherWithNeuralNetwork(brainsMover, log) :> ISearcher

            | RadnomMcts -> SearcherRandomer(log)  :> ISearcher

        range 
        |> List.map (fun x -> 
            let gameResult = MctsSpiderGameLoop.playGame log config.RandomMoveThreshold searcher updateHistory config.MctsIterationCount config.MoveCount x
            searcher.BrainServer |> Option.iter (fun brainsMover -> brainsMover.Flush())

            printfn "%A" gameResult.Game
            printfn "GameNumber: %d, Result: %s, MovesPlayed: %.0f" gameResult.GameNumber (if gameResult.IsWin  then "WIN" else "LOST") gameResult.MovesMade
            if List.isEmpty gameResult.Progress then 
                printfn "No progres"
            else                 
                gameResult.Progress |> List.map (fun x -> sprintf "%.2f" x) |> String.concat "," |> printfn "%s"

            printfn ""                

            // history |> List.map (fun x -> sprintf "%s,%b,%d" x isWin gameNumber) |> saver.SaveGameMoves)
            if gameResult.History |> List.isEmpty |> not then 
                gameResult.History  
                |> List.map (fun (x,y,z,_) -> x,y,z)
                |> saver.SaveGameMoves gameResult.IsWin gameResult.GameNumber 
            
            gameResult.GameNumber, gameResult.IsWin )
        |> fun xs -> 
            brain.Stop()
            searcher.BrainServer |> Option.iter (fun brainsMover -> brainsMover.Flush())
            xs

    let tasks = 
        gameNumbers
        |> List.toArray
        |> fun x -> Array.shuffle x; x
        |> Array.toList
        |> List.take parallelCount        
        |> List.splitInto parallelCount
        |> List.mapi (fun i x -> 5100 + i * 2, x)
        |> List.map (fun (port, range) -> Task.Run(fun () ->  playGamesForRange port range) )
        |> List.toArray    
        |> Task.WhenAll   

    // if not log then 
    //     qlearner.Learn()

    let results =    
        tasks 
        |> Async.AwaitTask
        |> Async.Catch
        |> Async.RunSynchronously
        |> function 
        | Choice1Of2 xs -> xs |> Array.collect List.toArray
        | Choice2Of2 e -> 
            raise e

    let winningRate: float = (results |> Array.filter snd |> Array.length |> float) / (float parallelCount)
    printfn "WINING RATE: %.2f" winningRate
    printfn "Games lost: %s" (results |> Array.filter (snd >> not) |> Array.map (fst >> string) |> String.concat ",")
    // qlearner.StopTraining()
    saver.Finish()

    if config.LoopCount > 0 then 
        printfn "Running policy: %d" config.LoopCount
        let config = {config with RandomMoveThreshold = Math.Min(0.99, config.RandomMoveThreshold + config.RandomDelta)}
        run log (saver: ISaver) parallelCount {config with LoopCount = config.LoopCount - 1} gameNumbers

[<EntryPoint>]
let main argv =
    let policyRaw = "/Users/willsam100/Desktop/spider-policy-raw.csv"
    let valueRaw = "/Users/willsam100/Desktop/spider-value-raw.csv"
    let qLearning = "/Users/willsam100/Desktop/spider-policy-net-train.csv"
    let qLearningBk = "/Users/willsam100/Desktop/message.csv"
    // let qlearner = QLearner(qLearning, qLearningBk)

    match argv with 
    // | [| "format-policy" |] -> Reformat.readAndFormatPolicy policyRaw
    // | [| "format-value" |] -> Reformat.readAndFormatValue valueRaw
    // | [| "format-moves" |] -> Reformat.readAndFormatValidMoves policyRaw
    // | [| "format-run" |] -> Reformat.readAndFormatRun valueRaw
    // | [| "format-all" |] ->  
    //     Reformat.readAndFormatPolicy policyRaw
    //     Reformat.readAndFormatValue valueRaw

    | [| "format-legacy" |] ->  

        GCSettings.LatencyMode <- GCLatencyMode.Batch
        if File.Exists policyRaw then 
            File.Delete (policyRaw)

        if File.Exists valueRaw then 
            File.Delete (valueRaw)

        let s = Saver(policyRaw, valueRaw, None)
    
        "/Users/willsam100/Desktop/spider-game-with-failure.csv"
        |> File.ReadAllLines
        |> Array.mapi Reformat.sequenceDataLegacy
        |> Array.groupBy (fun x -> x.GameNumber)
        |> Array.iter (fun (gn, xs) -> s.SaveLegacyFormat gn xs )

        s.Finish()
    | [| "play"; gameNumbers|] -> 
        let gameNumbers = 
            if gameNumbers.Contains "," then 
                gameNumbers.Split "," |> Array.toList |> List.map System.Int32.Parse
            else System.Int32.Parse gameNumbers |> List.singleton

        let parallelCount = 1
        let config = {
            MctsIterationCount = 10
            MoveCount = 50
            LoopCount = 0
            RandomMoveThreshold = 1.0
            RandomDelta = 0.
            MachineLearningApproach = MachineLearningApproach.RadnomMcts
        }

        let saver = 
            { new ISaver with 
                member ths.SaveGameMoves isWin gameNumber history = ()
                member this.Finish() = ()
                member this.Format() = () }

        run true saver parallelCount config gameNumbers


    | [| "generate" |] -> 

        Threading.ThreadPool.SetMinThreads(32, 32) |> ignore

        let gameNumbers = [50] // List.replicate 1 [ 1 .. 100000 ] |> List.concat
        let log = true
        let parallelCount = 1
        let config = {
            MctsIterationCount = 100
            LoopCount = 0
            MoveCount = 50
            RandomMoveThreshold = 0.9
            RandomDelta = 0.001
            MachineLearningApproach = MachineLearningApproach.RadnomMcts
        }

        let saver = 
            let s = Saver(policyRaw, valueRaw, Some qLearning)
            { new ISaver with 
                member ths.SaveGameMoves isWin gameNumber history = s.SaveGameMoves isWin gameNumber history
                member this.Finish() = s.Finish()
                member this.Format() = s.Format() }
        
        run log saver parallelCount config gameNumbers

        // if not log then 
        //     printfn "Training..."
        //     let train = Process.Start("/Users/willsam100/projects/plaidML/train.sh")
        //     train.WaitForExit()        

    | [| "train"; network; epochs; load |] -> 
        let p = train network load epochs
        p.WaitForExit()

    | [| "image"; |] -> 
        use image = new MagickImage(MagickColor.FromRgb(byte 255, byte  45, byte  0), 10, 6)
        
        image.GetPixels() |> Seq.iter (fun x -> x.Set([| byte 42; byte 45; byte 0   |]) )

        image.Write("/Users/willsam100/Desktop/image.png")
        
        let startInfo = ProcessStartInfo("open")
        startInfo.RedirectStandardInput <- true
        startInfo.RedirectStandardOutput <- true
        startInfo.UseShellExecute <- false
        startInfo.Arguments <- "/Users/willsam100/Desktop/image.png"
        startInfo.WindowStyle <- ProcessWindowStyle.Hidden
        Process.Start(startInfo) |> ignore

    | [| "balance"; file |] ->  
        // Reformat.reBalance file
        Reformat.augment policyRaw

    // | [| "play-as-human"; gameNumber |]

    //     let myAgent rand = 
    //         MailboxProcessor.Start<AppMove>(fun inbox -> 

    //             let rec loop gameResult = 
    //                 async { 
    //                     let! msg = inbox.Receive()
    //                     match msg with
    //                     | GetGame rc -> 
    //                         gameResult |> rc.Reply
    //                         return! loop gameResult
    //                     | GetMoves rc -> 
    //                         match gameResult with 
    //                         | Lost g -> rc.Reply []
    //                         | Won -> rc.Reply []
    //                         | Continue (g, moves) -> moves |> List.indexed |> rc.Reply 
    //                         return! loop gameResult
    //                     | PlayMove(moveIndex, rc) -> 
    //                         match gameResult with 
    //                         | Continue (game,moves) -> 
    //                             let move = moves |> List.indexed |> List.tryFind (fun (x,y) -> x = moveIndex)
    //                             match move with 
    //                             | None -> 
    //                                 rc.Reply gameResult
    //                                 return! loop gameResult
    //                             | Some (_, move) -> 
    //                                 let gameResult = GameMover.playMove move game
    //                                 rc.Reply gameResult
    //                                 return! loop gameResult
    //                         | _ -> 
    //                             rc.Reply gameResult
    //                             return! loop gameResult
    //                 }

    //             loop (GameMover.startGame (Card.deck One) rand))
        
    //     let start (gameAgent: MailboxProcessor<AppMove>) = 
    //         gameAgent.PostAndReply GetGame |> printGameResult |> printfn "%s"

    //     let playMoveAtIndex (gameAgent: MailboxProcessor<AppMove>) indexMove = 
    //         printfn "Playing: %d" indexMove
    //         (fun rc -> PlayMove(indexMove, rc))
    //         |> gameAgent.PostAndReply

    //     let playAndPrint gameAgent indexMove = 
    //         playMoveAtIndex gameAgent indexMove
    //         |> (fun x -> printfn "Scored: %f" <| GameOperations.getReward x; x)
    //         |> printGameResult
    //         |> printfn "%s"

    //     let getGame (gameAgent: MailboxProcessor<AppMove>) = 
    //         gameAgent.PostAndReply GetGame 


    // TODO: implement the game loop here, so that a human can play in the terminal. 


    //     0
        
    | _ -> 
        printfn "Bad option. Read the code for help :)"
    0 // return an integer exit code

