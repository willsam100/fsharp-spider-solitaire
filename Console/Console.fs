module Console
open SpiderSolitare
open System
open System.Threading.Tasks
open System.IO
open System.Runtime
open Persisance
open Brain
open ImageMagick
open SpiderSolitare.GameString
open SpiderSolitare.MonteCarloTreeSearch
open SpiderSolitare.Game
open System.Diagnostics
open SpiderSolitare.Operations
open SpiderSolitare.Operations.App
open System.Threading

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
    MaxSearchTime:TimeSpan
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

    let endPort = if log then 5106 else 5100 + parallelCount - 1
    let ports = [5100 .. endPort]
    let brains = 
        ports
        |> List.map (fun port -> 
            async {
                do! Async.SwitchToThreadPool()
            
                let brain = BrainMoverServer(port)
                brain.StartServer()
                return brain
            
            })
        |> Async.Parallel
        |> Async.RunSynchronously
        |> Array.toList

    // wait for the python flask server to start up. 
    Thread.Sleep (TimeSpan.FromSeconds 15.)

    let playGamesForRange port gameNumbers = 
        let ports = 
            if parallelCount = 1 then ports
            else [port]
        let brainsMover = BrainsMoverClient(ports) :> IBrainsMover

        printfn "Port: %d playing %d games" port (List.length gameNumbers)

        let searcher = 
            match config.MachineLearningApproach with 
            | MctsWithRestApi -> SearcherWithNeuralNetwork(brainsMover, log, config.MaxSearchTime) :> ISearcher
            | RadnomMcts -> SearcherRandomer(log)  :> ISearcher

        gameNumbers 
        |> List.map (fun gameNumber -> 

            let r = Random(gameNumber)
            let deck = Game.CardModule.deck Game.OneSuit //|> List.take (13)
            let game = Game.GameMover.createValidGame deck r |> Game.GameMover.unHideGame

            printfn "Port: %d playing game %d" port gameNumber

            let s = Stopwatch()
            s.Start()
            let gameResult = MctsSpiderGameLoop.playGame log config.RandomMoveThreshold searcher config.MctsIterationCount config.MoveCount game gameNumber
            searcher.BrainServer |> Option.iter (fun brainsMover -> brainsMover.Flush())

            printfn "%A" gameResult.Game
            printfn "GameNumber: %d, Result: %s, MovesPlayed: %.0f Time: %A" gameResult.GameNumber (if gameResult.IsWin  then "WIN" else "LOST") gameResult.MovesMade s.Elapsed
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
            searcher.BrainServer |> Option.iter (fun brainsMover -> brainsMover.Flush())
            xs


    let results =    
        gameNumbers
        |> List.toArray
        |> fun x -> 
            if not log then Array.shuffle x; x
            else x
        |> Array.splitInto (parallelCount)
        |> Array.mapi (fun i x -> 5100 + i, x)
        |> Array.map (fun (port, range) -> 
            let t = new Task<(int * bool) list>((fun () ->  playGamesForRange port (List.ofArray range)), TaskCreationOptions.LongRunning)
            t.Start()
            t|> Async.AwaitTask ) 
        |> Async.Parallel
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

    brains |> List.iter (fun brain -> brain.Stop())

    if config.LoopCount > 0 then 
        printfn "Running policy: %d" config.LoopCount
        let config = {config with RandomMoveThreshold = Math.Min(0.99, config.RandomMoveThreshold + config.RandomDelta)}
        run log (saver: ISaver) parallelCount {config with LoopCount = config.LoopCount - 1} gameNumbers

[<EntryPoint>]
let main argv =
    let prefix = "/Users/willsam100/Desktop/spider-data/"
    let prefixName = "spider-policy-raw"
    let policyRaw: int -> string = sprintf "%s/%s-%d.csv" prefix prefixName
    let prefixValueName = "spider-value-raw"
    let valueRaw: int -> string = sprintf "%s/%s-%d.csv" prefix prefixValueName
    // let valueRaw = "/Users/willsam100/Desktop/spider-value-raw.csv"
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

    // | [| "format-legacy" |] ->  

    //     GCSettings.LatencyMode <- GCLatencyMode.Batch
    //     if File.Exists policyRaw then 
    //         File.Delete (policyRaw)

    //     if File.Exists valueRaw then 
    //         File.Delete (valueRaw)

    //     let s = Saver(policyRaw, valueRaw, None)
    
    //     "/Users/willsam100/Desktop/spider-game-with-failure.csv"
    //     |> File.ReadAllLines
    //     |> Array.mapi Reformat.sequenceDataLegacy
    //     |> Array.groupBy (fun x -> x.GameNumber)
    //     |> Array.iter (fun (gn, xs) -> s.SaveLegacyFormat gn xs )

    //     s.Finish()
    | [| "play"; movesAhead; gameNumbers|] -> 
        let gameNumbers = 
            if gameNumbers.Contains "," then 
                gameNumbers.Split "," |> Array.toList |> List.map System.Int32.Parse
            else System.Int32.Parse gameNumbers |> List.singleton

        let parallelCount = 1
        let config = {
            MctsIterationCount = int movesAhead
            MoveCount = 300
            LoopCount = 0
            RandomMoveThreshold = 1.0
            RandomDelta = 0.
            MachineLearningApproach = MachineLearningApproach.MctsWithRestApi
            MaxSearchTime = TimeSpan.FromHours 1.
        }

        let saver = 
            { new ISaver with 
                member ths.SaveGameMoves isWin gameNumber history = ()
                member this.Finish() = ()
                member this.Format() = () }

        let s = Stopwatch()
        s.Start()
        run true saver parallelCount config gameNumbers
        printfn "Total Time Taken: %A" s.Elapsed

    | [| "flip"; log |] -> 
        // Tries to solve the moves to get to the hidden cards. 
        // This is used to translate games from another 'real' spider game into this game.

        let log = 
            match log with 
            | "true" -> true
            | "log" -> true
            | _ -> false

        let input = 
            """ 
            
                -  -  -   -  -   s7
                -  -  s10 -  -   s3  
                -  s7 sa  -  s7  sa -  sk -  sa         
                sk s3 s3  sa s5  sq s3 s3 -  s10            
                s6 sj s4  s6 s10 s8 sa s7 sk sq
                sq s9 s7  s4                    """
              // 1  2  3  4  5  6  7  8  9  10
              // 2  3  3  2  2  4  1  2  0  2
        let deck = 
            """
                sk s10 s9 sj  s3 s2  s5 s13 s5 s2
                sk sa  s5 s10 s2 sk  s4 s8  s9 s9
                sj sq  s8 sj  s4 s8  sq s6  s7 s8
                s3 s8  s4 s6  s5 s6  s9 sj  sq s4
                sa sj  s3 s2  sa s10 s7 s5  s9 sj
            """ 

        let rows = 
            GameString.toRows input
            |> List.mapi (fun i x -> Coord.parseColumn (i + 1) |> string, x |> List.tail |> List.length)

        let game = GameString.parseGame OneSuit (input, deck)
   
        printfn "%A" game
        printfn "%A" game.Stock

        let config = {
            MctsIterationCount = 20
            MoveCount = 1000
            LoopCount = 0
            RandomMoveThreshold = 0.9
            RandomDelta = 0.
            MachineLearningApproach = MachineLearningApproach.MctsWithRestApi
            MaxSearchTime = TimeSpan.FromMinutes 5.
        }

        let searcher = SearcherRandomer(true)  :> ISearcher

        let appendMove x acc = 
            match acc with 
            | [] -> [x] :: acc 
            | head::xs -> (x :: head) :: xs

        let printMoves (rows: (string * int) list) history = 

            let moves = 
                history 
                |> List.rev
                |> List.map (fun (_, move, _, _) -> move)

            let flipCount = 
                moves
                |> List.fold (fun (i, flipCount) (x: string) -> 
                    if x.ToLower().Contains "flip" then 
                        let flipCount =
                            flipCount
                            |> Map.tryFind x
                            |> Option.map (fun xs -> 
                                match xs with 
                                | [] -> flipCount |> Map.add x [(i, 1)]
                                | (_, count)::_ as xs -> flipCount |> Map.add x ((i, count + 1) :: xs) )
                            |> Option.defaultValue (flipCount |> Map.add x [(i, 1)])
                        i + 1, flipCount
                    else 
                        i+1, flipCount
                ) (0, Map.empty)
                |> snd

            let shouldPrint = 
                flipCount
                |> Map.exists (fun k v -> 
                    match rows |> List.tryFind (fun (c, _) -> k.Contains c) with 
                    | None -> false
                    | Some (_, count) -> v |> List.map snd |> List.max > count
                )

            moves
            |> List.iteri (fun i x -> 
                let move = 
                    flipCount 
                    |> Map.tryFind x 
                    |> Option.bind (fun xs -> 
                        xs 
                        |> List.tryFind (fun (index, count) -> index = i)
                        |> Option.bind (fun (_, count) ->  sprintf "%s %d" x count |> Some ))
                    |> Option.defaultValue x
                printfn "%s" move )

            if not shouldPrint then 
                printfn "No Flip cards found"


            flipCount
            |> Map.iter (fun k v -> 
                printfn "%s:%d" k (v |> List.map snd |> List.max)
            )

        let updateHistory pastGame currentGame move history = 
            let next = 1, string move, 1, ""
            next :: history
            
        MctsSpiderGameLoop.playGame log config.RandomMoveThreshold searcher config.MctsIterationCount config.MoveCount game -1
        |> fun x -> x.History
        |> printMoves rows

        ()


    | [| "generate" |] -> 

        // Threading.ThreadPool.SetMinThreads(32, 32) |> ignore

        let gameNumbers = 
            List.replicate 10 [50 .. 120] 
            |> List.concat //List.replicate 20 [ 50;51;52;55;57;59;60;61;62;63;64;65;66;67;68;69;70;76;77;79;80;81;82;83;84;85;86;87;88;89;90;91;92;96;97;98;99;100;101;103;104;105;107;108;110;111;113;114;115;116;118;119;120;121;122;123;124;126;127;128;129;130;131;132;134;135;136;137;138;139;141;142;143;144;146;148;149;150;152;153;154;156;157;158;159;160;161;162;163;164;165;166;167;169;170;172;174;175;176;177;178;180;181;182;183;184;185;186;188;189;190;191;192;193;194;195;196;197;200 ] |> List.concat
            |> List.filter (fun x -> x <> 100 && x <> 102 && x <> 103 && x <> 116
            )
        let parallelCount = 8
        let log = if parallelCount > 1 then false else true
        let config = {
            MctsIterationCount = 100
            LoopCount = 0
            MoveCount = 300
            RandomMoveThreshold = 0.98
            RandomDelta = 0.001
            MachineLearningApproach = MctsWithRestApi
            MaxSearchTime = TimeSpan.FromHours 1.
        }

        let saver = 
            let s = Saver(policyRaw, valueRaw, None)
            { new ISaver with 
                member ths.SaveGameMoves isWin gameNumber history = s.SaveGameMoves isWin gameNumber history
                member this.Finish() = s.Finish()
                member this.Format() = s.Format() }
        
        let s = Stopwatch()
        s.Start()
        run log saver parallelCount config gameNumbers
        printfn "Time Taken: %A" s.Elapsed

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

    | [| "balance"; |] ->  

        Directory.GetFiles prefix 
        |> Array.filter (fun x -> x.Contains prefixName)
        |> Array.map (fun x -> 
            x.Replace(prefix, "").Replace(prefixName, "").Replace("-", "").Replace(".csv", ""))
        |> Array.map int
        |> Array.filter (fun x -> x <= 100)
        |> Array.sort
        |> Array.iter (fun gameNumber -> 
            printfn "Balancing %d" gameNumber
            Reformat.augment gameNumber prefix (gameNumber |> policyRaw)
        )

    | [| "balance-cat"; |] ->      

        let destFileName = Path.Join(prefix, "data-value-1.csv") 

        let sourceFiles = 
            Directory.GetFiles prefix 
            |> Array.filter (fun x -> x.Contains "data-value-")
            // |> Array.filter (fun x -> x.Contains "-1.csv" |> not)

        let catFile sourceFiles =     
            use destStream = File.OpenWrite(destFileName)
            for srcFileName in sourceFiles do
                use srcStream = File.OpenRead(srcFileName)
                srcStream.CopyTo(destStream)
        catFile sourceFiles

        for f in sourceFiles do
            File.Delete f


        // /Users/willsam100/projects/spider-ml/datavalue/data-value-1.csv
        File.Delete "/Users/willsam100/projects/spider-ml/datavalue/data-value-1.csv"
        File.Move(destFileName, "/Users/willsam100/projects/spider-ml/datavalue/data-value-1.csv")

        // zip -r data.zip data/

    | [| "balance-value"; |] ->  

        Directory.GetFiles prefix 
        |> Array.filter (fun x -> x.Contains prefixValueName)
        |> Array.map (fun x -> x.Replace(prefix, "").Replace(prefixValueName, "").Replace("-", "").Replace(".csv", ""))
        |> Array.map int
        |> Array.sort
        // |> Array.filter (fun x -> x <= 100)
        |> Array.iter (fun gameNumber -> 
            Reformat.augmentValue gameNumber prefix (gameNumber |> valueRaw)
        )

    | [| "play-as-human"; gameNumber |] -> 

        let rand = new Random(int gameNumber)
        let gameStateAgent = App.myAgent rand
        
        App.start gameStateAgent

        let rec gameLoop () = 
            match gameStateAgent |> getGame with 
            | Lost _ | Won _ -> 
                gameStateAgent |> getGame |> printGameResult |> printfn "%s"
                printfn "Completed"
            | _ -> 
                printf "Enter move index:"
                let move = Console.ReadLine() |> int
                App.playAndPrint gameStateAgent move
                gameLoop()

        gameLoop()

    | _ -> 
        printfn "Bad option. Read the code for help :)"

    0 // return an integer exit code

