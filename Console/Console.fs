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

type ISaver = 
    // abstract member SaveGameMoves: bool ->  int -> (int * string * int * string) list -> unit
    abstract member SaveGameMoves: bool ->  int -> (int * string * int) list -> unit
    abstract member Finish: unit -> unit
    abstract member Format: unit -> unit

type RunConfig = {
    MctsIterationCount: int 
    LoopCount: int
    MoveCount: int
    RandomMoveThreshold: float
    RandomDelta: float
}

let showMnist (imageData: byte array array) =

    use image = new MagickImage(MagickColor.FromRgb(byte 0, byte  0, byte  0), 28, 28)            

    image.Grayscale()

    image.GetPixels() |> Seq.iter (fun pixel -> 
        let b = imageData.[pixel.Y].[pixel.X]
        pixel.Set([| b |] ) )

    image.Write("/Users/willsam100/Desktop/image.png")

    // use image = new MagickImage(MagickColor.FromRgb(byte 255, byte  45, byte  0), 10, 6)
    
    // image.GetPixels() |> Seq.iter (fun x -> x.Set([| byte 42; byte 45; byte 0   |]) )

    // image.Write("/Users/willsam100/Desktop/image.png")
    
    let startInfo = ProcessStartInfo("open")
    startInfo.RedirectStandardInput <- true
    startInfo.RedirectStandardOutput <- true
    startInfo.UseShellExecute <- false
    startInfo.Arguments <- "/Users/willsam100/Desktop/image.png"
    startInfo.WindowStyle <- ProcessWindowStyle.Hidden
    Process.Start(startInfo) |> ignore

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

    // member this.WaitForTraingingToStart() = 

    //     while File.Exists messageFile do 
    //         System.Threading.Thread.Sleep (TimeSpan.FromSeconds 0.5)
    //     File.WriteAllText (messageFile,"\n")

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


let rec run (qlearner: QLearner) log (saver: ISaver) parallelCount config gameNumbers = 

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
        brain.StartServer()
        let brainsMover = BrainsMoverClient(port) :> IBransMover

        range 
        |> List.map (fun x -> 
            match MctsSpiderGameLoop.playGame log config.RandomMoveThreshold brainsMover updateHistory config.MctsIterationCount config.MoveCount x with 
            | gameResult -> 

                brainsMover.Flush()

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

    if not log then 
        qlearner.Learn()

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
    qlearner.StopTraining()
    saver.Finish()

    if config.LoopCount > 0 then 
        printfn "Running policy: %d" config.LoopCount
        let config = {config with RandomMoveThreshold = Math.Min(0.99, config.RandomMoveThreshold + config.RandomDelta)}
        run qlearner log (saver: ISaver) parallelCount {config with LoopCount = config.LoopCount - 1} gameNumbers

[<EntryPoint>]
let main argv =
    let policyRaw = "/Users/willsam100/Desktop/spider-policy-raw-old.csv"
    let valueRaw = "/Users/willsam100/Desktop/spider-value-raw.csv"
    let qLearning = "/Users/willsam100/Desktop/spider-policy-net-train.csv"
    let qLearningBk = "/Users/willsam100/Desktop/message.csv"
    let qlearner = QLearner(qLearning, qLearningBk)

    match argv with 
    | [| "format-policy" |] -> Reformat.readAndFormatPolicy policyRaw
    | [| "format-value" |] -> Reformat.readAndFormatValue valueRaw
    | [| "format-moves" |] -> Reformat.readAndFormatValidMoves policyRaw
    | [| "format-run" |] -> Reformat.readAndFormatRun valueRaw
    | [| "format-all" |] ->  
        Reformat.readAndFormatPolicy policyRaw
        Reformat.readAndFormatValue valueRaw

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
            MctsIterationCount = 50
            MoveCount = 50
            LoopCount = 0
            RandomMoveThreshold = 1.0
            RandomDelta = 0.
        }

        let saver = 
            { new ISaver with 
                member ths.SaveGameMoves isWin gameNumber history = ()
                member this.Finish() = ()
                member this.Format() = () }

        run qlearner true saver parallelCount config gameNumbers


    | [| "generate" |] -> 

        Threading.ThreadPool.SetMinThreads(32, 32) |> ignore

        let gameNumbers = List.replicate 1 [ 1 .. 100000 ] |> List.concat
        let log = false
        let parallelCount = 4
        let config = {
            MctsIterationCount = 100
            LoopCount = 20
            MoveCount = 200
            RandomMoveThreshold = 0.9
            RandomDelta = 0.001
        }

        let saver = 
            let s = Saver(policyRaw, valueRaw, Some qLearning)
            { new ISaver with 
                member ths.SaveGameMoves isWin gameNumber history = s.SaveGameMoves isWin gameNumber history
                member this.Finish() = s.Finish()
                member this.Format() = s.Format() }
        
        run qlearner log saver parallelCount config gameNumbers

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


    | [| "mnist-play"; r |] -> 

        let mnistTrainingData = File.ReadAllBytes "/Users/willsam100/Downloads/train-images-idx3-ubyte"
        let imageData = parseIdxData "trainging data" mnistTrainingData 

        let r = Random(int r)

        let port = 5100

        let b = BrainMoverServer(port)
        b.StartServer()
        try 
            ()

            let client = BrainsMoverClient(port) :> IBransMover

            let image = 
                imageData
                |> Array.skip (r.Next(0, 10000))
                |> Array.head

            showMnist image

            let image =
                image
                |> Array.concat
                |> Array.map (int >> string)
                |> String.concat ","

            printfn "Image:\n%s" image

            let cnnView = client.GetMnist image

            // select height row
            // then select column
            // then select which image

            cnnView.ProbsOne
            |> List.splitInto cnnView.ProbsOneW
            |> List.map (List.splitInto cnnView.ProbsOneH)
            |> Visulization.createMnistImage "one" cnnView.ProbsOneC cnnView.ProbsOneH cnnView.ProbsOneW

            cnnView.ProbsTwo
            |> List.splitInto cnnView.ProbsTwoH
            |> List.map (List.splitInto cnnView.ProbsTwoH)
            |> Visulization.createMnistImage "two" cnnView.ProbsTwoC cnnView.ProbsTwoH cnnView.ProbsTwoH

            cnnView.ProbsThree
            |> List.splitInto cnnView.ProbsThreeH
            |> List.map (List.splitInto cnnView.ProbsThreeH)
            |> Visulization.createMnistImage "three" cnnView.ProbsThreeC cnnView.ProbsThreeH cnnView.ProbsThreeH

            cnnView.ProbsFour
            |> List.splitInto cnnView.ProbsFourH
            |> List.map (List.splitInto cnnView.ProbsFourH)
            |> Visulization.createMnistImage "five" cnnView.ProbsFourC cnnView.ProbsFourH cnnView.ProbsFourH

            cnnView.ProbsFive
            |> List.splitInto cnnView.ProbsFiveH
            |> List.map (List.splitInto cnnView.ProbsFiveH)
            |> Visulization.createMnistImage "fove" cnnView.ProbsFiveC cnnView.ProbsFiveH cnnView.ProbsFiveH            

        finally
            b.Stop()
        () 

    | [| "balance"; file |] ->  
        Reformat.reBalance file

    | [| "mnist"; |] -> 

        let parseIdxLabel name data = 
            let magic = parse 0 data
            let images = parse 4 data
            printfn "name:%s -> %d %d" name magic images

            data |> Array.skip 8

        let mnistTrainingData = File.ReadAllBytes "/Users/willsam100/Downloads/train-images-idx3-ubyte"
        let mnistLabelData = File.ReadAllBytes "/Users/willsam100/Downloads/train-labels-idx1-ubyte"

        let imageData = parseIdxData "trainging data" mnistTrainingData 
        let labelData = parseIdxLabel "label data" mnistLabelData

        printfn "%d %d" imageData.Length labelData.Length
        let data = Array.zip imageData labelData


        let outputFile = "/Users/willsam100/Desktop/mnist-value-net.csv"

        data 
        |> Array.take 42
        |> Array.map (fun (x,v) -> 

            let s = x |> Array.map (fun y -> string y.Length) |> String.concat ","
            printfn "%d: %s %d" x.Length s v

            let pixelData = x |> Array.concat |> Array.map (int >> string) |> String.concat ","

            sprintf "%s,%s"  pixelData (v |> int |> string)
        )
        |> fun xs -> File.WriteAllLines (outputFile, xs)


        // use image = new MagickImage(MagickColor.FromRgb(byte 0, byte  0, byte  0), 28, 28)            

        // image.Grayscale()

        // image.GetPixels() |> Seq.iter (fun pixel -> 
        //     let b = imageData.[pixel.Y].[pixel.X]
        //     pixel.Set([| b |] ) )

        // image.Write("/Users/willsam100/Desktop/image.png")

        // // use image = new MagickImage(MagickColor.FromRgb(byte 255, byte  45, byte  0), 10, 6)
        
        // // image.GetPixels() |> Seq.iter (fun x -> x.Set([| byte 42; byte 45; byte 0   |]) )

        // // image.Write("/Users/willsam100/Desktop/image.png")
        
        // let startInfo = ProcessStartInfo("open")
        // startInfo.RedirectStandardInput <- true
        // startInfo.RedirectStandardOutput <- true
        // startInfo.UseShellExecute <- false
        // startInfo.Arguments <- "/Users/willsam100/Desktop/image.png"
        // startInfo.WindowStyle <- ProcessWindowStyle.Hidden
        // Process.Start(startInfo) |> ignore
        
        
    | _ -> 
        printfn "Bad option. Read the code for help :)"
    0 // return an integer exit code

