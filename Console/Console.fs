module Console
open SpiderSolitare
open System
open System.Threading.Tasks
open System.IO
open System.Runtime
open Persisance
open Brain
open SpiderSolitare.MonteCarloTreeSearch
open SpiderSolitare.Game
open System.Diagnostics

type ISaver = 
    abstract member SaveGameMoves: bool ->  int -> (int * string * int) list -> unit
    abstract member Finish: unit -> unit
    abstract member Format: unit -> unit

type RunConfig = {
    MctsIterationCount: int 
    LoopCount: int
    MoveCount: int
    RandomMoveThreshold: float
}

let run log (saver: ISaver) parallelCount config gameNumbers = 
    let moveEncoder = Representation.ActionEncoder()

    let updateHistory game move history = 
        let gameAndBestMove = 
            (List.length history + 1), 
            (game |> Representation.encodeKeyGame cardEncoder |> Seq.map string |> String.concat ","), 
            (moveEncoder.Encode move |> encodeMove)
        gameAndBestMove :: history

//    let rec printTree depth (node: MonteCarloTreeSearch.Node) = 
//        printfn "%s%f %d %A %d" depth node.T node.N node.Move (node.Game.GetHashCode())
//        match node.Children with 
//        | [] -> ()
//        | xs -> 
//            xs |> List.iter (printTree (depth + "  "))

    let getGames node =
        let rec loop acc (nodes: MonteCarloTreeSearch.MutableNode list) =  

            // let game = Representation.decodeKeyedGame MonteCarloTreeSearch.cardEncoder (node.Game.Split (',') |> Array.map Int32.Parse |> Array.toList)
            let children = nodes |> List.collect (fun x -> x.Children)
            loop (acc @ nodes) children

        loop [] [node]

    let playGamesForRange port range = 
        let brain = BrainServerProcess(port)
        brain.StartServer()
        let brainsMover = BrainsMover(port) :> IBransMover
        let search = MonteCarloTreeSearch.search log brainsMover

        range 
        |> List.map (fun x -> 
            match MctsSpiderGameLoop.playGame log config.RandomMoveThreshold search updateHistory config.MctsIterationCount config.MoveCount x with 
            | isWin, gameNumber, game, movesMade, history -> 

                brainsMover.Flush()

                printfn "%A" game
                printfn "GameNumber: %d, Result: %s, MovesPlayed: %.0f\n" gameNumber (if isWin then "WIN" else "LOST") movesMade

                // history |> List.map (fun x -> sprintf "%s,%b,%d" x isWin gameNumber) |> saver.SaveGameMoves)
                if history |> List.isEmpty |> not then 
                    saver.SaveGameMoves isWin gameNumber history 
                
                gameNumber, isWin )
        |> fun xs -> 
            brain.Stop()
            xs

    let results = 
        gameNumbers
        |> List.splitInto parallelCount
        |> List.mapi (fun i x -> 5100 + i * 2, x)
        |> List.map (fun (port, range) -> Task.Run(fun () ->  playGamesForRange port range) )
        |> List.toArray    
        |> Task.WhenAll    
        |> Async.AwaitTask
        |> Async.RunSynchronously
        |> Array.collect List.toArray

    let winningRate: float = (results |> Array.filter snd |> Array.length |> float) / (List.length gameNumbers |> float)
    printfn "WINING RATE: %.2f" winningRate
    printfn "Games lost: %s" (results |> Array.filter (snd >> not) |> Array.map (fst >> string) |> String.concat ",")

    saver.Finish()
    saver.Format()


[<EntryPoint>]
let main argv =
    let policyRaw = "/Users/willsam100/Desktop/spider-policy-raw.csv"
    let valueRaw = "/Users/willsam100/Desktop/spider-value-raw.csv"


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

        let s = Saver(policyRaw, valueRaw)
    
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
            MctsIterationCount = 2000
            MoveCount = 5000
            LoopCount = 0
            RandomMoveThreshold = 1.0
        }

        let saver = 
            { new ISaver with 
                member ths.SaveGameMoves isWin gameNumber history = ()
                member this.Finish() = ()
                member this.Format() = () }

        run true saver parallelCount config gameNumbers


    | [| "generate" |] -> 

        Threading.ThreadPool.SetMinThreads(32, 32) |> ignore

        let gameNumbers = List.replicate 20 [ 0 ] |> List.concat
        let log = false
        let parallelCount = 8
        let config = {
            MctsIterationCount = 100
            LoopCount = 0
            MoveCount = 200
            RandomMoveThreshold = 0.8
        }

        let saver = 
            let s = Saver(policyRaw, valueRaw)
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
            | x -> failwithf "Invalid network to train: %s" x

        printfn "Training..."
        let train = new Process()
        train.StartInfo.FileName <- file
        train.StartInfo.Arguments <- args
        train.Start() |> ignore
        train.WaitForExit()

        ()

    | _ -> 
        printfn "Bad option. Read the code for help :)"
    0 // return an integer exit code

