module Console
open SpiderSolitare
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


[<EntryPoint>]
let main argv =

    let policyRaw = "/Users/willsam100/Desktop/spider-policy-raw.csv"
    let valueRaw = "/Users/willsam100/Desktop/spider-value-raw.csv"

    match argv with 
    | [| "format-policy" |] -> Reformat.readAndFormatPolicy policyRaw
    | [| "format-value" |] -> Reformat.readAndFormatValue valueRaw
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
    | _ -> 
    
        let moveEncoder = Representation.ActionEncoder()
        let mctsSearchIterationCount = 1000
        let moveCount = 100
        let loopCount = 10

        let log = false
        let gameNumbers = List.replicate 1 [ 0 .. 16] |> List.concat
        // let gameNumbers = [1]

        let saver = 

            if log then 
                { new ISaver with 
                    member ths.SaveGameMoves isWin gameNumber history = ()
                    member this.Finish() = ()
                    member this.Format() = () }
            else 
                let s = Saver(policyRaw, valueRaw)
                { new ISaver with 
                    member ths.SaveGameMoves isWin gameNumber history = s.SaveGameMoves isWin gameNumber history
                    member this.Finish() = s.Finish()
                    member this.Format() = s.Format() }


        let updateHistory game move history = 
            let gameAndBestMove = 
                    (List.length history + 1), 
                    (game |> Representation.encodeKeyGame MonteCarloTreeSearch.cardEncoder |> Seq.map string |> String.concat ","), 
                    (moveEncoder.Encode move |> Reformat.encodeMove)
            gameAndBestMove :: history

        let rec printTree depth (node: MonteCarloTreeSearch.Node) = 
            printfn "%s%f %d %A %d" depth node.T node.N node.Move (node.Game.GetHashCode())
            match node.Children with 
            | [] -> ()
            | xs -> 
                xs |> List.iter (printTree (depth + "  "))

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
            |> List.map (MctsSpiderGameLoop.playGame log search updateHistory mctsSearchIterationCount moveCount)
            |> List.map (function 
                | _, _, [] -> false
                | isWin, gameNumber, history -> 
                    // history |> List.map (fun x -> sprintf "%s,%b,%d" x isWin gameNumber) |> saver.SaveGameMoves)
                    saver.SaveGameMoves isWin gameNumber history 
                    brain.Stop()
                    isWin)
                

        let rec loop count = 

            let results = 
                gameNumbers
                |> List.splitInto 8
                |> List.mapi (fun i x -> 5100 + i * 2, x)
                |> List.map (fun (port, range) -> Task.Run(fun () ->  playGamesForRange port range) )
                |> List.toArray    
                |> Task.WhenAll    
                |> Async.AwaitTask
                |> Async.RunSynchronously
                |> Array.collect List.toArray

            let winningRate: float = (results |> Array.filter id |> Array.length |> float) / (List.length gameNumbers |> float)
            printfn "WINING RATE: %.2f" winningRate

            saver.Finish()
            saver.Format()

            if not log then 
                printfn "Training..."
                let train = Process.Start("/Users/willsam100/projects/plaidML/train.sh")
                train.WaitForExit()

            if count > 0 then 
                loop (count - 1)

        if log then 
            loop 0
        else         
            loop loopCount

    0 // return an integer exit code

