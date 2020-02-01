module Console
open SpiderSolitare
open System.Threading.Tasks
open Persisance
open Brain
open SpiderSolitare.MonteCarloTreeSearch
open SpiderSolitare.Game
open System.Diagnostics

type ISaver = 
    abstract member SaveGameMoves: bool ->  int -> (int * string * string) list -> unit
    abstract member Finish: unit -> unit
    abstract member Format: unit -> unit


[<EntryPoint>]
let main argv =

    match argv with 
    | [| "format" |] ->  Script.run()
    | _ -> 
    
        let moveEncoder = Representation.ActionEncoder()
        let mctsSearchIterationCount = 100
        let moveCount = 100
        let loopCount = 10

        let log = true
        let gameNumbers = List.replicate 2000 [ 2;] |> List.concat
        let gameNumbers = [202]

        let saver = 

            if log then 
                { new ISaver with 
                    member ths.SaveGameMoves isWin gameNumber history = ()
                    member this.Finish() = ()
                    member this.Format() = () }
            else 
                let s = Saver "/Users/willsam100/Desktop/spider-game-with-failure.csv" 
                { new ISaver with 
                    member ths.SaveGameMoves isWin gameNumber  history = s.SaveGameMoves isWin gameNumber history
                    member this.Finish() = s.Finish()
                    member this.Format() = s.Format() }


        let updateHistory game move history = 
            let gameAndBestMove = 
                    (List.length history + 1) , 
                    (game |> Representation.encodeKeyGame MonteCarloTreeSearch.cardEncoder |> Seq.map string |> String.concat ","), 
                    (moveEncoder.Encode move |> Array.map string |> String.concat ",")
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

            try 
                range 
                |> List.map (MctsSpiderGameLoop.playGame log search updateHistory mctsSearchIterationCount moveCount)
                |> List.iter (function 
                    | _, _, [] -> ()
                    | isWin, gameNumber, history -> 
                        history |> List.map (fun x -> sprintf "%s,%b,%d" x isWin gameNumber) |> saver.SaveGameMoves)

            finally 
                brain.Stop()

        let rec loop count = 

            gameNumbers
            |> List.splitInto 8
            |> List.mapi (fun i x -> 5100 + i * 2, x)
            |> List.map (fun (port, range) -> Task.Run(fun () ->  playGamesForRange port range) )
            |> List.toArray    
            |> Task.WhenAll    
            |> Async.AwaitTask
            |> Async.RunSynchronously

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

