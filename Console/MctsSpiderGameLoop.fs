module MctsSpiderGameLoop 
open System
open System.Collections.Generic
open SpiderSolitare
open System.Diagnostics
open SpiderSolitare.MonteCarloTreeSearch
open SpiderSolitare.Game
open SpiderSolitare.Visulization
open SpiderSolitare.Brain

let r = Random()

let pickBest log getMetrics (root: MutableNode<Game, 'b>) = 
    if root.Children.IsEmpty then 
        // This means that the NN suggested a move that had already been played ie a loop. 
        // It should be considered a loss since looping should be penalized. 
        None
    else 
        if log then  printfn "Playing best move"
        root.Children |> List.maxBy (getMetrics >> fst) |> Some

let rec pickRandom log count (root: MutableNode<Game, 'b>) = 

    if root.Children.IsEmpty then 
        // This means that the NN suggested a move that had already been played ie a loop. 
        // It should be considered a loss since looping should be penalized. 
        None
    else                
        if count < 0 then 
            let move = r.Next(0, root.Children.Length)
            if log then printfn "Playing UNEXPLORED random move. %d/%d" (move + 1) (root.Children.Length)
            root.Children.[move] |> Some
        else 
            let move = r.Next(0, root.Children.Length)
            let nextNode = root.Children.[move]
            match nextNode.Children, nextNode.TerminalValue with 
            | [],  None -> pickRandom log (count - 1) root
            | _ -> 
                if log then printfn "Playing random move. %d/%d" (move + 1) (root.Children.Length)
                Some nextNode

let updateHistory parentGame game move history = 
    let gameAndBestMove = 
        (List.length history + 1), 
        (parentGame  |> Representation.encodeKeyGame 26 cardEncoder |> Seq.map string |> String.concat ","), 
        (moveEncoder.Encode move |> encodeMove),
        (game |> Representation.encodeKeyGame 26 cardEncoder |> Seq.map string |> String.concat ",")
    gameAndBestMove :: history

type GameResult = {
    IsWin: bool
    GameNumber: int
    Game: Game
    MovesMade: float
    History: (int * string * int * string) list
    Progress: float list
}

let playGame log randomMoveThreshold (searcher: ISearcher) iterationCount totalCount game gameNumber: GameResult = 

    let root = MutableNode(game, 1.0, 0., None, 1, None, None, 0)
    let watch = Stopwatch()
    watch.Start()
    searcher.Init root
    if log then 
        printfn "Starting game:"
        printfn "%A" game
        printfn ""
    

    let rec loop foundWinningPath history count (root: MutableNode<Game, MoveType>) game =
        let fixedLookAhead = Math.Min(300, totalCount - (totalCount - count)) |> float

        let shouldIgnoreRandom =
            root.Game.Spades = Zero || root.Game.Spades = One || root.Game.Spades = Two || root.Game.Spades = Three || root.Game.Spades = Four

        let playedRandom, playMove, foundWinningPath = 
            match foundWinningPath, shouldIgnoreRandom, r.NextDouble() < randomMoveThreshold with
            | true, _, true -> false, pickBest log searcher.GetMetrics, true
            | _, _, true
            | _, true, false -> 
                false, pickBest log searcher.GetMetrics, searcher.Search iterationCount fixedLookAhead root
            | _, _, _ -> 
                // let randomLookAhead = r.Next(1, 5) |> float
                true, pickRandom log 5, false

        if playedRandom then 
            searcher.ResetMaxReward()
            watch.Restart()

        match playMove root with 
        | None ->   
            // we don't actually know why we are here. Some how we coudln't see that this was a dead state
            // Most likely this is the cause of a loop - we have already visited this state. 
            printfn "Finishing without a good reason. %A" playedRandom
            // false, gameNumber, game, movesMade, history, searcher.GetProgress() // TODO: keep reviing this
            searcher.PrintTree 4 root

            {
                IsWin = false
                GameNumber = gameNumber
                Game = game
                MovesMade = float (totalCount - count)
                History = history
                Progress = searcher.GetProgress()
            }

        // | Error (Some nMove) -> 
        //     printfn "Finishing - next move leads to a non-terminal state with no moves"
        //     let history = updateHistory game nMove.Game nMove.Move.Value history
        //     false, gameNumber, game, movesMade, history, searcher.GetProgress() // TODO: keep reviing this

        | Some nMove -> 
            nMove.Parent <- None
            match nMove.TerminalValue with 
            | None -> 
                if count <= 0 then 
                    let history = updateHistory game nMove.Game nMove.Move.Value history
                    {
                        IsWin = false
                        GameNumber = gameNumber
                        Game = nMove.Game
                        MovesMade = float (totalCount - count)
                        History = history
                        Progress = searcher.GetProgress()
                    }
                else    
                    if log then 
                        // if false then 
                        //     searcher.BrainServer |> Option.iter (fun brainsMover -> 
                        //         visulizeColumnPrediction brainsMover nMove )

                        printfn "%A" nMove.Game                        
                        printfn "%A" nMove.Move.Value

                        let foundWinningPath = 
                            if watch.Elapsed < searcher.MaxTime then foundWinningPath
                            else 
                                printfn "Out of time searching"
                                true // Play the best we can after searching

                        loop foundWinningPath (updateHistory game nMove.Game nMove.Move.Value history) (count - 1) nMove nMove.Game
                    else                         
                        loop foundWinningPath (updateHistory game nMove.Game nMove.Move.Value history) (count - 1) nMove nMove.Game                        

            | Some didWin -> 
//                let game = nMove.Game
                let history = updateHistory game nMove.Game nMove.Move.Value history
                if didWin then
                    {
                        IsWin = true
                        GameNumber = gameNumber
                        Game = nMove.Game
                        MovesMade = float (totalCount - count)
                        History = history
                        Progress = searcher.GetProgress()
                    }
                else
                    {
                        IsWin = false
                        GameNumber = gameNumber
                        Game = nMove.Game
                        MovesMade = float (totalCount - count)
                        History = history
                        Progress = searcher.GetProgress()
                    }

    loop false [] totalCount root game 

    // 198