module MctsSpiderGameLoop 
open System
open System.Collections.Generic
open SpiderSolitare
open System.Diagnostics
open SpiderSolitare.MonteCarloTreeSearch
open SpiderSolitare.Game
open SpiderSolitare.Visulization

let playMoves initGame moves =
    moves
    |> Array.fold (fun game move ->
        match GameMover.playMove move game with
        | Continue (g,_) -> g
        | Lost _ ->
            printfn "%A" game
            printfn "%A" move
            failwithf "bad move"
        | Won g -> g ) initGame

let playRandomMove move (node: MutableNode<Game, MoveType>) = 

    let gameResult = Game.GameMover.playMove move node.Game
    let prob = 0.5 // default init value since the network did not predict this move
    match gameResult with 
    | Game.Continue (g,_s) -> 
        MutableNode(g, prob, reward g, Some move, node.Depth + 1, Some node, None, 0)
    | Game.Lost g -> 
        MutableNode(g, prob, reward g, Some move, node.Depth + 1, Some node, Some false, 0)
    | Game.Won g -> 
        MutableNode(g, prob, reward g, Some move, node.Depth + 1, Some node, Some true, 0)

let r = Random()
let playMove log randomness getMetrics (root: MutableNode<'a, 'b>) t n  = 

    if root.Children.IsEmpty then 
        // This means that the NN suggested a move that had already been played ie a loop. 
        // It should be considered a loss since looping should be penalized. 
        Error None
    else 
        let v = r.NextDouble()
        if v < randomness then
            if log then  printfn "Playing best move"
            let nextNode = root.Children |> List.maxBy (getMetrics >> snd) 
            // match nextNode.Children with 
            // | [] when nextNode.TerminalValue = None -> nextNode |> Some |> Error
            //  | _ -> 
            Ok nextNode
        else                 
            let move = r.Next(0, root.Children.Length)
            if log then printfn "Playing random move. %d/%d" move (root.Children.Length)
            let nextNode = root.Children.[move]
            // match nextNode.Children with 
            // | [] when nextNode.TerminalValue = None -> nextNode |> Some |> Error
            // | _ -> 
            Ok nextNode

type GameResult = {
    IsWin: bool
    GameNumber: int
    Game: Game
    MovesMade: float
    History: (int * string * int * string) list
    Progress: float list
}

let playGame log randomMoveThreshold (searcher: ISearcher) updateHistory iterationCount totalCount gameNumber: GameResult = 

    let r = Random(gameNumber)
    let deck = Game.CardModule.deck Game.OneSuit //|> List.take (13 * 2)
    let game = Game.GameMover.createValidGame deck r |> Game.GameMover.unHideGame
    
    // let pastGames = Dictionary<_, _>()
    // let searcher = Searcher(log, brainsMover, pastGames, gameToMetrics)
    let root = MutableNode(game, 1.0, 0., None, 1, None, None, 0)

    searcher.Init root

    let rec loop foundWinningPath history count root game =
        let (t,n) = searcher.GetMetrics root
        
        let foundWinningPath =
            if foundWinningPath then true
            else
                // searcher.SearchWithNN(iterationCount, root)
                searcher.Search iterationCount root
                // false

        let movesMade = float (totalCount - count)
        let r = randomMoveThreshold
        match playMove log r searcher.GetMetrics root t n with 
        | Error _ ->   
            // we don't actually know why we are here. Some how we coudln't see that this was a dead state
            // Most likely this is the cause of a loop - we have already visited this state. 
            printfn "Finishing without a good reason."
            // false, gameNumber, game, movesMade, history, searcher.GetProgress() // TODO: keep reviing this
            {
                IsWin = false
                GameNumber = gameNumber
                Game = game
                MovesMade = movesMade
                History = history
                Progress = searcher.GetProgress()
            }

        // | Error (Some nMove) -> 
        //     printfn "Finishing - next move leads to a non-terminal state with no moves"
        //     let history = updateHistory game nMove.Game nMove.Move.Value history
        //     false, gameNumber, game, movesMade, history, searcher.GetProgress() // TODO: keep reviing this

        | Ok nMove -> 
            nMove.Parent <- None
            match nMove.TerminalValue with 
            | None -> 
                if count <= 0 then 
                    let history = updateHistory game nMove.Game nMove.Move.Value history
                    {
                        IsWin = false
                        GameNumber = gameNumber
                        Game = nMove.Game
                        MovesMade = movesMade
                        History = history
                        Progress = searcher.GetProgress()
                    }
                else    
                    if log then 
                        if false then 
                            searcher.BrainServer |> Option.iter (fun brainsMover -> 
                                visulizeColumnPrediction brainsMover nMove )

                        printfn "%A" nMove.Game                        
                        printfn "%A" nMove.Move.Value

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
                        MovesMade = movesMade
                        History = history
                        Progress = searcher.GetProgress()
                    }
                else
                    {
                        IsWin = false
                        GameNumber = gameNumber
                        Game = nMove.Game
                        MovesMade = movesMade
                        History = history
                        Progress = searcher.GetProgress()
                    }

    loop false [] totalCount root game 

    // 198