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
let playMove log randomness gameToMetrics (root: MutableNode<'a, 'b>) t n  = 

    if root.Children.IsEmpty then 
        // This means that the NN suggested a move that had already been played ie a loop. 
        // It should be considered a loss since looping should be penalized. 
        None
    else 
        let v = r.NextDouble()
        if v < randomness then
            if log then  printfn "Playing best move"
            root.Children |> List.maxBy (getMetrics gameToMetrics >> snd) |> Some
        else                 
            let move = r.Next(0, root.Children.Length)
            if log then printfn "Playing random move. %d/%d" move (root.Children.Length)
            root.Children.[move] |> Some

let playGame log randomMoveThreshold brainsMover updateHistory iterationCount totalCount gameNumber = 

    let r = Random(gameNumber)
    let gameToMetrics = new Dictionary<_, _>()
    let deck = Game.CardModule.deck Game.OneSuit |> List.take (13 * 3)
    let game = Game.GameMover.createValidGame deck r |> Game.GameMover.unHideGame
    
    let pastGames = Dictionary<_, _>()
    let searcher = Searcher(log, brainsMover, pastGames, gameToMetrics)
    let root = MutableNode(game, 1.0, 0., None, 1, None, None, 0)
    
    pastGames.[root.GameHashCode] <-Set.empty

    let rec loop foundWinningPath history count root game =
        let (t,n) = getMetrics gameToMetrics root
        
        let foundWinningPath =
            if foundWinningPath then true
            else
                searcher.SearchWithNN(iterationCount, root)
                // searcher.SearchRandom(iterationCount, root)
                // false

        let movesMade = float (totalCount - count)
        let r = randomMoveThreshold
        match playMove log r gameToMetrics root t n with 
        | None -> 
            false, gameNumber, game, movesMade, history, searcher.GetProgress() // TODO: keep reviing this

        | Some nMove -> 
            nMove.Parent <- None
            match nMove.TerminalValue with 
            | None -> 
                if count <= 0 then 
                    let history = updateHistory game nMove.Game nMove.Move.Value history
                    false, gameNumber, nMove.Game, movesMade, history, searcher.GetProgress()
                else    
                    if log then 
                        if false then 
                            visulizeColumnPrediction brainsMover nMove

                        printfn "%A" nMove.Game                        
                        printfn "%A" nMove.Move.Value

                        loop foundWinningPath (updateHistory game nMove.Game nMove.Move.Value history) (count - 1) nMove nMove.Game
                    else                         
                        loop foundWinningPath (updateHistory game nMove.Game nMove.Move.Value history) (count - 1) nMove nMove.Game                        

            | Some didWin -> 
//                let game = nMove.Game
                let history = updateHistory game nMove.Game nMove.Move.Value history
                if didWin then
                    true, gameNumber, nMove.Game, movesMade, history, searcher.GetProgress()
                else
                    false, gameNumber, nMove.Game, movesMade,  history, searcher.GetProgress()

    loop false [] totalCount root game 

    // 198