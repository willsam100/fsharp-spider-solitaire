module MctsSpiderGameLoop 
open System
open System.Collections.Generic
open SpiderSolitare
open System.Diagnostics
open SpiderSolitare.MonteCarloTreeSearch
open SpiderSolitare.Game

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

let playRandomMove move (node: MutableNode<'a, 'b>) = 

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
    let deck = Game.CardModule.deck Game.OneSuit |> List.take 13
    let game = Game.GameMover.createValidGame deck r |> Game.GameMover.unHideGame
    
    let pastGames = Dictionary<_, _>()
    let searcher = Searcher(log, brainsMover, pastGames, gameToMetrics)
    
//    let moves =
//        [|
//            { From = C1; To = C4; Card = Card (12, S) }
//            { From = C1; To = C2; Card = Card (2, S) }
//            { From = C1; To = C2; Card = Card (1, S) }
//            { From = C1; To = C5; Card = Card (13, S) }
//            { From = C2; To = C6; Card = Card (3, S) }
//            
//            { From = C2; To = C1; Card = Card (9, S) }
//            { From = C1; To = C2; Card = Card (10, S) }
//            { From = C2; To = C4; Card = Card (11, S) }
//            { From = C4; To = C5; Card = Card (12, S) }
//            
//            { From = C2; To = C3; Card = Card (6, S) }
//        |] |> Array.map MoveType.Move
        
//    let game = moves |> playMoves game
    let root = MutableNode(game, 1.0, 0., None, 1, None, None, 0)
    
    pastGames.[root.GameHashCode] <-Set.empty
    // let s = Stopwatch()

    let rec loop history count root game =
        let (t,n) = getMetrics gameToMetrics root
        // printfn "GN:%d, pg:%d" gameNumber count
        
        // s.Restart()
//        mctsSearch pastGames gameToMetrics iterationCount root
        let foundWinningPath = searcher.SearchWithNN(iterationCount, root)

        let movesMade = float (totalCount - count)
        let r = randomMoveThreshold
        match playMove log r gameToMetrics root t n with 
        | None -> 
            false, gameNumber, game, movesMade, history // TODO: keep reviing this

        | Some nMove -> 
            nMove.Parent <- None
            match nMove.TerminalValue with 
            | None -> 
                if count <= 0 then 
                    let history = updateHistory game nMove.Move.Value history
                    false, gameNumber, nMove.Game, movesMade, history
                else    
                    if log then 
                        printfn "%A" nMove.Game
                        printfn "Move: %A" nMove.Move
                        printfn "N: %f.0 "(getMetrics gameToMetrics nMove |> snd)
  
                    loop (updateHistory game nMove.Move.Value history) (count - 1) nMove nMove.Game

            | Some didWin -> 
                let game = nMove.Game
                let history = updateHistory game nMove.Move.Value history
                if didWin then
                    true, gameNumber, game, movesMade, history
                else
                    false, gameNumber, nMove.Game, movesMade,  history


    loop [] totalCount root game 

    // 198