module MctsSpiderGameLoop 
open System
open System.Collections.Generic
open SpiderSolitare
open System.Diagnostics
open SpiderSolitare.MonteCarloTreeSearch

let playRandomMove move (node: MutableNode) = 

    let gameResult = Game.GameMover.playMove move node.Game
    let prob = 0.5 // default init value since the network did not predict this move
    match gameResult with 
    | Game.Continue (g,_s) -> 
        MutableNode(g, prob, reward g, Some move, node.Depth + 1, Some node, None)
    | Game.Lost g -> 
        MutableNode(g, prob, reward g, Some move, node.Depth + 1, Some node, Some 0.)
    | Game.Won g -> 
        MutableNode(g, prob, reward g, Some move, node.Depth + 1, Some node, Some 1.)


let playMove log randomness gameToMetrics (root: MutableNode) t n  = 

    if root.Children.IsEmpty then 
        // This means that the NN suggested a move that had already been played ie a loop. 
        // It should be considered a loss since looping should be penalized. 
        None
    else 
            let r = Random()
        // if r.NextDouble() < randomness || log then
        //     root.Children |> List.maxBy (fun x -> getMetrics gameToMetrics x |> snd ) |> Some
        // else                 
            let move = r.Next(0, root.Children.Length)
            root.Children.[move] |> Some

let playGame log mctsSearch updateHistory iterationCount totalCount gameNumber = 

    let r = Random(gameNumber)
    let gameToMetrics = new Dictionary<_, _>()
    let deck = Game.CardModule.deck Game.OneSuit
    let game = Game.GameMover.createValidGame deck r |> Game.GameMover.unHideGame
    let root = MonteCarloTreeSearch.MutableNode(game, 1.0, 0., None, 1, None, None)
    let pastGames = Dictionary<_, _>()
    let randomness = 0.8
    let randomIncrement = 0.00
    if not log then 
        printfn "Random moves will be played! Good move will be played %.2f%%" randomness

    
    pastGames.[root.GameHashCode] <- Set.empty
    // let s = Stopwatch()

    let rec loop history count root game = 
        let (t,n) = getMetrics gameToMetrics root
        // let iterationCount = mctsSearchIterationCount - progress count + (if n = -1. ||  t / (float n) <=  -1.00 then 10000 else 5)
        // printfn "Iteration Count: %d - %d" iterationCount count
        
        // s.Restart()
        mctsSearch pastGames gameToMetrics iterationCount root
        // s.Stop()
        // printfn ""
        // printfn "C:%d %A (%d)" iterationCount  s.Elapsed (s.ElapsedMilliseconds / 1000L)
        let movesMade = float (totalCount - count)
        let r = randomness // Math.Min(0.9,randomness + (randomIncrement * movesMade))
        match playMove log r gameToMetrics root t n with 
        | None -> 
            printfn "%A" game
            printfn "No more moves: %d" gameNumber
            printfn "Moves played: %.0f" movesMade
            false, gameNumber, history // TODO: keep reviing this

        | Some nMove -> 
            // let pastGames = MonteCarloTreeSearch.getParents root
            nMove.Parent <- None
            match nMove.TerminalValue |> Option.map int |> Option.map (fun x -> x = 1) with 
            | None -> 

                if count <= 0 then 
                    printfn "%A" nMove.Game
                    printfn "Game Number: %d" gameNumber
                    printfn "Moves played: %.0f" movesMade
                    printfn "Lost: move count depleted:"
                    false, gameNumber, history

                else        
                    if log then      
                        printfn "%A" nMove.Game
                        printfn "Moves played: %.0f" movesMade
                    // printfn "Moves reminaing: %d" count
                    // let pastGames = Set.add game' pastGames
                    // let pastGames = Set.union pastGames (getGames nMove |> Set.ofList)
                    loop (updateHistory game nMove.Move.Value history) (count - 1) nMove nMove.Game

            | Some didWin -> 
                let game = nMove.Game
                if didWin then
                    let history = updateHistory game nMove.Move.Value history
                    // perodicSaveAsync gameNumber 0 history root        
                    printfn "%A" game
                    printfn "Moves taken: %.0f" movesMade
                    printfn "Game completed and saved %d" gameNumber
                    true, gameNumber, history
                else
               
                    printfn "%A" nMove.Game
                    printfn "Lost: %d" gameNumber
                    printfn "Moves played: %.0f" movesMade
                    false, gameNumber, history


    loop [] totalCount root game 

    // 198