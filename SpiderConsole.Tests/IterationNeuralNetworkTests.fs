namespace SpiderConsole.Tests.IterationNeuralNetworkTests

open FsCheck
open SpiderSolitare.Brain
open System
open System.Collections.Generic
open NUnit.Framework
open FsCheck.NUnit
open NUnit.Framework
open SpiderSolitare.Game
open SpiderConsole.Tests.TestsSetupUtils
open SpiderSolitare.MonteCarloTreeSearch
open Swensen.Unquote
type NoShrink =
    static member Int() =
        {new Arbitrary<Int32>() with
            override x.Generator = Arb.Default.Int32().Generator |> Gen.map Math.Abs
            override x.Shrinker t = Seq.empty }
        

[<TestFixture>]
type IterationNeuralNetworkTests() =
    
    let brain = BrainMoverServer(5100)
    let brainsMover = BrainsMoverClient(5100) :> IBransMover
    do
        brain.StartServer()
        
    let getValue (gameToMetrics: Dictionary<_, _>) gameHashCode =
        match gameToMetrics.TryGetValue gameHashCode with
        | true, (t,n) -> (t,n)
        | false, _ -> (0., 0.)
        
    [<OneTimeTearDown>]
    member this.TearDown () =
        brain.Stop()

    [<Test>]
    member __.``can find wining game`` () =
        
        let pastGames = Dictionary<_, _>()
        let gameToMetrics = Dictionary<int, float * float>()
        
        let r = Random(42)
        let deck = deck |> List.take 13
        let game = createGameSingleDeck r deck |> GameMover.unHideGame
            
        let moves = 
            [|
                yield! movesToStackToSeven
                yield MoveType.Move { From = C6;  To = C7; Card = Card (5, S) }
                yield MoveType.Move { From = C2;  To = C7; Card = Card (4, S) }
                yield MoveType.Move { From = C8;  To = C7; Card = Card (3, S) }
            |]
            
        let almostFinishedGame = moves |> playMoves game
        let finishedGame =
            Array.append moves [| MoveType.Move { From = C5;  To = C7; Card = Card (1, S) } |] |> playMoves game

            
        let node = MutableNode(almostFinishedGame, 0.0, 0.5, None, 0, None, None, 0)
        addNode pastGames node []
        
        Searcher(false, brainsMover, pastGames, gameToMetrics).SearchWithNN(25, node) |> ignore
        printTree gameToMetrics node
            
        test <@
                 node.Children
                |> List.map (fun x ->
                    let tAndN = getValue gameToMetrics x.GameHashCode
                    x.GameHashCode, tAndN )
                |> List.sortByDescending (snd >> snd)
                |> List.maxBy (fun (_, (_, n)) ->  n)
                |> fst
                    = finishedGame.GetHashCode()
                 @>
        
    [<Test>]
    member __.``can find wining game when there is a lot of searching in the tree`` () =
        
        let pastGames = Dictionary<_, _>()
        let gameToMetrics = Dictionary<int, float * float>()
        
        let r = Random(42)
        let deck = deck |> List.take 13
        let game = createGameSingleDeck r deck |> GameMover.unHideGame
            
        let moves = 
            [|
                yield! movesToStackToSeven
                yield MoveType.Move { From = C6;  To = C7; Card = Card (5, S) }
                yield MoveType.Move { From = C2;  To = C7; Card = Card (4, S) }
                yield MoveType.Move { From = C8;  To = C7; Card = Card (3, S) }
            |]
            
        let almostFinishedGame = moves |> playMoves game
        let finishedGame =
            Array.append moves [| MoveType.Move { From = C5;  To = C7; Card = Card (1, S) } |] |> playMoves game

            
        let node = MutableNode(almostFinishedGame, 0.0, 0.5, None, 0, None, None, 0)
        addNode pastGames node []
        
        Searcher(false, brainsMover, pastGames, gameToMetrics).SearchWithNN(10000, node) |> ignore
        printTree gameToMetrics node
        
        let getValue gameHashCode =
            match gameToMetrics.TryGetValue gameHashCode with
            | true, (t,n) -> (t,n)
            | false, _ -> (0., 0.)
            
        test <@
                 node.Children
                |> List.map (fun x ->
                    let tAndN = getValue x.GameHashCode
                    x.GameHashCode, tAndN )
                |> List.sortByDescending (snd >> snd)
                |> List.maxBy (fun (_, (_, n)) ->  n)
                |> fst
                    = finishedGame.GetHashCode()
                 @>

    [<Test>] 
    member __.``can find the shortest path to winning game`` () =
        
        let pastGames = Dictionary<_, _>()
        let gameToMetrics = Dictionary<int, float * float>()
        
        let r = Random(42)
        let deck = deck |> List.take 13
        let game = createGameSingleDeck r deck |> GameMover.unHideGame
        let moves = movesToStackToSeven
            
        let almostFinishedGame = moves |> playMoves game
        let nextGameForBestMove =
            Array.append moves [| MoveType.Move { From = C6;  To = C7; Card = Card (5, S) } |] |> playMoves game

        let node = MutableNode(almostFinishedGame, 0.0, 0.5, None, 0, None, None, 0)
        addNode pastGames node []
        
        Searcher(false, brainsMover, pastGames, gameToMetrics).SearchWithNN(15, node) |> ignore
//        Searcher(false, brainsMover, pastGames, gameToMetrics).SearchWithNN(105, node)
        printTree gameToMetrics node
        
        
        printfn "%A" almostFinishedGame
        node.Children
        |> List.map (fun x -> x.Game, x.Move, gameToMetrics.[x.GameHashCode])
        |> List.maxBy (fun (game, move, (t,n)) -> n)
        |> fun (game, move, _) -> printfn "%A" game; printfn "%A" move
        
        test <@
                node.Children
                |> List.map (fun x -> x.GameHashCode, gameToMetrics.[x.GameHashCode])
                |> List.sortByDescending (snd >> snd)
                |> List.maxBy (fun (_, (_, n)) ->  n)
                |> fst
                    = nextGameForBestMove.GetHashCode() 
                 @>
        
        
    [<Test>] 
    member __.``can find best move to sort 13 cards`` () =
        
        let pastGames = Dictionary<_, _>()
        let gameToMetrics = Dictionary<int, float * float>()
        
        let r = Random(42)
        let deck = deck |> List.take 13
        let game = createGameSingleDeck r deck |> GameMover.unHideGame

        let playSingleMove = playSingleMove game
        let bestMoves =
            [
                { From = C2;  To = C4; Card = Card (4, S) } |> playSingleMove
                { From = C2;  To = C5; Card = Card (4, S) } |> playSingleMove
                { From = C2;  To = C6; Card = Card (4, S) } |> playSingleMove
                { From = C2;  To = C7; Card = Card (4, S) } |> playSingleMove
                { From = C2;  To = C8; Card = Card (4, S) } |> playSingleMove
                { From = C2;  To = C9; Card = Card (4, S) } |> playSingleMove
                { From = C2;  To = C10; Card = Card (4, S) } |> playSingleMove
                
                { From = C1;  To = C4; Card = Card (8, S) } |> playSingleMove
                { From = C1;  To = C5; Card = Card (8, S) } |> playSingleMove
                { From = C1;  To = C6; Card = Card (8, S) } |> playSingleMove
                { From = C1;  To = C7; Card = Card (8, S) } |> playSingleMove
                { From = C1;  To = C8; Card = Card (8, S) } |> playSingleMove
                { From = C1;  To = C9; Card = Card (8, S) } |> playSingleMove
                { From = C1;  To = C10; Card = Card (8, S) } |> playSingleMove
            ]

        let node = MutableNode(game, 0.0, 0.5, None, 0, None, None, 0)
        addNode pastGames node []
        
        Searcher(false, brainsMover, pastGames, gameToMetrics).SearchWithNN(62, node) |> ignore
        printTree gameToMetrics node
        
        test <@
                node.Children
                |> List.map (fun x -> x.GameHashCode, gameToMetrics.[x.GameHashCode])
                |> List.sortByDescending (snd >> snd)
                |> List.maxBy (fun (_, (_, n)) ->  n)
                |> fst
                |> fun x -> bestMoves |> List.contains x 
                 @>
        
        
    [<Test>]
    member __.``can find good move to start game`` () =
        
        let pastGames = Dictionary<_, _>()
        let gameToMetrics = Dictionary<int, float * float>()
        
        let r = Random(42)
        let deck = deck
        let game = createGameSingleDeck r deck |> GameMover.unHideGame
            
        let playSingleMove = playSingleMove game
        let bestMoves =
            [
                { From = C4;  To = C10; Card = Card (9, S) } |> playSingleMove
                { From = C5;  To = C10; Card = Card (9, S) } |> playSingleMove
                { From = C4;  To = C3; Card = Card (9, S) } |> playSingleMove
                { From = C5;  To = C3; Card = Card (9, S) } |> playSingleMove
                
                { From = C7;  To = C4; Card = Card (8, S) } |> playSingleMove
                { From = C7;  To = C5; Card = Card (8, S) } |> playSingleMove                
                
                { From = C6;  To = C7; Card = Card (7, S) } |> playSingleMove
                { From = C9;  To = C7; Card = Card (7, S) } |> playSingleMove
                
                { From = C8;  To = C6; Card = Card (6, S) } |> playSingleMove
                { From = C8;  To = C9; Card = Card (6, S) } |> playSingleMove
            ]

            
        let node = MutableNode(game, 0.0, 0.5, None, 0, None, None, 0)
        addNode pastGames node []

        Searcher(false, brainsMover, pastGames, gameToMetrics).SearchWithNN(5, node) |> ignore
        
        test <@
                 node.Children
                |> List.map (fun x -> x.GameHashCode, gameToMetrics.[x.GameHashCode])
                |> List.sortByDescending (snd >> snd)
                |> List.maxBy (fun (_, (_, n)) ->  n)
                |> fst
                |> fun x -> bestMoves |> List.contains x
                 @>
        
//    [<Property(MaxTest = 10, Arbitrary = [| typeof<NoShrink> |], Replay = "1972085421,296718401")>]
    [<Test>]
    member __.``Neural network can find the winning node when searching when 1 move away`` () =
        
        let r = Random(1)
        let deck = CardModule.deck OneSuit |> List.take 13
        let game = createGameSingleDeck r deck |> GameMover.unHideGame
        printfn "%A" game
        
        let moves =
            [|
                { From = C1; To = C4; Card = Card (12, S) }
                { From = C1; To = C2; Card = Card (2, S) }
                { From = C1; To = C2; Card = Card (1, S) }
                { From = C1; To = C5; Card = Card (13, S) }
                { From = C2; To = C6; Card = Card (3, S) }
                
                { From = C2; To = C1; Card = Card (9, S) }
                { From = C1; To = C2; Card = Card (10, S) }
                { From = C2; To = C4; Card = Card (11, S) }
                { From = C4; To = C5; Card = Card (12, S) }
                
                { From = C2; To = C3; Card = Card (6, S) }
                { From = C3; To = C2; Card = Card (7, S) }
                { From = C2; To = C4; Card = Card (8, S) }
                
                { From = C2; To = C1; Card = Card (4, S) }
                { From = C6; To = C1; Card = Card (3, S) }
            |] |> Array.map MoveType.Move
            
        let game = moves |> playMoves game
    
        let pastGames = Dictionary<_, _>() 
        let gameToMetrics = Dictionary<int, float * float>()
        let node = MutableNode(game, 0.0, 0.5, None, 0, None, None, 0)
        addNode pastGames node []
        
        let nn =
            let searcher = Searcher(false, brainsMover, pastGames, gameToMetrics)
            let search(iterationCount, node) = searcher.SearchWithNN(iterationCount, node) |> ignore
            playGame gameToMetrics 50 5 node search
            
        printTree gameToMetrics node
        
        test <@
                 flattenTree node 
                 |> List.exists (fun (a,c,d,e,f) -> f = Some true)
             @>
        
        
//    [<Property(MaxTest = 10, Arbitrary = [| typeof<NoShrink> |], Replay = "1972085421,296718401")>]
    [<Test>]
    member __.``Neural network can find the winning node when searching`` () =
        
        let r = Random(1)
        let deck = CardModule.deck OneSuit |> List.take 13
        let game = createGameSingleDeck r deck |> GameMover.unHideGame
        printfn "%A" game
        
        let moves =
            [|
                { From = C1; To = C4; Card = Card (12, S) }
                { From = C1; To = C2; Card = Card (2, S) }
                { From = C1; To = C2; Card = Card (1, S) }
                { From = C1; To = C5; Card = Card (13, S) }
                { From = C2; To = C6; Card = Card (3, S) }
                
                { From = C2; To = C1; Card = Card (9, S) }
                { From = C1; To = C2; Card = Card (10, S) }
                { From = C2; To = C4; Card = Card (11, S) }
                { From = C4; To = C5; Card = Card (12, S) }
                
                { From = C2; To = C3; Card = Card (6, S) }
//                { From = C3; To = C2; Card = Card (7, S) }
//                { From = C2; To = C4; Card = Card (8, S) }
                
//                { From = C2; To = C1; Card = Card (4, S) }
//                { From = C6; To = C1; Card = Card (3, S) }
                
            |] |> Array.map MoveType.Move
            
        let game = moves |> playMoves game
    
        let pastGames = Dictionary<_, _>() 
        let gameToMetrics = Dictionary<int, float * float>()
        let node = MutableNode(game, 0.0, 0.5, None, 0, None, None, 0)
        addNode pastGames node []
        
        let nn =
            let searcher = Searcher(false, brainsMover, pastGames, gameToMetrics)
            let search(iterationCount, node) = searcher.SearchWithNN(iterationCount, node) |> ignore
            playGame gameToMetrics 1310 1 node search   // 1322 // 1342
            
        printTree gameToMetrics node
        
        test <@
                 flattenTree node 
                 |> List.exists (fun (a,c,d,e,f) -> f = Some true)
             @>
        
        
    [<Test>]
    member __.``Neural network can find the winning node when searching with a lot of searching`` () =
        
        let r = Random(1)
        let deck = CardModule.deck OneSuit |> List.take 13
        let game = createGameSingleDeck r deck |> GameMover.unHideGame
        
        
        let moves =
            [|
                { From = C1; To = C4; Card = Card (12, S) }
                { From = C1; To = C2; Card = Card (2, S) }
                { From = C1; To = C2; Card = Card (1, S) }
                { From = C1; To = C5; Card = Card (13, S) }
                { From = C2; To = C6; Card = Card (3, S) }
                
                { From = C2; To = C1; Card = Card (9, S) }
                { From = C1; To = C2; Card = Card (10, S) }
                { From = C2; To = C4; Card = Card (11, S) }
                { From = C4; To = C5; Card = Card (12, S) }
                
                { From = C2; To = C3; Card = Card (6, S) }
                { From = C3; To = C2; Card = Card (7, S) }
                
            |] |> Array.map MoveType.Move
            
        let game = moves |> playMoves game
        printfn "%A" game
    
        let pastGames = Dictionary<_, _>() 
        let gameToMetrics = Dictionary<int, float * float>()
                                                            

        let node = MutableNode(game, 0.0, 0.5, None, 0, None, None, 0)
        addNode pastGames node []
        
        let searcher = Searcher(false, brainsMover, pastGames, gameToMetrics)           
        searcher.SearchWithNN(1000, node) |> ignore
        
        printTreeWithDepth 1 gameToMetrics node

        let rec finishGame (root: MutableNode<Game,MoveType>) =
            
            match root.TerminalValue with
            | None ->
                let nextNode = root.Children |> List.maxBy (getMetrics gameToMetrics >> snd)
                printfn "%A" nextNode.Game
                printfn "Move: %A" nextNode.Move
                finishGame nextNode
            | Some x -> root.GameHashCode
            
        let winningGame = {Game.emptyGame with Spades = One}
        
        test <@
                finishGame node = winningGame.GetHashCode()
             @>
        
    
    
        
        