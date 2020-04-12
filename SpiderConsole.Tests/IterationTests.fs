namespace SpiderConsole.Tests.Iteration
open SpiderSolitare.Game
open NUnit.Framework
open System
open System.Collections.Generic
open SpiderSolitare.MonteCarloTreeSearch
open Swensen.Unquote
open SpiderConsole.Tests.TestsSetupUtils
open SpiderSolitare

[<TestFixture>]
type IterationTests() =   
        
    [<Test>]
    member __.``given a single node when iteration is called then nRoot tree is expanded with another node`` () =
        
        let depth = 0.
        let rollout (_, _, _:int, _) = 0.5
        let pastGames = Dictionary<_, _>()
        let gameToMetrics = Dictionary<int, float * float>()
        
        let node = rootWithPastGame pastGames
        let nRoot = node
        updateScore gameToMetrics node (0.5, 1.)
        
        let shortGame = 49
        let shortNode = MutableNode(shortGame, 0.0, 0.5, Some "a", 1, Some node, None, 0)
        addNode pastGames shortNode [node]
        
        let expandNode _ = [shortNode]        
        
        iteration 1 depth expandNode rollout pastGames gameToMetrics node
        
        test <@
                List.length nRoot.Children = 1 &&
                    nRoot.Children |> List.head |> (fun x ->
                            x.GameHashCode = (49).GetHashCode() &&
                                x.Children = List.empty)
                 @>
                 
    [<Test>]
    member __.``given a single node when iteration is called then gameToMetrics is updated with the rollout value`` () =
        
        let depth = 0.
        let rollout (_:int, _:int, _:int, _: int) = 0.5
        let pastGames = Dictionary<_, _>()
        let gameToMetrics = Dictionary<int, float * float>()
        
        let node = rootWithPastGame pastGames
        updateScore gameToMetrics node (0.5, 1.)
        
        let shortGame = 49
        let shortNode = MutableNode<int,string>(shortGame, 0.0, 0.0, Some "a", 1, Some node, None, 0)
        addNode pastGames shortNode [node]
        
        let expandNode _ = [shortNode]        
        
        iteration 1 depth expandNode rollout pastGames gameToMetrics node
        
        test <@
                gameToMetrics.[node.GameHashCode] = (1., 2.) &&
                    gameToMetrics.[(49).GetHashCode()] = (0.5, 1.) &&
                        gameToMetrics.Count = 2 &&
                            shortNode.Reward = (rollout (0, 0, 0, 0))
                 @>
        
    [<Test>]
    member __.``given a game tree with no more moves when iteration is called then gameToMetrics is updated`` () =
        
        let depth = 0.
        let rollout (_, _, _:int, _) = 0.5
        let pastGames = Dictionary<_, _>()
        let gameToMetrics = Dictionary<int, float * float>()
        
        let node = rootWithPastGame pastGames
        updateScore gameToMetrics node (0.5, 2.)
        
        let nextGame = 49
        let nextNode = MutableNode(nextGame, 0.0, 0.5, Some "a", 1, Some node, None, 0)
        addNode pastGames nextNode [node]
        updateScore gameToMetrics nextNode (0.25, 1.)
        node.Children <- [nextNode]
        
        let expandNode _ = []        
        
        iteration 1 depth expandNode rollout pastGames gameToMetrics node
        
        test <@
                gameToMetrics.[node.GameHashCode] = (0.5, 3.) &&
                    gameToMetrics.[(49).GetHashCode()] = (0.25, 2.) &&
                        gameToMetrics.Count = 2
                 @>
        
    [<Test>]
    member __.``given a game tree with a terminal node when iteration is called then gameToMetrics is updated`` () =
        
        let depth = 0.
        let rollout (_, _, _:int, _) = 0.5
        let pastGames = Dictionary<_, _>()
        let gameToMetrics = Dictionary<int, float * float>()
        
        let node = rootWithPastGame pastGames
        updateScore gameToMetrics node (0.5, 2.)
        
        let nextGame = 49
        let nextNode = MutableNode(nextGame, 0.0, 1.0, Some "a", 1, Some node, Some true, 0)
        addNode pastGames nextNode [node]
        updateScore gameToMetrics nextNode (0.25, 1.)
        node.Children <- [nextNode]
        
        let expandNode _ = []        
        
        iteration 1 depth expandNode rollout pastGames gameToMetrics node
        
        test <@
                gameToMetrics.[node.GameHashCode] = (1.5, 3.) &&
                    gameToMetrics.[(49).GetHashCode()] = (1.25, 2.) &&
                        gameToMetrics.Count = 2
                 @>

    [<Test>]
    member __.``given many children when iteration is called then each child is rolled out`` () =
        
        let depth = 0.
        let rollout (game, _: int, _:int, _ :int) =
            match game with
            | 49 -> 1.0
            | -1 -> 0.
            | _ -> 0.5
            
        let pastGames = Dictionary<_, _>()
        let gameToMetrics = Dictionary<int, float * float>()
        
        let node = rootWithPastGame pastGames
        updateScore gameToMetrics node (0.5, 1.)

        let winningGame = 49
        let winningNode = MutableNode(winningGame, 0.0, 0.5, Some "a", 1, Some node, Some true, 0)
        addNode pastGames winningNode [node]
        
        let nextGame = -1
        let nextNode = MutableNode(nextGame, 0.0, 0.5, Some "a", 1, Some node, None, 0)
        addNode pastGames nextNode [node]
        
        let losingGame = -2
        let losingNode = MutableNode(losingGame, 0.0, 0.5, Some "a", 1, Some node, Some false, 0)
        addNode pastGames losingNode [node]
        
        let expandNode _ = [losingNode; nextNode; winningNode]
        
        iteration 1 depth expandNode rollout pastGames gameToMetrics node
        iteration 1 depth expandNode rollout pastGames gameToMetrics node
        iteration 1 depth expandNode rollout pastGames gameToMetrics node
        
        test <@
                gameToMetrics.[winningNode.GameHashCode] |> snd = 1.0 &&
                    gameToMetrics.[nextNode.GameHashCode] |> snd = 1.0 &&
                        gameToMetrics.[losingNode.GameHashCode] |> snd = 1.0
                 @>
        
    [<Test>]
    member __.``given the next node is a winning when calling iteration then gameToMetrics captures the win`` () =
        
        let depth = 0.
        let rollout (game, _: int, _:int, _ :int) =
            printfn "rollout game: %A" game
            match game with
            | 49 -> SpiderSolitare.MonteCarloTreeSearch.winningNodeReward
            | -1 -> 0.
            | _ -> 0.5
            
        let pastGames = Dictionary<_, _>()
        let gameToMetrics = Dictionary<int, float * float>()
        
        let node = rootWithPastGame pastGames
        updateScore gameToMetrics node (0.5, 1.)

        let winningGame = 49
        let winningNode = MutableNode(winningGame, 1., 0.0, Some "a", 1, Some node, Some true, 0)
        addNode pastGames winningNode [node]
        
        let expandNode _ = [winningNode]
        
        iteration 1 depth expandNode rollout pastGames gameToMetrics node
        
        test <@
                winningNode.TerminalValue = Some true &&   
                    winningNode.Reward = MonteCarloTreeSearch.winningNodeReward &&
                        gameToMetrics.[winningNode.GameHashCode] = (MonteCarloTreeSearch.winningNodeReward, 1.0) &&
                            gameToMetrics.[node.GameHashCode] = (MonteCarloTreeSearch.winningNodeReward + 0.5, 2.) 
                 @>
        
    [<Test>]
    member __.``given a winning game among loosing game when iteration is called 100 times then the best game is the winning game`` () =
        
        let depth = 0.
        let rollout (game, _: int, _:int, _ :int) =
            match game with
            | 49 -> 1.0
            | -2 -> 0.
            | -3 -> 0.
            | _ -> 0.5
            
        let pastGames = Dictionary<_, _>()
        let gameToMetrics = Dictionary<int, float * float>()
        
        let node = rootWithPastGame pastGames
        updateScore gameToMetrics node (0.5, 1.)

        let winningGame = 49
        let winningNode = MutableNode(winningGame,1.0, 0.5, Some "a", 1, Some node, Some true, 0)
        let getWinningNode() =
            addNode pastGames winningNode [node]
            winningNode
        
        let nextGame = -1
        let nextNode = MutableNode(nextGame, 1.0, 0.5, Some "a", 1, Some node, None, 0)
        let getNextNode() =
            addNode pastGames nextNode [node]
            nextNode
        
        let losingGame = -2
        let losingNode = MutableNode(losingGame, 1.0, 0.5, Some "a", 1, Some node, Some false, 0)
        let getLosingNode() =
            addNode pastGames losingNode [node]
            losingNode
        
        let losingGameThree = -3
        let losingNodeThree = MutableNode(losingGameThree, 1.0, 0.5, Some "a", 1, Some nextNode, Some false, 0)
        
        let getNodeThree () = 
            addNode pastGames losingNodeThree [node; nextNode]
            losingNodeThree
        
        let expandNode (node: MutableNode<'a, 'b>) =
            match node.GameHashCode with
            | 42 -> [getLosingNode(); getNextNode(); getWinningNode()]
            | -1 -> [getNodeThree()]
            | _ -> []
        
        let run () = iteration 1 depth expandNode rollout pastGames gameToMetrics node
        [1 .. 100] |> List.iter (fun _ -> run ())
        
        test <@
                gameToMetrics.[node.GameHashCode] |> snd > 100. &&
                    gameToMetrics.[winningNode.GameHashCode] |> snd >= 90. &&
                        gameToMetrics.[losingNode.GameHashCode] |> snd < 10. &&
                            gameToMetrics.[nextNode.GameHashCode] |> snd < 10. &&
                                gameToMetrics.[losingNodeThree.GameHashCode] |> snd < 10.
                 @>        
        
    [<Test>]
    member __.``mcts favours the winning node over lots of almost wins`` () =
        
        let depth = 0.
        let rollout (game, _: int, _:int, _ :int) =
            match game with
            | 49 -> 1.0
            | 42 -> 0.5
            | 20 -> 0.9
            | -1 -> 0.5
            | _ -> 0.99
            
        let pastGames = Dictionary<_, _>()
        let gameToMetrics = Dictionary<int, float * float>()
        
        let node = rootWithPastGame pastGames
        updateScore gameToMetrics node (0.5, 1.)

        let nextGame = -1
        let nextNode = MutableNode(nextGame, 1.0, 0.5, Some "a", 1, Some node, None, 0)
        let getNextNode() =
            addNode pastGames nextNode [node]
            nextNode
            
        let winningGame = 49
        let winningNode = MutableNode(winningGame, 1.0, 0.5, Some "a", 1, Some nextNode, Some true, 0)
        let getWinningNode() =
            addNode pastGames winningNode [node; nextNode]
            winningNode
            
        let almostWinningNode =
            let losingGame = 20
            MutableNode(losingGame, 1.0, 0.5, Some "a", 1, Some node, None, 0) 
                
        let getAlmostWinningNodes () =
            addNode pastGames almostWinningNode [node]
            almostWinningNode
            
        let almostWinningNextNodes increment =
            [ 1 .. 100 ]
            |> List.map (fun x ->
                let losingGame = x + increment
                let almostWinningNode = MutableNode(losingGame, 1.0, 0.5, Some "a", 1, Some almostWinningNode, Some true, 0)
                fun () ->
                    addNode pastGames almostWinningNode [node; almostWinningNode]
                    almostWinningNode
                )
        
        let expandNode (node: MutableNode<'a, 'b>) =
            match node.GameHashCode with
            | 42 ->
                [
                    yield getAlmostWinningNodes ()
                    yield getNextNode() 
                ]
            | 20 -> almostWinningNextNodes 100 |> List.map (fun f -> f ())
            | -1 -> [getWinningNode()]
            | _ -> []
//                let size = x / 100 |> int
//                almostWinningNextNodes (size * 100) |> List.map (fun f -> f ())
        
        let run () = iteration 1 depth expandNode rollout pastGames gameToMetrics node
        [1 .. 100] |> List.iter (fun _ -> run ())
        
        printTree gameToMetrics node
        
        test <@
                node.Children
                |> List.map (fun x -> x.GameHashCode, gameToMetrics.[x.GameHashCode] |> snd)
                |> List.sortByDescending snd
                |> List.maxBy snd
                |> fst
                    = -1
                 @>
        
        
    [<Ignore("The expected value is hard-coded to a hash code which needs to be changed")>]
    [<Test>]
    member __.``search finds the best move`` () =
        
        let pastGames = Dictionary<_, _>()
        let gameToMetrics = Dictionary<int, float * float>()
        
        let r = Random(42)
        let deck = deck |> List.take 13
        let game = createGameSingleDeck r deck |> GameMover.unHideGame

            
        let almostFinishedGame =
            [|
                MoveType.Move { From = C1;  To = C4; Card = Card (8, S) }
                MoveType.Move { From = C1;  To = C5; Card = Card (1, S) }
                MoveType.Move { From = C1;  To = C6; Card = Card (5, S) }
                MoveType.Move { From = C1;  To = C7; Card = Card (13, S) }
                MoveType.Move { From = C1;  To = C2; Card = Card (3, S) }
                MoveType.Move { From = C1;  To = C4; Card = Card (7, S) }
                MoveType.Move { From = C2;  To = C1; Card = Card (4, S) }
                MoveType.Move { From = C1;  To = C8; Card = Card (3, S) }
                MoveType.Move { From = C1;  To = C6; Card = Card (4, S) }
                MoveType.Move { From = C2;  To = C1; Card = Card (9, S) }
                MoveType.Move { From = C1;  To = C3; Card = Card (9, S) }
                MoveType.Move { From = C2;  To = C4; Card = Card (6, S) }
                MoveType.Move { From = C2;  To = C1; Card = Card (12, S) }
                MoveType.Move { From = C2;  To = C8; Card = Card (2, S) }
                MoveType.Move { From = C3;  To = C1; Card = Card (10, S) }
                MoveType.Move { From = C6;  To = C2; Card = Card (4, S) }
                MoveType.Move { From = C1;  To = C3; Card = Card (9, S) }
                MoveType.Move { From = C1;  To = C7; Card = Card (12, S) }
                MoveType.Move { From = C2;  To = C1; Card = Card (4, S) }
                MoveType.Move { From = C1;  To = C6; Card = Card (4, S) }
                MoveType.Move { From = C3;  To = C1; Card = Card (9, S) }
                MoveType.Move { From = C1;  To = C2; Card = Card (9, S) }
                MoveType.Move { From = C4;  To = C1; Card = Card (6, S) }
                MoveType.Move { From = C4;  To = C3; Card = Card (7, S) }
                MoveType.Move { From = C2;  To = C10; Card = Card (9, S) }
                MoveType.Move { From = C6;  To = C2; Card = Card (4, S) }
                MoveType.Move { From = C1;  To = C3; Card = Card (6, S) }
                MoveType.Move { From = C10;  To = C7; Card = Card (9, S) }
                MoveType.Move { From = C4;  To = C7; Card = Card (8, S) }
                MoveType.Move { From = C3;  To = C7; Card = Card (7, S) }
                MoveType.Move { From = C6;  To = C7; Card = Card (5, S) }
            |]  |> playMoves game
            
        let node = MutableNode(almostFinishedGame, 1.0, 0.5, None, 0, None, None, 0)
        addNode pastGames node []

        let brainsMover =
            { new IBransMover with
                    member this.GetBestMove _ = []
                    member this.GetValue _ = 0.0
                    member this.GetMoves _ = []
                    member this.Flush() = ()
                 }
            
        Searcher(false, brainsMover, pastGames, gameToMetrics).SearchRandom(800, node)
        
        node.Children
        |> List.sortByDescending (fun x -> reward x.Game)
        |> List.iter (fun x ->
            printfn "%f" <| reward x.Game
            printfn "%d" x.GameHashCode
            printfn ""
            )
        
        test <@
                node.Children
                |> List.map (fun x -> x.GameHashCode, gameToMetrics.[x.GameHashCode])
                |> List.sortByDescending (snd >> snd)
                |> List.maxBy (fun (_, (_, n)) ->  n)
                |> fst
                    = -1766123932 // hash code of th game with the next best move ie highest reward. 
                 @>
        
        
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

        let brainsMover =
            { new IBransMover with
                    member this.GetBestMove _ = []
                    member this.GetValue _ = 0.0
                    member this.GetMoves _ = []
                    member this.Flush() = ()
                 }
        
        Searcher(false, brainsMover, pastGames, gameToMetrics).SearchRandom(2000, node)
        printTree gameToMetrics node
        
        test <@
                 node.Children
                |> List.map (fun x -> x.GameHashCode, gameToMetrics.[x.GameHashCode])
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
        let deck = CardModule.deck OneSuit |> List.take 13
        let game = createGameSingleDeck r deck |> GameMover.unHideGame        
        let moves = movesToStackToSeven

        let almostFinishedGame = moves |> playMoves game
        let nextGameForBestMove =
            Array.append moves [| MoveType.Move { From = C6;  To = C7; Card = Card (5, S) } |] |> playMoves game

        let node = MutableNode(almostFinishedGame, 1.0, 0.5, None, 0, None, None, 0)
        addNode pastGames node []
        
        Searcher(false, mockkBrainServer, pastGames, gameToMetrics).SearchRandom(3500, node)
        printTree gameToMetrics node
        
        test <@
                node.Children
                |> List.map (fun x -> x.GameHashCode, gameToMetrics.[x.GameHashCode])
                |> List.sortByDescending (snd >> snd)
                |> List.maxBy (fun (_, (_, n)) ->  n)
                |> fst
                    = nextGameForBestMove.GetHashCode() 
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

        let brainsMover =
            { new IBransMover with
                    member this.GetBestMove _ = []
                    member this.GetValue _ = 0.0
                    member this.GetMoves _ = []
                    member this.Flush() = ()
                 }
        
        Searcher(false, brainsMover, pastGames, gameToMetrics).SearchRandom(10, node)
        
        test <@
                 node.Children
                |> List.map (fun x -> x.GameHashCode, gameToMetrics.[x.GameHashCode])
                |> List.sortByDescending (snd >> snd)
                |> List.maxBy (fun (_, (_, n)) ->  n)
                |> fst
                |> fun x -> bestMoves |> List.contains x
                 @>
        
        
        
        
        
        
        

            
        
        
                
                
        
        
        
        
        
        
        
        
        
        
        