namespace SpiderSolitarTests.MtctsTests
open NUnit.Framework
open System
open SpiderSolitare.Game
open System.Collections.Generic
open SpiderSolitare.MonteCarloTreeSearch
open Swensen.Unquote

[<TestFixture>]
type MctsTests() =
    
    let toMap kvps =
        kvps
        |> Seq.map (|KeyValue|)
        |> Map.ofSeq
    
    let deck = CardModule.deck OneSuit
    
    let getMoves game =
        let ms  = GameMover.validMoves game
        ms |> List.map (fun x ->  x, 1. / float ms.Length)
        
    let getContinueGame = function
        | Continue (g, _) -> g
        | x -> failwithf "Expected game to be Continue but got: %A" x
    
    let createGame r = GameMover.startGame deck r |> getContinueGame
            
    let rootWithPastGame (pastGames: Dictionary<int, int Set>) =
        
        let r = Random(42)
        let game = createGame r

        let node = MutableNode(game, 0.0, 0.01,  None, 0, None, None)
        pastGames.[node.GameHashCode] <- Set.empty
        game, node
        
    let addNode (pastGames: Dictionary<int, int Set>) (nextNode:MutableNode) (parents: MutableNode list) =
        pastGames.[nextNode.GameHashCode] <- parents |> List.map (fun x -> x.GameHashCode) |> Set.ofList
        

    let createNode r parent  =
        let r = Random(r)
        let nextGame = createGame r
        MutableNode(nextGame, 0.0, reward nextGame,  None, 0, Some parent, None)
        
    let appendNodeToTree r parent  =
        let nextNode = createNode r parent
        parent.Children <- [nextNode]
        nextNode
        
        
    [<Test>]
    member __.``given all next moves are not seen when expand is called then all nodes are set to be added`` () =
        let pastGames = Dictionary<_, _>()
        let game, node = rootWithPastGame pastGames
        
        test <@
                expandNode getMoves pastGames node
                 |> List.map (fun x -> x.GameHashCode)
//                 |> List.choose (function
//                     | AddLeaf x -> Some x.GameHashCode
//                     | _ -> None)
                 = (getMoves game
                    |> List.map (fun (move, _) ->
                            GameMover.playMove move game
                            |> getContinueGame
                            |> fun x -> x.GetHashCode() )
                        )
             @>
        
//    [<Test>]
//    member __.``given one move has been seen when expand is called only the moves not seen are added`` () =
//        let pastGames = Dictionary<_, _>()
//        let game, node = rootWithPastGame pastGames
//        
//        let nextNodes =
//            getMoves game
//            |> List.map (fun (move, _) ->
//                let g = GameMover.playMove move game |> getContinueGame
//                MutableNode(g, 0.0, reward g,  Some move, 0, Some node, None) )
//            
//        addNode pastGames (List.head nextNodes) [node]
//        
//        let nextNode = nextNodes |> List.head |> fun x -> x.GameHashCode
//        printfn "%A" pastGames.[nextNode]
//            
//        test <@
//                expandNode getMoves pastGames node
//                 |> List.map (fun x -> x.GameHashCode)
////                 |> List.map (function
////                     | AddLeaf _ -> 1
////                     | LinkSeenNode _ -> 2 )
//                 = (nextNodes |> List.tail |> List.map (fun x -> x.GameHashCode))
//             @>
        
//    [<Test>]
//    member __.``given the node is already in the longest path when expand is called then the node is set to be removed`` () =
//        let pastGames = Dictionary<_, _>()
//        let game, node = rootWithPastGame pastGames
//        
//        let r = Random(43)
//        let nextGame = createGame r
//        let nextNode = MutableNode(nextGame, 0.0, reward nextGame,  None, 0, Some node, None)
//        
//        addNode pastGames nextNode [node]
//        
//        let move, nextNextNode =
//            getMoves game
//            |> List.head
//            |> (fun (move, _) ->
//                let g = GameMover.playMove move game |> getContinueGame    
//                move, MutableNode(g, 0.0, reward g,  Some move, 0, Some nextNode, None) )
//
//        addNode pastGames nextNextNode [nextNode; node]
//        let getMoves (_:Game) = [move, 0.4]
//
//        test <@
//                expandNode getMoves pastGames node
//                 |> List.map (fun x -> x.GameHashCode)
////                 |> List.choose (function
////                     | AddLeaf _ -> None
////                     | LinkSeenNode (h, s,l) -> Some (h,s,l)
////                     )
//                 = [(nextNextNode.GameHashCode, [node.GameHashCode], [nextNode.GameHashCode; node.GameHashCode])]
//             @>
        
    [<Test>]
    member __.``given all next moves are not seen when expand is called then all nodes are in the pastGames`` () =
        let pastGames = Dictionary<_, _>()
        let game, node = rootWithPastGame pastGames
        
        expandNode getMoves pastGames node |> ignore
        
        let nextMoves =
            getMoves game
            |> List.map (fun (move, _) ->
                let game = GameMover.playMove move game |> getContinueGame                    
                (game.GetHashCode() , [node.GameHashCode])
            )

        test <@
                 pastGames
                     |> toMap
                     |> Map.map (fun key y -> y |> seq |> Seq.toList) =
                        ((node.GameHashCode, []) :: nextMoves |> Map.ofList)
                 
             @>
        
    [<Test>]
    member __.``given a next move has been seen when expand is called then all nodes are in the pastGames`` () =
        let pastGames = Dictionary<_, _>()
        let game, node = rootWithPastGame pastGames
        
        let nextNode =
            getMoves game
            |> List.head
            |> (fun (move, _) ->
                let g = GameMover.playMove move game |> getContinueGame
                MutableNode(g, 0.0, reward g,  Some move, 0, Some node, None) )
            
        addNode pastGames nextNode [node]
        
        expandNode getMoves pastGames node |> ignore
        
        let nextMoves =
            getMoves game
            |> List.map (fun (move, _) ->
                let game = GameMover.playMove move game |> getContinueGame                    
                (game.GetHashCode() , [node.GameHashCode])
            )

        test <@
                 pastGames
                     |> toMap
                     |> Map.map (fun key y -> y |> seq |> Seq.toList) =
                        ((node.GameHashCode, []) :: nextMoves |> Map.ofList)
                 
             @>
        
//    [<Test>]
//    member __.``given a next move is in the longest when expand then it is moved to the shortest path`` () =
//        let pastGames = Dictionary<_, _>()
//        let game, node = rootWithPastGame pastGames
//        
//        let r = Random(43)
//        let nextGame = createGame r
//        let nextNode = MutableNode(nextGame, 0.0, reward nextGame,  None, 0, Some node, None)
//        
//        addNode pastGames nextNode [node]
//        
//        let move, nextNextNode =
//            getMoves game
//            |> List.head
//            |> (fun (move, _) ->
//                let g = GameMover.playMove move game |> getContinueGame    
//                move, MutableNode(g, 0.0, reward g,  Some move, 0, Some nextNode, None) )
//
//        addNode pastGames nextNextNode [nextNode; node]
//        let getMoves (_:Game) = [move, 0.4]
//        
//        expandNode getMoves pastGames node |> ignore
//
//        test <@
//                 pastGames
//                     |> toMap
//                     |> Map.map (fun key y -> y |> seq |> Seq.toList) =
//                        ([node.GameHashCode, []; nextNode.GameHashCode, [node.GameHashCode]; nextNextNode.GameHashCode, [nextNode.GameHashCode; node.GameHashCode]]  |> Map.ofList)  
//             @>
        
    [<Test>]
    member __.``Given a game is in the first level of nRoot when updateLongestPath is called then the game is removed`` () =
        let pastGames = Dictionary<_, _>()
        let game, nRoot = rootWithPastGame pastGames
        
        let nextNode = appendNodeToTree 43 nRoot
        
        test <@
                 updateLongestPath nextNode.GameHashCode [nRoot.GameHashCode] nRoot = nextNode && 
                    nRoot.Children = List.empty
             @>
        
    [<Test>]
    member __.``Given a game is in the second level of nRoot when updateLongestPath is called then the game is removed`` () =
        let pastGames = Dictionary<_, _>()
        let game, nRoot = rootWithPastGame pastGames
        
        let nextNode = appendNodeToTree 43 nRoot
        let nextNextNode = appendNodeToTree 44 nextNode
        
        test <@
                 updateLongestPath nextNextNode.GameHashCode [nRoot.GameHashCode; nextNode.GameHashCode] nRoot = nextNextNode && 
                    nextNode.Children = List.empty &&
                        nRoot.Children = [nextNode]
             @>
        
    [<Test>]
    member __.``Given a game is in the third level of nRoot when updateLongestPath is called then the game is removed`` () =
        let pastGames = Dictionary<_, _>()
        let game, nRoot = rootWithPastGame pastGames
        
        let nextNode = appendNodeToTree 43 nRoot
        let nextNextNode = appendNodeToTree 44 nextNode
        let nextNexNexttNode = appendNodeToTree 45 nextNextNode
        
        test <@
                 updateLongestPath nextNexNexttNode.GameHashCode [nRoot.GameHashCode; nextNode.GameHashCode; nextNextNode.GameHashCode] nRoot = nextNexNexttNode &&
                    nextNextNode.Children = List.empty &&
                        nextNode.Children = [nextNextNode] &&
                            nRoot.Children = [nextNode]
             @>


    [<Test>]
    member __.``Given a game is in the first level of nRoot when updateShortPath is called then the game is added`` () =
        let pastGames = Dictionary<_, _>()
        let game, nRoot = rootWithPastGame pastGames
        
        let nextNode = createNode 43 nRoot
        
        updateShortestPath [nRoot.GameHashCode] nextNode nRoot
        
        test <@ nRoot.Children = [nextNode] @>
        
    [<Test>]
    member __.``Given a game is in the second level of nRoot when updateShortPath is called then the game is added`` () =
        let pastGames = Dictionary<_, _>()
        let game, nRoot = rootWithPastGame pastGames
        
        let nextNode = appendNodeToTree 43 nRoot
        let nexNexttNode = createNode 44 nextNode
        
        updateShortestPath [nRoot.GameHashCode; nextNode.GameHashCode] nexNexttNode nRoot
        
        test <@ nRoot.Children = [nextNode] && nextNode.Children = [nexNexttNode] @>

    [<Test>]
    member __.``Given a game is in the third of nRoot when updateShortPath is called then the game is added`` () =
        let pastGames = Dictionary<_, _>()
        let game, nRoot = rootWithPastGame pastGames
        
        let nextNode = appendNodeToTree 43 nRoot
        let nextNexttNode = appendNodeToTree 44 nextNode
        let nextNextNexttNode = createNode 45 nextNexttNode
        
        updateShortestPath [nRoot.GameHashCode; nextNode.GameHashCode; nextNexttNode.GameHashCode] nextNextNexttNode nRoot
        
        test <@ nRoot.Children = [nextNode] && nextNode.Children = [nextNexttNode] && nextNexttNode.Children = [nextNextNexttNode] @>
        
        
    [<Test>]
    member __.``Given a new game has a sibling when updateShortPath is called then the game is added`` () =
        let pastGames = Dictionary<_, _>()
        let game, nRoot = rootWithPastGame pastGames
        
        let nextNode = appendNodeToTree 43 nRoot
        let nextNexttNode = createNode 44 nextNode
        let nextNexttNode2 = createNode 44 nextNode
        nextNode.Children <- [nextNexttNode; nextNexttNode2]
        let nextNextNexttNode = createNode 45 nextNexttNode
        
        updateShortestPath [nRoot.GameHashCode; nextNode.GameHashCode; nextNexttNode.GameHashCode] nextNextNexttNode nRoot
        
        test <@
                    nRoot.Children = [nextNode] &&
                    nextNode.Children = [nextNexttNode; nextNexttNode2] &&
                    nextNexttNode.Children = [nextNextNexttNode]
             @>
        
    [<Test>]
    member __.``Given a node with children when updateShortPath is called then there are not circular nodes`` () =
        let pastGames = Dictionary<_, _>()
        let game, nRoot = rootWithPastGame pastGames
        
        let nextNode = appendNodeToTree 43 nRoot
        let nextNexttNode = createNode 44 nextNode
        let nextNexttNode2 = createNode 44 nextNode
        nextNode.Children <- [nextNexttNode; nextNexttNode2]
        let nextNextNexttNode = createNode 45 nextNexttNode
        
        updateShortestPath [nRoot.GameHashCode; nextNode.GameHashCode; nextNexttNode.GameHashCode] nextNextNexttNode nRoot
        
        test <@
                    nRoot.Children = [nextNode] &&
                    nextNode.Children = [nextNexttNode; nextNexttNode2] &&
                    nextNexttNode.Children = [nextNextNexttNode]
             @>      
        
        

        
        
        
        


