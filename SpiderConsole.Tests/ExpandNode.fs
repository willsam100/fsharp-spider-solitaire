namespace SpiderConsole.Tests.ExpandNode

open NUnit.Framework
open SpiderSolitare.MonteCarloTreeSearch
open System.Collections.Generic
open Swensen.Unquote

    
type ExpandNodeTests() =
    
    let addNode (pastGames: Dictionary<int, int Set>) (nextNode:MutableNode<'a, 'b>) (parents: MutableNode<'a, 'b> list) =
        pastGames.[nextNode.GameHashCode] <- parents |> List.map (fun x -> x.GameHashCode) |> Set.ofList
    
    let rootWithPastGame (pastGames: Dictionary<int, int Set>) =
        let node = MutableNode(42, 0.0, 0.01,  None, 0, None, None, 0)
        pastGames.[node.GameHashCode] <- Set.empty
        node

    [<Test>]
    member __.``given all next moves are not seen when expand is called then all nodes are set to be added`` () =
        let pastGames = Dictionary<_, _>()
        let node = rootWithPastGame pastGames
        let getMoves (_: int): (int * bool option * string * float) list =
            [43, None, "a", 0.0 ]
        
        test <@
                expandNode getMoves pastGames node |> List.map (fun x -> x.GameHashCode)
                    = [(43).GetHashCode()]
             @>
        
    [<Test>]
    member __.``given some next moves are seen when expand is called then new games are returned`` () =
        let pastGames = Dictionary<_, _>()
        let node = rootWithPastGame pastGames
        let nextGame = 43
        let getMoves (_: int): (int * bool option * string * float) list =  
            [
                nextGame, None, "a", 0.0
                44, None, "a", 0.0
             ]
            
        let nextNode = MutableNode(nextGame, 0.0, 0.5, Some "a", 1, Some node, None, 0)
        addNode pastGames nextNode [node]
        
        test <@
                expandNode getMoves pastGames node |> List.map (fun x -> x.GameHashCode)
                    = [(44).GetHashCode()]
             @>
        
    [<Test>]
    member __.``given all next moves have not been seen when expand is called then all moves are added to past games`` () =
        let pastGames = Dictionary<_, _>()
        let node = rootWithPastGame pastGames
        let getMoves (_: int): (int * bool option * string * float) list =
            [43, None, "a", 0.0 ]
            
        expandNode getMoves pastGames node |> ignore
        
        test <@
                 pastGames.[node.GameHashCode] = Set.empty &&
                    pastGames.[(43).GetHashCode()] = (Set.ofList [node.GameHashCode]) &&
                        pastGames.Count = 2
             @> 
        
    [<Test>]
    member __.``given some next moves are seen when expand is called then past games are updated`` () =
        let pastGames = Dictionary<_, _>()
        let node = rootWithPastGame pastGames
        let nextGame = 43
        let getMoves (_: int): (int * bool option * string * float) list =  
            [
                nextGame, None, "a", 0.0
                44, None, "a", 0.0
             ]
            
        let nextNode = MutableNode(nextGame, 0.0, 0.5, Some "a", 1, Some node, None, 0)
        addNode pastGames nextNode [node]
        
        expandNode getMoves pastGames node |> ignore
        
        test <@
                 pastGames.[node.GameHashCode] = Set.empty &&
                    pastGames.[nextGame] = (Set.ofList [node.GameHashCode]) &&
                        pastGames.[(44).GetHashCode()] = (Set.ofList [node.GameHashCode]) &&
                            pastGames.Count = 3
             @>
        
    [<Test>]
    member __.``given a circular move when expand is called then the game is not returned`` () =
        let pastGames = Dictionary<_, _>()
        let node = rootWithPastGame pastGames
        
        let nextGame = 43
        let nextNode = MutableNode(nextGame, 0.0, 0.5, Some "a", 1, Some node, None, 0)
        addNode pastGames nextNode [node]
        
        let getMoves (_: int): (int * bool option * string * float) list =  
            [ 42, None, "a", 0.0 ]
            
        test <@
                 expandNode getMoves pastGames nextNode |> List.map (fun x -> x.GameHashCode) = []
             @>
        
    [<Test>]
    member __.``given a circular move when expand is called then past games are updated`` () =
        let pastGames = Dictionary<_, _>()
        let node = rootWithPastGame pastGames
        
        let nextGame = 43
        let nextNode = MutableNode(nextGame, 0.0, 0.5, Some "a", 1, Some node, None, 0)
        addNode pastGames nextNode [node]
        
        let getMoves (_: int): (int * bool option * string * float) list =  
            [ 42, None, "a", 0.0 ]
            
        expandNode getMoves pastGames nextNode |> ignore
        
        test <@
                 pastGames.[node.GameHashCode] = Set.empty &&
                    pastGames.[nextGame] = (Set.ofList [node.GameHashCode]) &&
                            pastGames.Count = 2
             @>
        
    [<Test>]
    member __.``given a longer path is the next move when expand is called then the game is not returned`` () =
        let pastGames = Dictionary<_, _>()
        let node = rootWithPastGame pastGames
        
        let nextGame = 52
        let nextNode = MutableNode(nextGame, 0.0, 0.5, Some "a", 1, Some node, None, 0)
        addNode pastGames nextNode [node]
        
        let nextGame' = 43
        let nextNode' = MutableNode(nextGame', 0.0, 0.5, Some "a", 1, Some node, None, 0)
        addNode pastGames nextNode' [node]
        
        let getMoves (x: int): (int * bool option * string * float) list =
            if x <> 43 then failwith "Invalid test setup"
            [ 52, None, "a", 0.0 ]
            
        test <@
                 expandNode getMoves pastGames nextNode'  = []
             @>
        
    [<Test>]
    member __.``given a longer path is the next move when expand is called then the node is not changed in past games`` () =
        let pastGames = Dictionary<_, _>()
        let node = rootWithPastGame pastGames
        
        let nextGame = 52
        let nextNode = MutableNode(nextGame, 0.0, 0.5, Some "a", 1, Some node, None, 0)
        addNode pastGames nextNode [node]
        
        let nextGame' = 43
        let nextNode' = MutableNode(nextGame', 0.0, 0.5, Some "a", 1, Some node, None, 0)
        addNode pastGames nextNode' [node]
        
        let getMoves (x: int): (int * bool option * string * float) list =
            if x <> 43 then failwith "Invalid test setup"
            [ 52, None, "a", 0.0 ]
            
        expandNode getMoves pastGames nextNode' |> ignore
        test <@
                 pastGames.[node.GameHashCode] = Set.empty &&
                    pastGames.[nextGame] = (Set.ofList [node.GameHashCode]) &&
                        pastGames.[nextGame'] = (Set.ofList [node.GameHashCode]) &&
                            pastGames.Count = 3
             @>
        
    [<Test>]
    member __.``given a shorter path is the next move when expand is called then the game not returned`` () =
        let pastGames = Dictionary<_, _>()
        let node = rootWithPastGame pastGames
        
        let nextGame = 43
        let nextNode = MutableNode(nextGame, 0.0, 0.5, Some "a", 1, Some node, None, 0)
        addNode pastGames nextNode [node]
        
        let nextNextGame = 44
        let nextNextNode = MutableNode(nextNextGame, 0.0, 0.5, Some "a", 1, Some nextNode, None, 0)
        addNode pastGames nextNextNode [node; nextNode]
        
        let nextNextNextGame = 52
        let nextNextNextNode = MutableNode(nextNextNextGame, 0.0, 0.5, Some "a", 1, Some nextNextNode, None, 0)
        addNode pastGames nextNextNextNode [node; nextNode; nextNextNode]
        
        let shortGame = 49
        let shortNode = MutableNode(shortGame, 0.0, 0.5, Some "a", 1, Some node, None, 0)
        addNode pastGames shortNode [node]  
        
        let getMoves (x: int): (int * bool option * string * float) list =
            if x <> 49 then failwith "Invalid test setup"
            [ nextNextNextGame, None, "a", 0.0 ]
            
        test <@
                 expandNode getMoves pastGames shortNode
                    |> List.map (fun x -> x.GameHashCode)
                         = [(52).GetHashCode()]
             @>
        
    [<Test>]
    member __.``given a shorter path is the next move when expand is called then past games is updated to the shorter path`` () =
        let pastGames = Dictionary<_, _>()
        let node = rootWithPastGame pastGames
        
        let nextGame = 43
        let nextNode = MutableNode(nextGame, 0.0, 0.5, Some "a", 1, Some node, None, 0)
        addNode pastGames nextNode [node]
        
        let nextNextGame = 44
        let nextNextNode = MutableNode(nextNextGame, 0.0, 0.5, Some "a", 1, Some nextNode, None, 0)
        addNode pastGames nextNextNode [node; nextNode]
        
        let nextNextNextGame = 52
        let nextNextNextNode = MutableNode(nextNextNextGame, 0.0, 0.5, Some "a", 1, Some nextNextNode, None, 0)
        addNode pastGames nextNextNextNode [node; nextNode; nextNextNode]
        
        let shortGame = 49
        let shortNode = MutableNode(shortGame, 0.0, 0.5, Some "a", 1, Some node, None, 0)
        addNode pastGames shortNode [node]  
        
        let getMoves (x: int): (int * bool option * string * float) list =
            if x <> 49 then failwith "Invalid test setup"
            [ nextNextNextGame, None, "a", 0.0 ]
            
        expandNode getMoves pastGames shortNode |> ignore
            
        test <@
                 pastGames.[node.GameHashCode] = Set.empty &&
                    pastGames.[nextNode.GameHashCode] = (Set.ofList [node.GameHashCode]) &&
                        pastGames.[nextNextNode.GameHashCode] = (Set.ofList [node.GameHashCode; nextNode.GameHashCode]) &&
                            pastGames.[nextNextNextNode.GameHashCode] = (Set.ofList [node.GameHashCode; shortNode.GameHashCode]) &&
                                pastGames.[shortNode.GameHashCode] = (Set.ofList [node.GameHashCode]) &&
                                    pastGames.Count = 5
             @>