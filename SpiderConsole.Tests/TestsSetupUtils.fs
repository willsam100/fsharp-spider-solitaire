module SpiderConsole.Tests.TestsSetupUtils

open SpiderSolitare.Game
open SpiderSolitare.MonteCarloTreeSearch
open System
open System.Collections.Generic

let toMap kvps =
    kvps
    |> Seq.map (|KeyValue|)
    |> Map.ofSeq

let deck = CardModule.deck OneSuit
    
let getContinueGame = function
    | Continue (g, _) -> g
    | x -> failwithf "Expected game to be Continue but got: %A" x

let createGame r = GameMover.startGame deck r |> getContinueGame
        
let rootWithPastGame (pastGames: Dictionary<int, int Set>) =
    let node = MutableNode(42, 0.0, 0.01,  None, 0, None, None, 0)
    pastGames.[node.GameHashCode] <- Set.empty
    node
    
let addNode (pastGames: Dictionary<int, int Set>) (nextNode:MutableNode<'a, 'b>) (parents: MutableNode<'a, 'b> list) =
    pastGames.[nextNode.GameHashCode] <- parents |> List.map (fun x -> x.GameHashCode) |> Set.ofList
    
let updateScore (gameToMetrics: Dictionary<int, float * float>) (nextNode:MutableNode<'a, 'b>) reward =
    gameToMetrics.[nextNode.GameHashCode] <- reward

let createGameSingleDeck rand deck = 

    let createTableau count cards = Tableau.create (List.take count cards)

    let constructTableaus (game, cards) column = 
        match column with 
        | C1 | C2 | C3 | C4 -> Math.Min (6, List.length cards)
        | C5 | C6 | C7 | C8 | C9 | C10 -> Math.Min (5, List.length cards)
        |> (fun count -> 
            game |> Game.updateColumn column (createTableau count cards), cards |> List.skip count)

    let constructStock (game, cards) = 
        {game with Stock = cards}

    let cards = 
        deck
        |> LogicUtils.shuffleList rand

    Coord.allColumns |> Seq.fold (constructTableaus) (Game.emptyGame, cards) |> constructStock
    
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
    
let playSingleMove game = MoveType.Move >> Array.singleton >> playMoves game >> (fun x -> x.GetHashCode())
    
let movesToStackToSeven =
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
    |]
    
let mockkBrainServer =
    { new IBransMover with
            member this.GetBestMove _ = []
            member this.GetValue _ = 0.0
            member this.GetMoves _ = []
            member this.Flush() = ()
         }
    
    
let playGame gameToMetrics iterationCount totalCount game search =
    
    let rec loop count (root: MutableNode<Game, MoveType>) =
        search(iterationCount, root)

        match root.Children |> List.maxBy (getMetrics gameToMetrics >> snd) |> Some with 
        | None ->
            let movesMade = float (totalCount - count)
            false, movesMade

        | Some nMove -> 
            nMove.Parent <- None
            match nMove.TerminalValue with 
            | None -> 
                if count <= 0 then
                    let movesMade = float (totalCount - count)
                    false, movesMade
                else
                    printfn "%A" nMove.Game
                    loop (count - 1) nMove

            | Some didWin ->
                let movesMade = float (totalCount - count)
                if didWin then true, movesMade
                else false, movesMade
                
    loop totalCount game
    
let flattenTree (root:MutableNode<'a, 'b>) =
//    let getDetails (x: MutableNode<'a, 'b>) =
//            {| Depth = x.Depth; Game = x.Game; GameHashCode = x.GameHashCode; Prob = x.Prob; Reward = x.Reward; TerminalValue = x.TerminalValue; |}
            
    let getDetails (x: MutableNode<'a, 'b>) =
            x.Depth, x.GameHashCode, x.Prob, x.Reward, x.TerminalValue   
    
    let rec collect acc (nodes:MutableNode<'a, 'b> list) =
        let details = nodes |> List.map getDetails
        let nextNodes = nodes |> List.collect (fun x -> x.Children)
        match nextNodes with
        | [] -> details @ acc
        | _ -> collect (details @ acc) nextNodes
        
    getDetails root :: collect [] root.Children
        
    
            
        
        
    
    