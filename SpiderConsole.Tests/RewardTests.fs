namespace SpiderConsole.Tests.RewardTests

open System
open NUnit.Framework
open SpiderConsole.Tests
open SpiderSolitare.Game
open SpiderSolitare.MonteCarloTreeSearch
open Swensen.Unquote
    
    
[<TestFixture>]
type RewardTests() = 
        
    [<Test>]
    [<Ignore("Only used to generate games")>]
    member __.``generate a bunch of games`` () =
        
        let r = Random(42)
        let deck = CardModule.deck OneSuit |> List.take 13
        let game = TestsSetupUtils.createGameSingleDeck r deck |> GameMover.unHideGame
        
        let rec loop depth (nodes: (int * MoveType * Game) list) (node: Game) =
            if depth >= 25 then
                nodes
            else
                let depth = depth + 1
                let nextGames =
                    let validMoves = GameMover.validMoves node
                    validMoves
                    |> List.map (fun m -> depth, m,  GameMover.playMove m node)
                    |> List.choose (fun (depth, m, gameResult) ->
                        match gameResult with
                        | Continue (g,_) ->
                            if nodes |> List.map (fun (_,_,g) -> g) |> List.contains g then None else Some (depth, m, gameResult) 
                        | _ ->  Some (depth, m, gameResult))
                    |> List.distinctBy (fun (_, _, gr) ->
                        match gr with 
                        | Continue (g,_) -> reward g
                        | Lost g -> reward g
                        | Won g -> reward g)                    
                        
                let nodes = nodes @ (nextGames |> List.map (fun (d, m, gr) ->
                    match gr with 
                    | Continue (g,_) -> (d, m, g)
                    | Lost (g) -> (d, m, g)
                    | Won g -> (d, m, g) ))
                
                let nodes = List.distinct nodes
                    
                nextGames
                |> List.collect (fun (depth, _, gameResult)  ->
                    match gameResult with
                    | Continue (g,_) -> loop depth nodes g
                    | _ -> nodes)
                
        printfn "Starting"
        loop 0 [] game
        |> List.sortBy (fun (depth, _, _) -> depth) 
        |> List.iter (fun (depth, m, _) ->
            match m with
            | Move c ->
                let (Card (n,s)) = c.Card
                printfn "MoveType.Move { From = %A;  To = %A; Card = Card (%d, %A) }, %d" c.From c.To n s depth
            | _ -> printfn "%A" m )
                    
            
//    [<Ignore("Only used to generate games")>]    
    [<Test>]
    member __.``generate a bunch of games from list of moves`` () =
        let moves =
            [|
                MoveType.Move { From = C1;  To = C4; Card = Card (8, S) }, 1
                MoveType.Move { From = C1;  To = C4; Card = Card (8, S) }, 1
                MoveType.Move { From = C1;  To = C4; Card = Card (8, S) }, 1
                MoveType.Move { From = C1;  To = C4; Card = Card (8, S) }, 1
                MoveType.Move { From = C1;  To = C4; Card = Card (8, S) }, 1
                MoveType.Move { From = C1;  To = C4; Card = Card (8, S) }, 1
                MoveType.Move { From = C1;  To = C4; Card = Card (8, S) }, 1
                MoveType.Move { From = C1;  To = C4; Card = Card (8, S) }, 1
                MoveType.Move { From = C1;  To = C4; Card = Card (8, S) }, 1
                MoveType.Move { From = C1;  To = C4; Card = Card (8, S) }, 1
                MoveType.Move { From = C1;  To = C4; Card = Card (8, S) }, 1
                MoveType.Move { From = C1;  To = C4; Card = Card (8, S) }, 1
                MoveType.Move { From = C1;  To = C4; Card = Card (8, S) }, 1
                MoveType.Move { From = C1;  To = C5; Card = Card (1, S) }, 2
                MoveType.Move { From = C1;  To = C5; Card = Card (1, S) }, 2
                MoveType.Move { From = C1;  To = C5; Card = Card (1, S) }, 2
                MoveType.Move { From = C1;  To = C5; Card = Card (1, S) }, 2
                MoveType.Move { From = C1;  To = C5; Card = Card (1, S) }, 2
                MoveType.Move { From = C1;  To = C5; Card = Card (1, S) }, 2
                MoveType.Move { From = C1;  To = C5; Card = Card (1, S) }, 2
                MoveType.Move { From = C1;  To = C5; Card = Card (1, S) }, 2
                MoveType.Move { From = C1;  To = C5; Card = Card (1, S) }, 2
                MoveType.Move { From = C1;  To = C5; Card = Card (1, S) }, 2
                MoveType.Move { From = C1;  To = C5; Card = Card (1, S) }, 2
                MoveType.Move { From = C1;  To = C5; Card = Card (1, S) }, 2
                MoveType.Move { From = C1;  To = C5; Card = Card (1, S) }, 2
                MoveType.Move { From = C1;  To = C6; Card = Card (5, S) }, 3
                MoveType.Move { From = C1;  To = C6; Card = Card (5, S) }, 3
                MoveType.Move { From = C1;  To = C6; Card = Card (5, S) }, 3
                MoveType.Move { From = C1;  To = C6; Card = Card (5, S) }, 3
                MoveType.Move { From = C1;  To = C6; Card = Card (5, S) }, 3
                MoveType.Move { From = C1;  To = C6; Card = Card (5, S) }, 3
                MoveType.Move { From = C1;  To = C6; Card = Card (5, S) }, 3
                MoveType.Move { From = C1;  To = C6; Card = Card (5, S) }, 3
                MoveType.Move { From = C1;  To = C6; Card = Card (5, S) }, 3
                MoveType.Move { From = C1;  To = C6; Card = Card (5, S) }, 3
                MoveType.Move { From = C1;  To = C6; Card = Card (5, S) }, 3
                MoveType.Move { From = C1;  To = C6; Card = Card (5, S) }, 3
                MoveType.Move { From = C1;  To = C6; Card = Card (5, S) }, 3
                MoveType.Move { From = C1;  To = C7; Card = Card (13, S) }, 4
                MoveType.Move { From = C1;  To = C7; Card = Card (13, S) }, 4
                MoveType.Move { From = C1;  To = C7; Card = Card (13, S) }, 4
                MoveType.Move { From = C1;  To = C7; Card = Card (13, S) }, 4
                MoveType.Move { From = C1;  To = C7; Card = Card (13, S) }, 4
                MoveType.Move { From = C1;  To = C7; Card = Card (13, S) }, 4
                MoveType.Move { From = C1;  To = C7; Card = Card (13, S) }, 4
                MoveType.Move { From = C1;  To = C7; Card = Card (13, S) }, 4
                MoveType.Move { From = C1;  To = C7; Card = Card (13, S) }, 4
                MoveType.Move { From = C1;  To = C7; Card = Card (13, S) }, 4
                MoveType.Move { From = C1;  To = C7; Card = Card (13, S) }, 4
                MoveType.Move { From = C1;  To = C7; Card = Card (13, S) }, 4
                MoveType.Move { From = C1;  To = C7; Card = Card (13, S) }, 4
                MoveType.Move { From = C1;  To = C2; Card = Card (3, S) }, 5
                MoveType.Move { From = C1;  To = C2; Card = Card (3, S) }, 5
                MoveType.Move { From = C1;  To = C2; Card = Card (3, S) }, 5
                MoveType.Move { From = C1;  To = C2; Card = Card (3, S) }, 5
                MoveType.Move { From = C1;  To = C2; Card = Card (3, S) }, 5
                MoveType.Move { From = C1;  To = C2; Card = Card (3, S) }, 5
                MoveType.Move { From = C1;  To = C2; Card = Card (3, S) }, 5
                MoveType.Move { From = C1;  To = C2; Card = Card (3, S) }, 5
                MoveType.Move { From = C1;  To = C2; Card = Card (3, S) }, 5
                MoveType.Move { From = C1;  To = C2; Card = Card (3, S) }, 5
                MoveType.Move { From = C1;  To = C2; Card = Card (3, S) }, 5
                MoveType.Move { From = C1;  To = C2; Card = Card (3, S) }, 5
                MoveType.Move { From = C1;  To = C2; Card = Card (3, S) }, 5
                MoveType.Move { From = C1;  To = C4; Card = Card (7, S) }, 6
                MoveType.Move { From = C1;  To = C4; Card = Card (7, S) }, 6
                MoveType.Move { From = C1;  To = C4; Card = Card (7, S) }, 6
                MoveType.Move { From = C1;  To = C4; Card = Card (7, S) }, 6
                MoveType.Move { From = C1;  To = C4; Card = Card (7, S) }, 6
                MoveType.Move { From = C1;  To = C4; Card = Card (7, S) }, 6
                MoveType.Move { From = C1;  To = C4; Card = Card (7, S) }, 6
                MoveType.Move { From = C1;  To = C4; Card = Card (7, S) }, 6
                MoveType.Move { From = C1;  To = C4; Card = Card (7, S) }, 6
                MoveType.Move { From = C1;  To = C4; Card = Card (7, S) }, 6
                MoveType.Move { From = C1;  To = C4; Card = Card (7, S) }, 6
                MoveType.Move { From = C1;  To = C4; Card = Card (7, S) }, 6
                MoveType.Move { From = C1;  To = C4; Card = Card (7, S) }, 6
                MoveType.Move { From = C2;  To = C1; Card = Card (3, S) }, 7
                MoveType.Move { From = C2;  To = C1; Card = Card (3, S) }, 7
                MoveType.Move { From = C2;  To = C1; Card = Card (3, S) }, 7
                MoveType.Move { From = C2;  To = C1; Card = Card (3, S) }, 7
                MoveType.Move { From = C2;  To = C1; Card = Card (3, S) }, 7
                MoveType.Move { From = C2;  To = C1; Card = Card (3, S) }, 7
                MoveType.Move { From = C2;  To = C1; Card = Card (3, S) }, 7
                MoveType.Move { From = C2;  To = C1; Card = Card (3, S) }, 7
                MoveType.Move { From = C2;  To = C1; Card = Card (3, S) }, 7
                MoveType.Move { From = C2;  To = C1; Card = Card (3, S) }, 7
                MoveType.Move { From = C2;  To = C1; Card = Card (3, S) }, 7
                MoveType.Move { From = C2;  To = C1; Card = Card (3, S) }, 7
                MoveType.Move { From = C2;  To = C1; Card = Card (3, S) }, 7
                MoveType.Move { From = C1;  To = C8; Card = Card (3, S) }, 8
                MoveType.Move { From = C1;  To = C8; Card = Card (3, S) }, 8
                MoveType.Move { From = C1;  To = C8; Card = Card (3, S) }, 8
                MoveType.Move { From = C1;  To = C8; Card = Card (3, S) }, 8
                MoveType.Move { From = C1;  To = C8; Card = Card (3, S) }, 8
                MoveType.Move { From = C1;  To = C8; Card = Card (3, S) }, 8
                MoveType.Move { From = C1;  To = C8; Card = Card (3, S) }, 8
                MoveType.Move { From = C1;  To = C8; Card = Card (3, S) }, 8
                MoveType.Move { From = C1;  To = C8; Card = Card (3, S) }, 8
                MoveType.Move { From = C1;  To = C8; Card = Card (3, S) }, 8
                MoveType.Move { From = C1;  To = C8; Card = Card (3, S) }, 8
                MoveType.Move { From = C1;  To = C8; Card = Card (3, S) }, 8
                MoveType.Move { From = C1;  To = C8; Card = Card (3, S) }, 8
                MoveType.Move { From = C2;  To = C1; Card = Card (4, S) }, 9
                MoveType.Move { From = C2;  To = C1; Card = Card (4, S) }, 9
                MoveType.Move { From = C2;  To = C1; Card = Card (4, S) }, 9
                MoveType.Move { From = C2;  To = C1; Card = Card (4, S) }, 9
                MoveType.Move { From = C2;  To = C1; Card = Card (4, S) }, 9
                MoveType.Move { From = C2;  To = C1; Card = Card (4, S) }, 9
                MoveType.Move { From = C2;  To = C1; Card = Card (4, S) }, 9
                MoveType.Move { From = C2;  To = C1; Card = Card (4, S) }, 9
                MoveType.Move { From = C2;  To = C1; Card = Card (4, S) }, 9
                MoveType.Move { From = C2;  To = C1; Card = Card (4, S) }, 9
                MoveType.Move { From = C2;  To = C1; Card = Card (4, S) }, 9
                MoveType.Move { From = C2;  To = C1; Card = Card (4, S) }, 9
                MoveType.Move { From = C2;  To = C1; Card = Card (4, S) }, 9
                MoveType.Move { From = C1;  To = C6; Card = Card (4, S) }, 10
                MoveType.Move { From = C1;  To = C6; Card = Card (4, S) }, 10
                MoveType.Move { From = C1;  To = C6; Card = Card (4, S) }, 10
                MoveType.Move { From = C1;  To = C6; Card = Card (4, S) }, 10
                MoveType.Move { From = C1;  To = C6; Card = Card (4, S) }, 10
                MoveType.Move { From = C1;  To = C6; Card = Card (4, S) }, 10
                MoveType.Move { From = C1;  To = C6; Card = Card (4, S) }, 10
                MoveType.Move { From = C1;  To = C6; Card = Card (4, S) }, 10
                MoveType.Move { From = C1;  To = C6; Card = Card (4, S) }, 10
                MoveType.Move { From = C1;  To = C6; Card = Card (4, S) }, 10
                MoveType.Move { From = C1;  To = C6; Card = Card (4, S) }, 10
                MoveType.Move { From = C1;  To = C6; Card = Card (4, S) }, 10
                MoveType.Move { From = C1;  To = C6; Card = Card (4, S) }, 10
                MoveType.Move { From = C2;  To = C1; Card = Card (9, S) }, 11
                MoveType.Move { From = C2;  To = C1; Card = Card (9, S) }, 11
                MoveType.Move { From = C2;  To = C1; Card = Card (9, S) }, 11
                MoveType.Move { From = C2;  To = C1; Card = Card (9, S) }, 11
                MoveType.Move { From = C2;  To = C1; Card = Card (9, S) }, 11
                MoveType.Move { From = C2;  To = C1; Card = Card (9, S) }, 11
                MoveType.Move { From = C2;  To = C1; Card = Card (9, S) }, 11
                MoveType.Move { From = C2;  To = C1; Card = Card (9, S) }, 11
                MoveType.Move { From = C2;  To = C1; Card = Card (9, S) }, 11
                MoveType.Move { From = C2;  To = C1; Card = Card (9, S) }, 11
                MoveType.Move { From = C2;  To = C1; Card = Card (9, S) }, 11
                MoveType.Move { From = C2;  To = C1; Card = Card (9, S) }, 11
                MoveType.Move { From = C2;  To = C1; Card = Card (9, S) }, 11
                MoveType.Move { From = C1;  To = C3; Card = Card (9, S) }, 12
                MoveType.Move { From = C1;  To = C3; Card = Card (9, S) }, 12
                MoveType.Move { From = C1;  To = C3; Card = Card (9, S) }, 12
                MoveType.Move { From = C1;  To = C3; Card = Card (9, S) }, 12
                MoveType.Move { From = C1;  To = C3; Card = Card (9, S) }, 12
                MoveType.Move { From = C1;  To = C3; Card = Card (9, S) }, 12
                MoveType.Move { From = C1;  To = C3; Card = Card (9, S) }, 12
                MoveType.Move { From = C1;  To = C3; Card = Card (9, S) }, 12
                MoveType.Move { From = C1;  To = C3; Card = Card (9, S) }, 12
                MoveType.Move { From = C1;  To = C3; Card = Card (9, S) }, 12
                MoveType.Move { From = C1;  To = C3; Card = Card (9, S) }, 12
                MoveType.Move { From = C1;  To = C3; Card = Card (9, S) }, 12
                MoveType.Move { From = C1;  To = C3; Card = Card (9, S) }, 12
                MoveType.Move { From = C2;  To = C1; Card = Card (6, S) }, 13
                MoveType.Move { From = C2;  To = C1; Card = Card (6, S) }, 13
                MoveType.Move { From = C2;  To = C1; Card = Card (6, S) }, 13
                MoveType.Move { From = C2;  To = C1; Card = Card (6, S) }, 13
                MoveType.Move { From = C2;  To = C1; Card = Card (6, S) }, 13
                MoveType.Move { From = C2;  To = C1; Card = Card (6, S) }, 13
                MoveType.Move { From = C2;  To = C1; Card = Card (6, S) }, 13
                MoveType.Move { From = C2;  To = C1; Card = Card (6, S) }, 13
                MoveType.Move { From = C2;  To = C1; Card = Card (6, S) }, 13
                MoveType.Move { From = C2;  To = C1; Card = Card (6, S) }, 13
                MoveType.Move { From = C2;  To = C1; Card = Card (6, S) }, 13
                MoveType.Move { From = C2;  To = C1; Card = Card (6, S) }, 13
                MoveType.Move { From = C2;  To = C1; Card = Card (6, S) }, 13
                MoveType.Move { From = C1;  To = C4; Card = Card (6, S) }, 14
                MoveType.Move { From = C1;  To = C4; Card = Card (6, S) }, 14
                MoveType.Move { From = C1;  To = C4; Card = Card (6, S) }, 14
                MoveType.Move { From = C1;  To = C4; Card = Card (6, S) }, 14
                MoveType.Move { From = C1;  To = C4; Card = Card (6, S) }, 14
                MoveType.Move { From = C1;  To = C4; Card = Card (6, S) }, 14
                MoveType.Move { From = C1;  To = C4; Card = Card (6, S) }, 14
                MoveType.Move { From = C1;  To = C4; Card = Card (6, S) }, 14
                MoveType.Move { From = C1;  To = C4; Card = Card (6, S) }, 14
                MoveType.Move { From = C1;  To = C4; Card = Card (6, S) }, 14
                MoveType.Move { From = C1;  To = C4; Card = Card (6, S) }, 14
                MoveType.Move { From = C1;  To = C4; Card = Card (6, S) }, 14
                MoveType.Move { From = C1;  To = C4; Card = Card (6, S) }, 14
                MoveType.Move { From = C2;  To = C1; Card = Card (11, S) }, 15
                MoveType.Move { From = C2;  To = C1; Card = Card (11, S) }, 15
                MoveType.Move { From = C2;  To = C1; Card = Card (11, S) }, 15
                MoveType.Move { From = C2;  To = C1; Card = Card (11, S) }, 15
                MoveType.Move { From = C2;  To = C1; Card = Card (11, S) }, 15
                MoveType.Move { From = C2;  To = C1; Card = Card (11, S) }, 15
                MoveType.Move { From = C2;  To = C1; Card = Card (11, S) }, 15
                MoveType.Move { From = C2;  To = C1; Card = Card (11, S) }, 15
                MoveType.Move { From = C2;  To = C1; Card = Card (11, S) }, 15
                MoveType.Move { From = C2;  To = C1; Card = Card (11, S) }, 15
                MoveType.Move { From = C2;  To = C1; Card = Card (11, S) }, 15
                MoveType.Move { From = C2;  To = C1; Card = Card (11, S) }, 15
                MoveType.Move { From = C2;  To = C1; Card = Card (11, S) }, 15
                MoveType.Move { From = C1;  To = C9; Card = Card (11, S) }, 16
                MoveType.Move { From = C1;  To = C9; Card = Card (11, S) }, 16
                MoveType.Move { From = C1;  To = C9; Card = Card (11, S) }, 16
                MoveType.Move { From = C1;  To = C9; Card = Card (11, S) }, 16
                MoveType.Move { From = C1;  To = C9; Card = Card (11, S) }, 16
                MoveType.Move { From = C1;  To = C9; Card = Card (11, S) }, 16
                MoveType.Move { From = C1;  To = C9; Card = Card (11, S) }, 16
                MoveType.Move { From = C1;  To = C9; Card = Card (11, S) }, 16
                MoveType.Move { From = C1;  To = C9; Card = Card (11, S) }, 16
                MoveType.Move { From = C1;  To = C9; Card = Card (11, S) }, 16
                MoveType.Move { From = C1;  To = C9; Card = Card (11, S) }, 16
                MoveType.Move { From = C1;  To = C9; Card = Card (11, S) }, 16
                MoveType.Move { From = C1;  To = C9; Card = Card (11, S) }, 16
                MoveType.Move { From = C2;  To = C1; Card = Card (12, S) }, 17
                MoveType.Move { From = C2;  To = C1; Card = Card (12, S) }, 17
                MoveType.Move { From = C2;  To = C1; Card = Card (12, S) }, 17
                MoveType.Move { From = C2;  To = C1; Card = Card (12, S) }, 17
                MoveType.Move { From = C2;  To = C1; Card = Card (12, S) }, 17
                MoveType.Move { From = C2;  To = C1; Card = Card (12, S) }, 17
                MoveType.Move { From = C2;  To = C1; Card = Card (12, S) }, 17
                MoveType.Move { From = C2;  To = C1; Card = Card (12, S) }, 17
                MoveType.Move { From = C2;  To = C1; Card = Card (12, S) }, 17
                MoveType.Move { From = C2;  To = C1; Card = Card (12, S) }, 17
                MoveType.Move { From = C2;  To = C1; Card = Card (12, S) }, 17
                MoveType.Move { From = C2;  To = C1; Card = Card (12, S) }, 17
                MoveType.Move { From = C2;  To = C1; Card = Card (12, S) }, 17
                MoveType.Move { From = C1;  To = C7; Card = Card (12, S) }, 18
                MoveType.Move { From = C3;  To = C10; Card = Card (9, S) }, 18
                MoveType.Move { From = C1;  To = C7; Card = Card (12, S) }, 18
                MoveType.Move { From = C3;  To = C10; Card = Card (9, S) }, 18
                MoveType.Move { From = C1;  To = C7; Card = Card (12, S) }, 18
                MoveType.Move { From = C3;  To = C10; Card = Card (9, S) }, 18
                MoveType.Move { From = C1;  To = C7; Card = Card (12, S) }, 18
                MoveType.Move { From = C3;  To = C10; Card = Card (9, S) }, 18
                MoveType.Move { From = C1;  To = C7; Card = Card (12, S) }, 18
                MoveType.Move { From = C3;  To = C10; Card = Card (9, S) }, 18
                MoveType.Move { From = C1;  To = C7; Card = Card (12, S) }, 18
                MoveType.Move { From = C3;  To = C10; Card = Card (9, S) }, 18
                MoveType.Move { From = C1;  To = C7; Card = Card (12, S) }, 18
                MoveType.Move { From = C3;  To = C10; Card = Card (9, S) }, 18
                MoveType.Move { From = C1;  To = C7; Card = Card (12, S) }, 18
                MoveType.Move { From = C3;  To = C10; Card = Card (9, S) }, 18
                MoveType.Move { From = C1;  To = C7; Card = Card (12, S) }, 18
                MoveType.Move { From = C3;  To = C10; Card = Card (9, S) }, 18
                MoveType.Move { From = C1;  To = C7; Card = Card (12, S) }, 18
                MoveType.Move { From = C3;  To = C10; Card = Card (9, S) }, 18
                MoveType.Move { From = C1;  To = C7; Card = Card (12, S) }, 18
                MoveType.Move { From = C3;  To = C10; Card = Card (9, S) }, 18
                MoveType.Move { From = C1;  To = C7; Card = Card (12, S) }, 18
                MoveType.Move { From = C3;  To = C10; Card = Card (9, S) }, 18
                MoveType.Move { From = C1;  To = C7; Card = Card (12, S) }, 18
                MoveType.Move { From = C3;  To = C10; Card = Card (9, S) }, 18
                MoveType.Move { From = C2;  To = C1; Card = Card (2, S) }, 19
                MoveType.Move { From = C1;  To = C7; Card = Card (12, S) }, 19
                MoveType.Move { From = C1;  To = C7; Card = Card (12, S) }, 19
                MoveType.Move { From = C1;  To = C7; Card = Card (12, S) }, 19
                MoveType.Move { From = C1;  To = C7; Card = Card (12, S) }, 19
                MoveType.Move { From = C1;  To = C7; Card = Card (12, S) }, 19
                MoveType.Move { From = C1;  To = C7; Card = Card (12, S) }, 19
                MoveType.Move { From = C1;  To = C7; Card = Card (12, S) }, 19
                MoveType.Move { From = C1;  To = C7; Card = Card (12, S) }, 19
                MoveType.Move { From = C1;  To = C7; Card = Card (12, S) }, 19
                MoveType.Move { From = C1;  To = C7; Card = Card (12, S) }, 19
                MoveType.Move { From = C1;  To = C7; Card = Card (12, S) }, 19
                MoveType.Move { From = C1;  To = C7; Card = Card (12, S) }, 19
                MoveType.Move { From = C1;  To = C8; Card = Card (2, S) }, 20
                MoveType.Move { From = C2;  To = C1; Card = Card (2, S) }, 20
                MoveType.Move { From = C4;  To = C1; Card = Card (6, S) }, 20
                MoveType.Move { From = C2;  To = C1; Card = Card (2, S) }, 20
                MoveType.Move { From = C4;  To = C1; Card = Card (6, S) }, 20
                MoveType.Move { From = C2;  To = C1; Card = Card (2, S) }, 20
                MoveType.Move { From = C4;  To = C1; Card = Card (6, S) }, 20
                MoveType.Move { From = C2;  To = C1; Card = Card (2, S) }, 20
                MoveType.Move { From = C4;  To = C1; Card = Card (6, S) }, 20
                MoveType.Move { From = C2;  To = C1; Card = Card (2, S) }, 20
                MoveType.Move { From = C4;  To = C1; Card = Card (6, S) }, 20
                MoveType.Move { From = C2;  To = C1; Card = Card (2, S) }, 20
                MoveType.Move { From = C4;  To = C1; Card = Card (6, S) }, 20
                MoveType.Move { From = C2;  To = C1; Card = Card (2, S) }, 20
                MoveType.Move { From = C4;  To = C1; Card = Card (6, S) }, 20
                MoveType.Move { From = C2;  To = C1; Card = Card (2, S) }, 20
                MoveType.Move { From = C4;  To = C1; Card = Card (6, S) }, 20
                MoveType.Move { From = C2;  To = C1; Card = Card (2, S) }, 20
                MoveType.Move { From = C4;  To = C1; Card = Card (6, S) }, 20
                MoveType.Move { From = C2;  To = C1; Card = Card (2, S) }, 20
                MoveType.Move { From = C4;  To = C1; Card = Card (6, S) }, 20
                MoveType.Move { From = C2;  To = C1; Card = Card (2, S) }, 20
                MoveType.Move { From = C4;  To = C1; Card = Card (6, S) }, 20
                MoveType.Move { From = C2;  To = C1; Card = Card (2, S) }, 20
                MoveType.Move { From = C4;  To = C1; Card = Card (6, S) }, 20
                MoveType.Move { From = C3;  To = C1; Card = Card (9, S) }, 21
                MoveType.Move { From = C1;  To = C8; Card = Card (2, S) }, 21
                MoveType.Move { From = C4;  To = C2; Card = Card (6, S) }, 21
                MoveType.Move { From = C1;  To = C8; Card = Card (2, S) }, 21
                MoveType.Move { From = C4;  To = C2; Card = Card (6, S) }, 21
                MoveType.Move { From = C1;  To = C8; Card = Card (2, S) }, 21
                MoveType.Move { From = C4;  To = C2; Card = Card (6, S) }, 21
                MoveType.Move { From = C1;  To = C8; Card = Card (2, S) }, 21
                MoveType.Move { From = C4;  To = C2; Card = Card (6, S) }, 21
                MoveType.Move { From = C1;  To = C8; Card = Card (2, S) }, 21
                MoveType.Move { From = C4;  To = C2; Card = Card (6, S) }, 21
                MoveType.Move { From = C1;  To = C8; Card = Card (2, S) }, 21
                MoveType.Move { From = C4;  To = C2; Card = Card (6, S) }, 21
                MoveType.Move { From = C2;  To = C8; Card = Card (2, S) }, 21
                MoveType.Move { From = C2;  To = C8; Card = Card (2, S) }, 21
                MoveType.Move { From = C2;  To = C8; Card = Card (2, S) }, 21
                MoveType.Move { From = C2;  To = C8; Card = Card (2, S) }, 21
                MoveType.Move { From = C2;  To = C8; Card = Card (2, S) }, 21
                MoveType.Move { From = C2;  To = C8; Card = Card (2, S) }, 21
                MoveType.Move { From = C1;  To = C2; Card = Card (9, S) }, 22
                MoveType.Move { From = C3;  To = C1; Card = Card (10, S) }, 22
                MoveType.Move { From = C1;  To = C8; Card = Card (2, S) }, 22
                MoveType.Move { From = C1;  To = C8; Card = Card (2, S) }, 22
                MoveType.Move { From = C1;  To = C8; Card = Card (2, S) }, 22
                MoveType.Move { From = C1;  To = C8; Card = Card (2, S) }, 22
                MoveType.Move { From = C1;  To = C8; Card = Card (2, S) }, 22
                MoveType.Move { From = C1;  To = C2; Card = Card (6, S) }, 22
                MoveType.Move { From = C4;  To = C2; Card = Card (7, S) }, 22
                MoveType.Move { From = C1;  To = C2; Card = Card (6, S) }, 22
                MoveType.Move { From = C4;  To = C2; Card = Card (7, S) }, 22
                MoveType.Move { From = C1;  To = C2; Card = Card (6, S) }, 22
                MoveType.Move { From = C4;  To = C2; Card = Card (7, S) }, 22
                MoveType.Move { From = C1;  To = C2; Card = Card (6, S) }, 22
                MoveType.Move { From = C4;  To = C2; Card = Card (7, S) }, 22
                MoveType.Move { From = C1;  To = C2; Card = Card (6, S) }, 22
                MoveType.Move { From = C4;  To = C2; Card = Card (7, S) }, 22
                MoveType.Move { From = C1;  To = C2; Card = Card (6, S) }, 22
                MoveType.Move { From = C4;  To = C2; Card = Card (7, S) }, 22
                MoveType.Move { From = C2;  To = C10; Card = Card (9, S) }, 23
                MoveType.Move { From = C1;  To = C2; Card = Card (10, S) }, 23
                MoveType.Move { From = C2;  To = C1; Card = Card (6, S) }, 23
                MoveType.Move { From = C4;  To = C1; Card = Card (7, S) }, 23
                MoveType.Move { From = C2;  To = C1; Card = Card (6, S) }, 23
                MoveType.Move { From = C4;  To = C1; Card = Card (7, S) }, 23
                MoveType.Move { From = C2;  To = C1; Card = Card (6, S) }, 23
                MoveType.Move { From = C4;  To = C1; Card = Card (7, S) }, 23
                MoveType.Move { From = C2;  To = C1; Card = Card (6, S) }, 23
                MoveType.Move { From = C4;  To = C1; Card = Card (7, S) }, 23
                MoveType.Move { From = C2;  To = C1; Card = Card (6, S) }, 23
                MoveType.Move { From = C4;  To = C1; Card = Card (7, S) }, 23
                MoveType.Move { From = C2;  To = C4; Card = Card (6, S) }, 23
                MoveType.Move { From = C4;  To = C1; Card = Card (7, S) }, 23
                MoveType.Move { From = C2;  To = C4; Card = Card (6, S) }, 23
                MoveType.Move { From = C4;  To = C1; Card = Card (7, S) }, 23
                MoveType.Move { From = C2;  To = C4; Card = Card (6, S) }, 23
                MoveType.Move { From = C4;  To = C1; Card = Card (7, S) }, 23
                MoveType.Move { From = C1;  To = C2; Card = Card (6, S) }, 23
                MoveType.Move { From = C1;  To = C2; Card = Card (6, S) }, 23
                MoveType.Move { From = C1;  To = C2; Card = Card (6, S) }, 23
                MoveType.Move { From = C3;  To = C1; Card = Card (10, S) }, 24
                MoveType.Move { From = C2;  To = C9; Card = Card (10, S) }, 24
                MoveType.Move { From = C3;  To = C2; Card = Card (10, S) }, 24
                MoveType.Move { From = C4;  To = C2; Card = Card (7, S) }, 24
                MoveType.Move { From = C3;  To = C2; Card = Card (10, S) }, 24
                MoveType.Move { From = C4;  To = C2; Card = Card (7, S) }, 24
                MoveType.Move { From = C3;  To = C2; Card = Card (10, S) }, 24
                MoveType.Move { From = C4;  To = C2; Card = Card (7, S) }, 24
                MoveType.Move { From = C2;  To = C1; Card = Card (6, S) }, 24
                MoveType.Move { From = C2;  To = C1; Card = Card (6, S) }, 24
                MoveType.Move { From = C3;  To = C1; Card = Card (10, S) }, 24
                MoveType.Move { From = C2;  To = C1; Card = Card (6, S) }, 24
                MoveType.Move { From = C2;  To = C1; Card = Card (6, S) }, 24
                MoveType.Move { From = C2;  To = C1; Card = Card (7, S) }, 24
                MoveType.Move { From = C6;  To = C1; Card = Card (4, S) }, 24
                MoveType.Move { From = C2;  To = C1; Card = Card (7, S) }, 24
                MoveType.Move { From = C6;  To = C1; Card = Card (4, S) }, 24
                MoveType.Move { From = C2;  To = C1; Card = Card (7, S) }, 24
                MoveType.Move { From = C6;  To = C1; Card = Card (4, S) }, 24
                MoveType.Move { From = C1;  To = C2; Card = Card (10, S) }, 25
                MoveType.Move { From = C4;  To = C1; Card = Card (6, S) }, 25
                MoveType.Move { From = C1;  To = C3; Card = Card (6, S) }, 25
                MoveType.Move { From = C4;  To = C3; Card = Card (7, S) }, 25
                MoveType.Move { From = C1;  To = C3; Card = Card (6, S) }, 25
                MoveType.Move { From = C4;  To = C3; Card = Card (7, S) }, 25
                MoveType.Move { From = C1;  To = C2; Card = Card (6, S) }, 25
                MoveType.Move { From = C1;  To = C2; Card = Card (7, S) }, 25
                MoveType.Move { From = C6;  To = C2; Card = Card (4, S) }, 25
                MoveType.Move { From = C1;  To = C2; Card = Card (7, S) }, 25
                MoveType.Move { From = C6;  To = C2; Card = Card (4, S) }, 25
                MoveType.Move { From = C1;  To = C2; Card = Card (10, S) }, 25
                MoveType.Move { From = C1;  To = C2; Card = Card (7, S) }, 25
                MoveType.Move { From = C6;  To = C2; Card = Card (4, S) }, 25
                MoveType.Move { From = C1;  To = C2; Card = Card (7, S) }, 25
                MoveType.Move { From = C6;  To = C2; Card = Card (4, S) }, 25
                MoveType.Move { From = C1;  To = C2; Card = Card (6, S) }, 25
                MoveType.Move { From = C1;  To = C4; Card = Card (7, S) }, 25
                MoveType.Move { From = C1;  To = C2; Card = Card (6, S) }, 25
                MoveType.Move { From = C1;  To = C4; Card = Card (7, S) }, 25
                MoveType.Move { From = C2;  To = C4; Card = Card (7, S) }, 25
            |]
            |> List.ofArray
            |> List.map fst
    
        let r = Random(42)
        let deck = CardModule.deck OneSuit |> List.take 13
        let game = TestsSetupUtils.createGameSingleDeck r deck |> GameMover.unHideGame
        
        let rec loop acc gameMoves = 
            gameMoves
            |> List.collect (fun (pastMoves, game) ->
                GameMover.validMoves game
                |> List.filter (fun x -> moves |> List.contains x)
                |> List.map (fun m ->
                    let pastMoves =
                        match pastMoves with
                        | None -> [m]
                        | Some ms ->  m :: ms
                        
                    match GameMover.playMove m game with
                    | Continue (g,_) -> pastMoves,g 
                    | Lost g -> pastMoves,g
                    | Won g -> pastMoves,g ) )
            |> List.distinctBy (fun (_,x) -> reward x)
            |> List.filter (fun (_,g) -> acc |> List.map snd |> List.contains g |> not)
            |> function
            | [] -> acc
            | xs -> xs |> List.map (fun (m,g) -> Some m, g) |> loop (acc @ gameMoves)
            
        
        loop [] [None, game]
        |> List.iter (fun (moves, game) ->
            if reward game < 0.05 then
                printfn "%A" game
                printfn "R:%f" <| reward game
                moves |> Option.iter (List.rev >> List.iter (fun m ->
                    match m with
                    | Move c ->
                        let (Card (n,s)) = c.Card
                        printfn "MoveType.Move { From = %A;  To = %A; Card = Card (%d, %A) }" c.From c.To n s
                    | _ -> printfn "%A" m))
                printfn ""
                
                
            if reward game > 0.6 then
                printfn "%A" game
                printfn "R:%f" <| reward game
                moves |> Option.iter (List.rev >> List.iter (fun m ->
                    match m with
                    | Move c ->
                        let (Card (n,s)) = c.Card
                        printfn "MoveType.Move { From = %A;  To = %A; Card = Card (%d, %A) }" c.From c.To n s
                    | _ -> printfn "%A" m))
                printfn ""
                
                )
                
                
                
    [<Test>]
    member __.``score should be low for this game`` () =
        
        let r = Random(42)
        let deck = CardModule.deck OneSuit |> List.take 13
        let game = TestsSetupUtils.createGameSingleDeck r deck |> GameMover.unHideGame
        
        let playMoves initGame moves =
            moves
            |> Array.fold (fun game move ->
                match GameMover.playMove move game with
                | Continue (g,_) -> g
                | Lost g -> g
                | Won g -> g ) initGame
        
        let lowScoreGame =
            [|
                MoveType.Move { From = C4;  To = C3; Card = Card (7, S) }
                MoveType.Move { From = C3;  To = C1; Card = Card (10, S) }
                MoveType.Move { From = C1;  To = C6; Card = Card (4, S) }
                MoveType.Move { From = C1;  To = C8; Card = Card (3, S) }
                MoveType.Move { From = C2;  To = C1; Card = Card (4, S) }
                MoveType.Move { From = C1;  To = C4; Card = Card (7, S) }
                MoveType.Move { From = C1;  To = C2; Card = Card (3, S) }
                MoveType.Move { From = C1;  To = C7; Card = Card (13, S) }
                MoveType.Move { From = C1;  To = C6; Card = Card (5, S) }
                MoveType.Move { From = C1;  To = C5; Card = Card (1, S) }
                MoveType.Move { From = C1;  To = C4; Card = Card (8, S) }
            |] |> playMoves game
            
            
        let highScoreGame =
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
            |]  |> playMoves game
            
        test <@ reward lowScoreGame < reward highScoreGame @>
        
    [<Test>]
    member __.``game can be won`` () =
        
        let r = Random(42)
        let deck = CardModule.deck OneSuit |> List.take 13
        let game = TestsSetupUtils.createGameSingleDeck r deck |> GameMover.unHideGame
        
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
            
        let wonGame =
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
                MoveType.Move { From = C2;  To = C7; Card = Card (4, S) }
                MoveType.Move { From = C8;  To = C7; Card = Card (3, S) }
                MoveType.Move { From = C5;  To = C7; Card = Card (1, S) }
            |]  |> playMoves game
            
        test <@ SpiderSolitare.MonteCarloTreeSearch.winningNodeReward = reward wonGame @>
        
        
    [<Test>]
    member __.``longer streak scores higher than many short streaks`` () =
        
        let r = Random(42)
        let deck = CardModule.deck OneSuit |> List.take 13
        let game = TestsSetupUtils.createGameSingleDeck r deck |> GameMover.unHideGame
        
        let playMoves initGame moves =
            moves
            |> Array.fold (fun game move ->
                match GameMover.playMove move game with
                | Continue (g,_) -> g
                | Lost g -> g
                | Won g -> g ) initGame
        
        let longerStreak =
            [|
                MoveType.Move { From = C1;  To = C4; Card = Card (8, S) }
                MoveType.Move { From = C1;  To = C5; Card = Card (1, S) }
                MoveType.Move { From = C2;  To = C1; Card = Card (4, S) }
                MoveType.Move { From = C1;  To = C6; Card = Card (5, S) }
                MoveType.Move { From = C1;  To = C7; Card = Card (13, S) }
                MoveType.Move { From = C1;  To = C8; Card = Card (3, S) }
                MoveType.Move { From = C2;  To = C10; Card = Card (9, S) }
                MoveType.Move { From = C2;  To = C1; Card = Card (6, S) }
                MoveType.Move { From = C1;  To = C4; Card = Card (7, S) }
                MoveType.Move { From = C2;  To = C1; Card = Card (11, S) }
            |] |> playMoves game
            
            
        let shorterStreak =
            [|
                MoveType.Move { From = C1;  To = C4; Card = Card (8, S) }
                MoveType.Move { From = C1;  To = C5; Card = Card (1, S) }
                MoveType.Move { From = C2;  To = C1; Card = Card (4, S) }
                MoveType.Move { From = C1;  To = C6; Card = Card (5, S) }
                MoveType.Move { From = C1;  To = C7; Card = Card (13, S) }
                MoveType.Move { From = C1;  To = C8; Card = Card (3, S) }
                MoveType.Move { From = C2;  To = C10; Card = Card (9, S) }
                MoveType.Move { From = C2;  To = C1; Card = Card (6, S) }
                MoveType.Move { From = C1;  To = C4; Card = Card (7, S) }
                MoveType.Move { From = C4;  To = C1; Card = Card (6, S) }
            |]  |> playMoves game
            
        test <@ reward longerStreak > reward shorterStreak @>
        
        
    [<Test>]
    member __.``a run is only scored if it is not blocked`` () =
        
        let r = Random(42)
        let deck = CardModule.deck OneSuit |> List.take 13
        let game = TestsSetupUtils.createGameSingleDeck r deck |> GameMover.unHideGame
        
        let playMoves initGame moves =
            moves
            |> Array.fold (fun game move ->
                match GameMover.playMove move game with
                | Continue (g,_) -> g
                | Lost g -> g
                | Won g -> g ) initGame
        
        let blockedRun =
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
                MoveType.Move { From = C3;  To = C1; Card = Card (10, S) }
                MoveType.Move { From = C4;  To = C3; Card = Card (7, S) }
            |] |> playMoves game
            
            
        let openRun =
            [|
                MoveType.Move { From = C1;  To = C8; Card = Card (8, S) }
                MoveType.Move { From = C1;  To = C5; Card = Card (1, S) }
                MoveType.Move { From = C1;  To = C6; Card = Card (5, S) }
                MoveType.Move { From = C1;  To = C7; Card = Card (13, S) }
                MoveType.Move { From = C1;  To = C9; Card = Card (3, S) }
                MoveType.Move { From = C1;  To = C4; Card = Card (7, S) }
                MoveType.Move { From = C2;  To = C1; Card = Card (4, S) }
                MoveType.Move { From = C1;  To = C6; Card = Card (4, S) }
                MoveType.Move { From = C2;  To = C10; Card = Card (9, S) }
                MoveType.Move { From = C1;  To = C3; Card = Card (9, S) }
                MoveType.Move { From = C2;  To = C1; Card = Card (6, S) }
            |]  |> playMoves game
            
        test <@ reward blockedRun < reward openRun @>