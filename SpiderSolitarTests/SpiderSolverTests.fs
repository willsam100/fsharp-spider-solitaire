namespace SpiderSolverTests
open System

open SpiderSolitare.Game
open SpiderSolitare.Operations.App
open SpiderSolitare.Game.Card
open SpiderSolitare.Game.GameMover
open SpiderSolitare.Solver

open NUnit.Framework
open FsCheck.NUnit
open FsCheck
open FsUnit
open Swensen.Unquote

module TableauUtils = 
    let isHiddenEmpty t = 
        t.Hidden = []

module MoveTypeUtils = 
    let isFlip = function  
        | Flip _ -> true
        | _ -> false

module GameUtils = 

    let hasCardToBeFliped g = 
        g |> Game.getAllTabs |> List.exists (fun t -> t.Visible = [])

    let tabsAreNotEmpty g = 
        g |> Game.getAllTabs |> List.forall (fun x -> Tableau.length x > 0)

module SpiderTree = 

    type PropertyFiftyAttribute() = 
        inherit PropertyAttribute(Verbose=true, MaxTest = 50 )

    [<PropertyFiftyAttribute()>]
    let ``Create tree with game adds game to tree`` game moves =
        test <@
                SpiderTree.createTree game moves 0
                |> (function
                    | LeafNode _ -> false
                    | InternalNode (g,_) -> g.Game = game )
                @>

    [<PropertyFiftyAttribute()>]
    let ``Adding game to spiderTree depens the tree by one`` game moves leaf =

        let root = SpiderTree.createTree game moves 0

        (game <> leaf.Game) ==> 
        lazy (test <@ 
                    root 
                    |> SpiderTree.addMovePlayedGreedy game leaf
                    |> SpiderTree.depth = 2
                @>)

    [<PropertyFiftyAttribute()>]
    let ``Adding two games to spiderTree depens the tree by one`` game moves leafOne leafTwo =

        let root = SpiderTree.createTree game moves 0

        (game <> leafOne.Game && game <> leafTwo.Game && leafOne.Game <> leafTwo.Game) ==> 
            lazy (test <@ 
                         root 
                         |> SpiderTree.addMovePlayedGreedy game leafOne 
                         |> SpiderTree.addMovePlayedGreedy game leafTwo
                         |> SpiderTree.depth = 2
                    @>)

    [<PropertyFiftyAttribute()>]
    let ``Adding a game to a tree means the game is an extra game in the tree`` game moves leaf =

        let root = SpiderTree.createTree game moves 0
        game <> leaf.Game ==> 
            lazy (test <@ 
                            root 
                            |> SpiderTree.addMovePlayedGreedy game leaf
                            |> SpiderTree.toList
                            |> Set.ofList
                            |> Set.count = 2
                            @>)

    [<PropertyFiftyAttribute()>]
    let ``Adding two games to spiderTree depens the tree by two`` 
        game moves movePlayedOne movePlayedTwo =

        let root = SpiderTree.createTree game moves 0

        (game <> movePlayedOne.Game && game <> movePlayedTwo.Game 
            && movePlayedOne.Game <> movePlayedTwo.Game) ==> 
            lazy (test <@ 
                        root 
                        |> SpiderTree.addMovePlayedGreedy game movePlayedOne
                        |> SpiderTree.addMovePlayedGreedy movePlayedOne.Game movePlayedTwo 
                        |> SpiderTree.depth = 3
                    @>)

    [<PropertyFiftyAttribute()>]
    let ``Can print a tree`` game moves = 
        test <@ 
                SpiderTree.createTree game moves 0
                |> SpiderTree.toString
                |> String.length > 0
                @>

    [<PropertyFiftyAttribute()>]
    let ``Can get the right node for a tree`` game moves movePlayed = 
        (game <> movePlayed.Game) ==> 
            lazy (test <@ 
                          SpiderTree.createTree game moves 0
                          |> SpiderTree.addMovePlayedGreedy game movePlayed
                          |> SpiderTree.getNode game
                          |> Option.map (fun x -> 
                                                let leaves = 
                                                    match x with 
                                                    | InternalNode (_, xs) -> xs
                                                    | LeafNode x -> []
                                                leaves |> List.exists (fun x -> 
                                                    match x with 
                                                    | InternalNode _ -> false 
                                                    | LeafNode mp -> mp = movePlayed ))
                          |> Option.defaultValue false
                      @>)


module ScrathPad =
    open SpiderSolitareTests

    type GameTableausNonEmpty() = 
        static member Game() = Arb.fromGen <| GameGenerators.gameWithTableaus (GameGenerators.tableau GameGenerators.NonEmtpyTab)

    [<Property(Verbose=true, MaxTest = 200, Arbitrary=[|typeof<GameTableausNonEmpty>|])>]
    let ``Stock is always last`` game card stock = 

        let game = 
            game 
            |> Game.getAllTabsWithColumn 
            |> List.map (fun (c,t) -> if Tableau.length t = 0 || t.Visible = [] then 
                                        c, {Visible = t.Visible @ [card]; Hidden = []}
                                      else 
                                        c,t )
            |> (F.flip Game.updateTableaus game)
            |> (fun x -> {x with Stock = stock})

        (GameMover.validMoves game |> List.contains Stock && List.length stock > 10) ==> 
            lazy (test <@
                            game 
                            |> GameMover.validMoves
                            |> ScrathPad.moveOrdering game
                            |> List.last = Stock
                        @>)

    [<Property(Verbose=true, MaxTest = 200, Arbitrary=[|typeof<GameTableausNonEmpty>|])>]
    let ``Flip is always before a column move`` game card column = 

        let game = 
            game 
            |> Game.getAllTabsWithColumn 
            |> List.map (fun (c,t) -> if c = column then 
                                        c, {Visible = []; Hidden = t.Visible @ t.Hidden @ [card]}
                                      else 
                                        c,t )
            |> (F.flip Game.updateTableaus game)
    
        (GameUtils.hasCardToBeFliped game && GameUtils.tabsAreNotEmpty game) ==> 
            lazy (test <@
                            let orderedMoves = 
                                game 
                                |> GameMover.validMoves 
                                |> ScrathPad.moveOrdering game
                            
                            orderedMoves |> List.takeWhile MoveTypeUtils.isFlip |> List.length > 0 &&
                                orderedMoves |> List.skipWhile (MoveTypeUtils.isFlip >> not) |> List.length > 0
                        @>) 


    [<Property(Verbose=true, MaxTest = 10, Arbitrary=[|typeof<GameTableausNonEmpty>|])>]
    let ``Can play multistage rollout`` i = 
        let r = System.Random i
        let game = (Game.createGame r (Card.deck Difficulty.One))


        let runSearch getFromCache addToCache = 
            ScrathPad.multistageNestedRolloutFast 
                    (fun _ _ -> false) //(hasTimeExpired: int -> Stopwatch -> bool) 
                    (fun _ _ -> false) //isDeadEnd 
                    getFromCache
                    addToCache
                    (ScrathPad.filterLocalLoopMoves GameMover.playMove)
                    ScrathPad.maxH
                    ScrathPad.moveOrdering
                    (F.flip GameMover.playMove)
                    (fun _ -> false)
                    (Diagnostics.Stopwatch.StartNew())
                    game
                    [ScrathPad.stageFour] [1] (Continue (game, GameMover.validMoves game))

        let noCache () = 
            runSearch (fun _ _ _ -> None) (fun _ _ _ _ -> ())

        let withCache () = 
            runSearch ScrathPad.getFromCache ScrathPad.addToCache 

        (game |> GameMover.validMoves |> List.length > 2) ==> 
            lazy (test <@ 
                            noCache () = withCache ()
                    @>)










































































































//module Search = 

    //[<Property(Verbose=true, MaxTest = 50 )>]
    //let ``Rollout can grow`` game newGame move timelimit  = 

    //    let playMove (_:Game) (_:MoveType) = Continue (newGame, [])
    //    let moves = [move]
    //    let tree = SpiderTree.createTree game moves 0
    //    let scoreGame (_:Game) = 1

    //    (game <> newGame) ==> 
    //        lazy (test <@
    //                        Search.rollout timelimit playMove Set.empty Map.empty [(scoreGame, 1)]  tree game moves
    //                        |> fst
    //                        |> SpiderTree.depth = 2
    //            @>)

    //[<Property(Verbose=true, MaxTest = 50 )>]
    //let ``Rollout filters games that have already been played`` game newGame moveOne moveTwo timelimit  = 

    //    let playMove (_:Game) (m:MoveType) = 
    //        if m = moveOne then Continue (newGame, []) else Continue (game, [])
    //    let moves = [moveOne; moveTwo]
    //    let tree = SpiderTree.createTree game moves 0
    //    let scoreGame (_:Game) = 1

    //    (moveOne <> moveTwo && game <> newGame) ==> 
    //        lazy (test <@
    //                        Search.rollout timelimit playMove Set.empty Map.empty [(scoreGame, 1)]  tree game moves
    //                        |> fst
    //                        |> SpiderTree.totalCount = 2
    //            @>)

    //[<Property(Verbose=true, MaxTest = 50 )>]
    //let ``Rollout handles no further moves`` game newGame timelimit playMove heuristic = 
       
    //    let moves: MoveType list = []
    //    let tree = SpiderTree.createTree game moves 0

    //    (game <> newGame) ==> 
    //        lazy (test <@
    //                        Search.rollout timelimit playMove Set.empty Map.empty [(heuristic, 1)]  tree game moves
    //                        |> fst
    //                        |> SpiderTree.totalCount = 1
    //            @>)

    //[<Property(Verbose=true, MaxTest = 50 )>]
    //let ``Rollout updates states after recursing`` gameOne gameTwo gameThree timelimit moveOne moveTwo moveThree = 

    //    let playMove (_:Game) (m:MoveType) = 
    //        match m with 
    //        | m when m = moveTwo -> Continue (gameOne, [])
    //        | m when m = moveOne -> Continue (gameTwo, [moveThree]) 
    //        | _ -> Continue (gameThree, [])

    //    let moves = [moveOne; moveTwo]
    //    let tree = SpiderTree.createTree gameOne moves 0
    //    let scoreGame (g:Game) = 
    //        match g with 
    //        | g when g = gameOne -> 0
    //        | g when g = gameTwo -> 1
    //        | _ -> 2

    //    (Set.ofList [moveOne; moveTwo; moveThree] |> Set.count = 3 && 
    //        Set.ofList [gameOne; gameTwo; gameThree] |> Set.count = 3) ==> 
    //        lazy (test <@
    //                        Search.rollout timelimit playMove Set.empty Map.empty [(scoreGame, 2)]  tree gameOne moves
    //                        |> fst
    //                        |> SpiderTree.totalCount = 3
    //            @>)

    //[<Property(Verbose=true, MaxTest = 50 )>]
    //let ``Rollout does not revist the same game`` gameOne gameTwo gameThree timelimit moveOne moveTwo moveThree = 

    //    let playMove (_:Game) (m:MoveType) = 
    //        match m with 
    //        | m when m = moveTwo -> Continue (gameOne, [moveThree]) // the same game from moveThree will be returned
    //        | m when m = moveOne -> Continue (gameTwo, [moveThree]) 
    //        | _ -> Continue (gameThree, [])

    //    let moves = [moveOne; moveTwo]
    //    let tree = SpiderTree.createTree gameOne moves 0
    //    let scoreGame (g:Game) = 
    //        match g with 
    //        | g when g = gameOne -> 0
    //        | g when g = gameTwo -> 1
    //        | _ -> 2

    //    (Set.ofList [moveOne; moveTwo; moveThree] |> Set.count = 3 && 
    //        Set.ofList [gameOne; gameTwo; gameThree] |> Set.count = 3) ==> 
    //        lazy (test <@
    //                        Search.rollout timelimit playMove Set.empty Map.empty [(scoreGame, 2)]  tree gameOne moves
    //                        |> fst
    //                        |> SpiderTree.totalCount = 3
    //            @>)

    //[<Property(Verbose=true, MaxTest = 50 )>]
    //let ``when subTrees are empty then heuristic is not changed`` 
    //        rootGame game moves (heuristics: ((Game -> int * int) list)) heuristic = 
            
    //    let tree = SpiderTree.createTree rootGame moves 0

    //    (rootGame <> game && List.length heuristics > 1) ==> 
    //        lazy (test <@ Search.shouldChangeHeuristic heuristics heuristic game tree = false @>)

    //[<Property(Verbose=true, MaxTest = 50 )>]
    //let ``when subTrees are of less value than parent node then heuristic should be changed`` 
    //        rootGame game moves playedGame (heuristics: ((Game -> int * int) list)) = 
            
    //    let tree = 
    //        SpiderTree.createTree rootGame moves 0 
    //        |> SpiderTree.addMovePlayedGreedy rootGame playedGame
       
    //    let heuristic (game: Game) = Core.int.MaxValue

    //    (rootGame <> game && rootGame <> playedGame.Game && game <> playedGame.Game && List.length heuristics > 1) ==> 
    //        lazy (test <@ Search.shouldChangeHeuristic heuristics heuristic rootGame tree @>)

    //[<Property(Verbose=true, MaxTest = 50 )>]
    //let ``Stops rollout when there are no more moves`` gameOne gameTwo gameThree timelimit moveOne moveTwo = 

    //    let moves = [moveOne]
    //    let tree = SpiderTree.createTree gameOne moves 0
    //    let playMove (_:Game) (m:MoveType) = 
    //        match m with 
    //        | m when m = moveOne -> Continue (gameTwo, [moveTwo]) 
    //        | _ -> Continue (gameThree, [])    

    //    let scoreGame (_:Game) = 1

    //    (Set.ofList [moveOne; moveTwo] |> Set.count = 2 && 
    //        Set.ofList [gameOne; gameTwo] |> Set.count = 2) ==> 
    //        lazy (test <@
    //                        Search.rollout timelimit playMove Set.empty Map.empty [(scoreGame, 1)] tree gameOne moves
    //                        |> (fun (tree, map) -> SpiderTree.totalCount tree = 2)
    //            @>)


    //[<Property(Verbose=true, MaxTest = 5 )>]
    //let ``PlayNextMoves: Given the next heuristic the tree is returned is added together`` 
    //        movePlayed  rootGame move tailHeuristics movePlayedTwo heuristic = 
    //    printfn "Running test...."

    //    let tree = 
    //        SpiderTree.createTree rootGame [move] 0 
    //        |> SpiderTree.addMovePlayedGreedy rootGame movePlayed

    //    let newTree =  tree |> SpiderTree.addMovePlayedGreedy movePlayed.Game movePlayedTwo

    //    let recurse (_: int) (tree: SpiderTree) (game: Game) (moves: MoveType list)  = newTree, Map.empty<int, int>
    //    (rootGame <> movePlayed.Game && rootGame <> movePlayedTwo.Game && movePlayedTwo.Game <> movePlayed.Game) ==>  
    //        lazy(test <@  
    //                     Search.playNextMoves (fun _ _ -> true) recurse tailHeuristics heuristic movePlayed.Game movePlayed.Moves tree Map.empty
    //                     |> fst
    //                     |> SpiderTree.totalCount = ([rootGame; movePlayed.Game; movePlayedTwo.Game] |> List.length)
    //            @>)


    //[<Property(Verbose=true, MaxTest = 10 )>]
    //let ``Second heuristic is called`` gameOne gameTwo gameThree timelimit moveOne moveTwo = 

        //let moves = [moveOne]
        //let tree = SpiderTree.createTree gameOne moves 100

        //let playMove (_:Game) (m:MoveType) = 
        //    match m with 
        //    | m when m = moveOne -> Continue (gameTwo, [moveTwo]) 
        //    | _ -> Continue (gameThree, [])

        //let scoreGame (g:Game) = 0
        //let scoreGameTwo (g:Game) = 3000

        //(Set.ofList [moveOne; moveTwo;] |> Set.count = 2 && 
            //Set.ofList [gameOne; gameTwo; gameThree] |> Set.count = 3) ==> 
            //lazy (test <@
                //            Search.rollout timelimit playMove Set.empty Map.empty [(scoreGame, 1); (scoreGameTwo, 1)]  tree gameOne moves
                //            |> fst
                //            |> SpiderTree.findBestScore = 3000
                //@>)
