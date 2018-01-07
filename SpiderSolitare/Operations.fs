namespace SpiderSolitare.Operations
open SpiderSolitare.Game

module MLTab = 

    let hiddenLength tab = List.length tab.Hidden
    let getStreak tableau = 
        let isValidRun run = run |> Tableau.validate [ Tableau.isAscendingInValue]

        let rec folder run = 
            match run, isValidRun run with 
            | [], true -> run
            | _, true -> run
            | [], false -> []
            | run, false -> run |> (List.take (List.length run - 1)) |> folder

        folder tableau.Visible 

module GameOperations = 

    let getGameAndMove = function 
        | Continue (game, moves) -> 
            (game,moves)


    let toGameAndMoves games = 
        let folder acc = function 
            | m, Won -> acc
            | m, Lost _ -> acc
            | m, Continue x -> (m,x) :: acc
        Seq.fold folder [] games

    let filterLocalLoopMoves playMove s ms = 
            let playMoves s ms =
                    ms   |> List.map (fun a -> a, playMove a s)

            let isWin = function
                | Won -> true
                | _ -> false
                       
            let hasWonGame = 
                List.filter (fun (a, x) -> isWin x)

            if List.isEmpty ms then List.empty else 
            let firstLevel =  playMoves s ms 

            let movesThatWinGame = 
                firstLevel |> hasWonGame |> List.map fst

            let secondLevel = 
                firstLevel 
                |> List.filter (fun (a, _) -> List.contains a movesThatWinGame |> not) 
                |> toGameAndMoves 
                |> List.map (fun (a, (s, ms)) -> a, playMoves s ms)

            let movesThatWinGame = 
                movesThatWinGame @ (secondLevel |> List.filter (fun (a, ss) -> ss |> hasWonGame |> List.isEmpty |> not) |> List.map fst)

            let accpetedMoves = 
                secondLevel
                |> List.filter (fun (a, _) -> List.contains a movesThatWinGame |> not) 
                |> List.map (fun (a, ss) -> a, toGameAndMoves ss |> List.map (snd >> fst))
                |> List.filter (fun (a, ss) -> List.forall (fun x -> x = s) ss |> not) 
                |> List.map fst

            movesThatWinGame @ accpetedMoves

    let moveOrdering s ms = 
        if List.isEmpty ms then List.empty else 

        let columnsWithOneCard = 
            s 
            |> Game.getAllTabsWithColumn 
            |> List.filter (fun (c,t) -> Tableau.getVisible t |> List.length = 1)
            |> List.map fst

        let columnsWithRun = 
            s 
            |> Game.getAllTabsWithColumn 
            |> List.filter (fun (c,t) -> Tableau.getRun t |> List.length > 1)
            |> List.map fst

        let columnsWithStreak = 
            s 
            |> Game.getAllTabsWithColumn 
            |> List.filter (fun (c,t) -> MLTab.getStreak t |> List.length > 1)
            |> List.map fst

        let applyToMove f s m  = 
            match m with 
            | Stock -> s
            | Flip _ -> s
            | Move m -> f s m

        let scoreFaceUp s m = 
            if List.contains m.From columnsWithOneCard then s + 5 else s

        let addToRun s m = 
            if List.contains m.To columnsWithRun && List.contains m.From columnsWithRun |> not then s + 4 else s 

        let addToRunOrStreak s m = 
            if List.contains m.To columnsWithRun && List.contains m.From columnsWithRun || List.contains m.To columnsWithStreak then s + 3 else s 

        let isFlip s m = 
            match m with 
            | Stock -> s
            | Flip _ -> 100
            | Move _ -> s

        //"Moves:" |> log
        //App.printableGame s |> log
        ms 
        |> List.map (fun m -> 0, m)
        |> List.map (fun (s,m) -> isFlip s m, m)
        |> List.map (fun (s,m) -> applyToMove scoreFaceUp s m, m)
        |> List.map (fun (s,m) -> applyToMove addToRun s m, m)
        |> List.map (fun (s,m) -> applyToMove addToRunOrStreak s m, m)
        |> List.sortByDescending (fst)
        //|> List.map (fun (v, m) -> m |> App.printMove |> sprintf "%d: %s" v |>  log; m)
        //|> (fun x -> printfn "---\n"; x)
        |> List.map snd



    let getReward state = 
        match state with 
        | Won -> 1000.
        | Lost _ -> -1000.
        | Continue (state, moves) -> 
            let suitCompletion = 
                let scoreSuit = function 
                    | Zero -> 0
                    | SuitCompletedStatus.One -> 100 //(13 * 13) * 40
                    | SuitCompletedStatus.Two -> 200 //(13 * 13) * 50
                    | SuitCompletedStatus.Three -> 300 //(13 * 13) * 60
                    | SuitCompletedStatus.Four -> 1000
                [state.Hearts; state.Spades; state.Clubs; state.Diamonds] 
                
                |> List.map scoreSuit |> List.sum |> float

            let distanceToEmptyColumn = 
                let shortestColumn = 
                    state |> Game.getAllTabs  |> List.map Tableau.length |> List.map (fun x -> x + 1 |> float) |> List.min
                1. / shortestColumn

            let lengthOfLogestRun = 
                1. -  (1. / (state |> GameMover.getRuns  |> List.map (snd >> List.length) |> List.max |> float))

            let runToLengthValue = 
                let lengthOfRuns =  
                    state |> Game.getAllTabs  |> List.map Tableau.getRun |> List.map (List.length >> float)
                let lengthOfVisible = 
                    state |> Game.getAllTabs  |> List.map Tableau.getVisible |> List.map (List.length >> float)

                1. - (1. /
                    (List.zip lengthOfRuns lengthOfVisible
                    |> List.map (fun (r,v) -> if (v = 0.) then 0. else  r / v )
                    |> List.sum))

            let lengthOfRuns = 
                1. - (1. /
                    (state 
                    |> Game.getAllTabs 
                    |> List.map Tableau.getRun 
                    |> List.map (List.length >> float)
                    |> List.filter (fun x -> x = 1.)
                    |> List.map (fun x -> x * 2.)
                    |> List.sum))

            //state  |> Game.getAllTabs  |> List.map (Tableau.getRun >> List.length >> string) |> (String.concat "," >> printfn "%s")

            //printfn "Suit: %f, dEmpty:%f, long:%f, r2L:%f, lenRuns:%f" suitCompletion  distanceToEmptyColumn  lengthOfLogestRun  runToLengthValue  lengthOfRuns 
            suitCompletion + distanceToEmptyColumn + lengthOfLogestRun + runToLengthValue + lengthOfRuns |> float

    let rec findMinmia state reward = 
        state |> function 
        | Won -> state
        | Lost _ -> state
        | Continue (game,moves) -> 
            let s' = 
                if moves = [Stock] then 
                    GameMover.playMove Stock game
                else 
                    moves
                    |> List.filter (fun x -> x <> Stock) 
                    |> (filterLocalLoopMoves GameMover.playMove game)
                    |> moveOrdering game
                    |> List.map (fun a -> GameMover.playMove a game)
                    |> List.maxBy (fun s -> getReward s)

            let reward' = getReward s'
            if (reward' > reward ) then findMinmia s' reward'
            else s'


    let playToLocalMinima state = 
        findMinmia state (getReward state)

    let playMoveToMinima game move = 
      
        let next = GameMover.playMove game move
        match next with 
        | Won -> next
        | Lost _ -> next
        | Continue (game, moves) -> 
            findMinmia next (getReward next)

module App = 
    open Card
    open Game
    open System

    type AppMove = 
        | GetMoves of AsyncReplyChannel<List<int * MoveType>>
        | PlayMove of int * AsyncReplyChannel<GameResult>
        | GetGame of AsyncReplyChannel<GameResult>
       

    let toStringGame game =
        game  
        |> Game.getAllTabs
        |> List.mapi (fun i tab -> 
            let visible = tab.Visible |> List.map printCard
            let hidden = tab.Hidden |> List.map (fun _ -> "*")
               
            (visible @ hidden)
            |> List.map (sprintf "%-5s")
            |> List.toSeq
            |> String.concat ""
            |> (fun s -> sprintf "%-2d::%s" (i + 1) s))

    let printMove = function 
    | Stock -> "Stock"
    | Flip column -> sprintf "Flip: %A" column
    | Move coord -> coord.ToString()

    let printMoves moves = 
        moves |> List.mapi (fun i m -> sprintf "i:%d %s" i (printMove m))

    let printGameResult x = printfn "PRINTING GAME"; x |> function 
        | Lost game -> sprintf "LOST GAME\n" + (toString game)
        | Won -> "GAME HAS BEEN WON"
        | Continue (game, moves) -> 
            let add s y = sprintf "%s\n%s" s y  
            toString game + "\n" + (moves |> printMoves |> List.reduce (add))


    let myAgent rand = 
        MailboxProcessor.Start(fun inbox -> 

            let rec loop gameResult = 
                async { 
                    let! msg = inbox.Receive()
                    match msg with
                    | GetGame rc -> 
                        gameResult |> rc.Reply
                        return! loop gameResult
                    | GetMoves rc -> 
                        match gameResult with 
                        | Lost g -> rc.Reply []
                        | Won -> rc.Reply []
                        | Continue (g, moves) -> moves |> List.indexed |> rc.Reply 
                        return! loop gameResult
                    | PlayMove(moveIndex, rc) -> 
                        match gameResult with 
                        | Continue (game,moves) -> 
                            let move = moves |> List.indexed |> List.tryFind (fun (x,y) -> x = moveIndex)
                            match move with 
                            | None -> 
                                rc.Reply gameResult
                                return! loop gameResult
                            | Some (_, move) -> 
                                let gameResult = GameMover.playMove move game
                                rc.Reply gameResult
                                return! loop gameResult
                        | _ -> 
                            rc.Reply gameResult
                            return! loop gameResult
                }

            loop (GameMover.startGame (Card.deck One) rand))
    
    let start (gameAgent: MailboxProcessor<AppMove>) = 
        gameAgent.PostAndReply GetGame |> printGameResult |> printfn "%s"

    let playMoveAtIndex (gameAgent: MailboxProcessor<AppMove>) indexMove = 
        printfn "Playing: %d" indexMove
        (fun rc -> PlayMove(indexMove, rc))
        |> gameAgent.PostAndReply

    let playAndPrint gameAgent indexMove = 
        playMoveAtIndex gameAgent indexMove
        |> (fun x -> printfn "Scored: %f" <| GameOperations.getReward x; x)
        |> printGameResult
        |> printfn "%s"

    let getGame (gameAgent: MailboxProcessor<AppMove>) = 
        gameAgent.PostAndReply GetGame 
        //|> function 
        //| Lost g -> Some g
        //| Won -> None
        //| Continue (game, _) -> Some game

