namespace SpiderSolitare
open System

module Tuple = 
    let apply f g (x,y) = 
        (f x, g y)

    let append f x =
        x, f x

module F = 
    let flip f x y = 
        f y x

module Option = 
    let apply f a = 
        match f, a with 
        | Some f, Some a -> Some <| f a
        | _ -> None

module List = 
    let sequenceOption list =
        let (<*>) = Option.apply
        let retn = Some

        let cons head tail = head :: tail
        let initState = retn List.empty
        
        let folder head tail = retn cons <*> (head) <*> tail
        List.foldBack folder list initState 
       
module String = 
    let join s (x : 'a seq) = String.Join(s, x)

module Guid = 
    let TryParse s = 
        match Guid.TryParse s with
        | true, g -> Some g
        | false, _ -> None

type LogLevel = 
    | Debug
    | Info

module Log = 
    let logLevel = Info
    
    let printrtrnfn level x = 
        if level >= logLevel then 
            printfn "%A" x
            x
        else x
    
    let printfn level x = 
        if level >= logLevel then printfn "%A" x

[<AutoOpen>]
module LogicUtils = 
    let isZero x = x = 0
    
    let rec distribute e = 
        function 
        | [] -> [ [ e ] ]
        | x :: xs' as xs -> 
            (e :: xs) :: [ for xs in distribute e xs' -> x :: xs ]
    
    let rec permute = 
        function 
        | [] -> [ [] ]
        | e :: xs -> List.collect (distribute e) (permute xs)

module Card = 
    type Suit = H | D | S | C
    
    type Number = 
        | CardValue of int
    
    type Card = 
        | Card of (Number * Suit)
    
    let getDetails card = 
        let (Card((CardValue n), s)) = card
        (n, s)

    let getValue = getDetails >> fst
    let getSuit = getDetails >> snd

    let create n s = 
        match n with 
        | n when n <= 13 && n >= 1 -> 
            Some (Card((CardValue n), s))
        | _ -> None

    let deck = 
        let suits = [ H; D; S; C ]
        let numbers = [ 1..13 ]
        seq { 
            for s in suits do
                for n in numbers do
                    yield Card(CardValue n, s)
        }
        |> Seq.toList

    let canAddCardToRun bottomCard topCard = 
        let (nB, sB) = getDetails bottomCard
        let (nT, sT) = getDetails topCard
        sB = sT && nB = (nT + 1)

type Tableau = { 
    Visible: Card.Card list
    Hidden: Card.Card list
}

module Tableau = 
    
    let firstCard tab = 
        match tab.Visible with 
        | [] -> None 
        | xs -> List.head xs |> Some


    let length tab = List.length tab.Visible + List.length tab.Hidden
    let empty: Tableau = {Visible = List.empty; Hidden = List.empty}

    let getVisible tab = 
        tab.Visible

    let create cards = 
        match cards with 
        | [] -> empty
        | x::xs -> 
            {Visible = [x]; Hidden = xs}

    let canAddCardToTab bottomCard topCard = 
        Card.getValue bottomCard = Card.getValue topCard + 1


    let isSameSuit xs = 
        xs |> List.map (Card.getSuit) |> List.distinct |> List.length = 1

    let isAscendingInValue xs = 
        let rec folder expectedValue values = 
            match values with
            | [] -> true
            | actualValue::values -> 
                let expectedValue = Option.defaultValue actualValue expectedValue
                if expectedValue = actualValue then 
                    folder (expectedValue + 1 |> Some) values
                else 
                    false
            
        xs |> List.map (Card.getValue) |> folder None 

    let validate rules run = 
        rules 
        |> List.map (fun f -> f run) 
        |> List.reduce (&&)

    let isCompleteRun run = 
        let isFullLength xs = 
            List.length xs = 13
        run |> validate [isFullLength; isSameSuit; isAscendingInValue;]

    let validateRun run = 
        if isCompleteRun run then 
            run |> List.skip 13
        else run

    let canFlipHiddenCard tab = 
        List.isEmpty tab.Visible && List.isEmpty tab.Hidden |> not

    let flipHiddenCard tab = 
        match tab.Visible, tab.Hidden with 
        | [], [] -> tab
        | _, [] -> tab
        | _, xs -> 
            let card = List.head tab.Hidden
            {tab with Visible = [card]; Hidden = List.tail tab.Hidden}

    let canAddCard canAddTwoCards card tableau = 
        match tableau.Visible with 
        | [] -> List.isEmpty tableau.Hidden
        | bottomCard::xs -> canAddTwoCards bottomCard card

    let addCard canAddCard validateRun tableau card =
        match canAddCard tableau card with 
        | true -> {tableau with Visible = validateRun (card :: tableau.Visible) }
        | false -> tableau

    let getRun tableau = 

        // TODO:: consider perfomance
        let isValidRun run = run |> validate [isSameSuit; isAscendingInValue]

        let rec folder run = 
            match run, isValidRun run with 
            | [], true -> run
            | _, true -> run
            | [], false -> []
            | run, false -> run |> (List.take (List.length run - 1)) |> folder

        folder tableau.Visible 

    let addStockCard card tableau = 
        {tableau with Visible = card :: tableau.Visible}

    let moveCards cardsToMove tab =
        let rec removeCards visibleTab card = 
            match visibleTab, card with 
            | [], [] -> []
            | xs, [] -> xs
            | [], ys -> []
            | x::xs, y::ys when x = y -> removeCards xs ys
            | x::xs, y::ys -> xs

        {tab with Visible = removeCards tab.Visible cardsToMove}

type Column = 
    C1 | C2 | C3 | C4 | C5 | C6 | C7 | C8 | C9 | C10

type Coords = 
    { From : Column
      To : Column
      Card: Card.Card }

module Coord = 
    let getColumnIndex = 
        function 
        | C1 -> 0
        | C2 -> 1
        | C3 -> 2
        | C4 -> 3
        | C5 -> 4
        | C6 -> 5
        | C7 -> 6
        | C8 -> 7
        | C9 -> 8
        | C10 -> 9

    let indexToColumName index = 
        match index with
        | 0 -> C1 |> Some
        | 1 -> C2 |> Some
        | 2 -> C3 |> Some
        | 3 -> C4 |> Some
        | 4 -> C5 |> Some
        | 5 -> C6 |> Some
        | 6 -> C7 |> Some
        | 7 -> C8 |> Some
        | 8 -> C9 |> Some
        | 9 -> C10 |> Some
        | _ -> None

    let allColumns = [ C1; C2; C3; C4; C5; C6; C7; C8; C9; C10 ]

type SuitCompletedStatus = 
    | Zero
    | One
    | Two

type Game = 
    { One: Tableau
      Two: Tableau
      Three: Tableau
      Four: Tableau
      Five: Tableau
      Six: Tableau
      Seven: Tableau
      Eight: Tableau
      Nine: Tableau
      Ten: Tableau
      Stock : Card.Card list
      Hearts : SuitCompletedStatus
      Spades : SuitCompletedStatus
      Clubs : SuitCompletedStatus
      Diamonds : SuitCompletedStatus }

module Game = 
    
    let maxCardsInTableau = 54


    let canAddCards = Tableau.canAddCard Tableau.canAddCardToTab
    let addCard = Tableau.addCard (fun x y -> canAddCards y x) Tableau.validateRun
    let addCards xs tab = 
        let suit = xs |> List.head |> Card.getSuit

        let result = 
            xs |> List.rev |> List.fold (fun tab card -> addCard tab card) tab

        match Tableau.length result = (List.length xs + Tableau.length tab) with 
        | true -> result, None
        | false -> result, Some suit

    let emptyGame = 
        { Stock = List.empty
          One = Tableau.empty
          Two = Tableau.empty
          Three = Tableau.empty
          Four = Tableau.empty
          Five= Tableau.empty
          Six = Tableau.empty
          Seven = Tableau.empty
          Eight = Tableau.empty
          Nine = Tableau.empty
          Ten = Tableau.empty
          Hearts  = Zero
          Spades = Zero
          Clubs = Zero
          Diamonds = Zero }
    
    let completeSuit game suit = 
        let incrementSuit = 
            function 
            | Zero -> One
            | One -> Two
            | Two -> Two

        match suit with 
            | Card.Suit.H -> game.Hearts |> incrementSuit
                             |> (fun suitCompleteStatus -> { game with Hearts = suitCompleteStatus })
            | Card.Suit.C -> game.Clubs |> incrementSuit
                             |> (fun suitCompleteStatus -> { game with Clubs = suitCompleteStatus })
            | Card.Suit.D -> game.Diamonds |> incrementSuit
                             |> (fun suitCompleteStatus -> { game with Diamonds = suitCompleteStatus })
            | Card.Suit.S -> game.Spades |> incrementSuit
                             |> (fun suitCompleteStatus -> { game with Spades = suitCompleteStatus })

    let getTabForColumn game column = 
        match column with 
        | C1 -> game.One
        | C2 -> game.Two
        | C3 -> game.Three
        | C4 -> game.Four
        | C5 -> game.Five
        | C6 -> game.Six
        | C7 -> game.Seven
        | C8 -> game.Eight
        | C9 -> game.Nine
        | C10 -> game.Ten

    let updateColumn column tab game = 
        match column with 
        | C1 -> {game with One = tab}
        | C2 -> {game with Two = tab}
        | C3 -> {game with Three = tab}
        | C4 -> {game with Four = tab}
        | C5 -> {game with Five = tab}
        | C6 -> {game with Six = tab} 
        | C7 -> {game with Seven = tab} 
        | C8 -> {game with Eight = tab}
        | C9 -> {game with Nine = tab}
        | C10 -> {game with Ten = tab}
    
    let createGame gameNumber deck = 

        let constructTableaus (game, cards) column = 
            match column with 
            | C1 -> {game with One = Tableau.create (List.take 6 cards) }, cards |> List.skip 6
            | C2 -> {game with Two = Tableau.create (List.take 6 cards) }, cards |> List.skip 6
            | C3 -> {game with Three = Tableau.create (List.take 6 cards) }, cards |> List.skip 6
            | C4 -> {game with Four = Tableau.create (List.take 6 cards) }, cards |> List.skip 6
            | C5 -> {game with Five = Tableau.create (List.take 5 cards) }, cards |> List.skip 5
            | C6 -> {game with Six = Tableau.create (List.take 5 cards) }, cards |> List.skip 5
            | C7 -> {game with Seven = Tableau.create (List.take 5 cards) }, cards |> List.skip 5
            | C8 -> {game with Eight = Tableau.create (List.take 5 cards) }, cards |> List.skip 5
            | C9 -> {game with Nine = Tableau.create (List.take 5 cards) }, cards |> List.skip 5
            | C10 -> {game with Ten = Tableau.create (List.take 5 cards) }, cards |> List.skip 5

        let constructStock (game, cards) = 
            {game with Stock = cards}

        let cards = 
            Seq.replicate 2 deck 
            |> Seq.concat 
            |> Seq.toList
            |> LogicUtils.permute
            |> List.skip (gameNumber)
            |> List.head

        Coord.allColumns |> Seq.fold (constructTableaus) (emptyGame, cards) |> constructStock

    let getAllTabsWithColumn game = 
        Coord.allColumns |> List.map (fun x -> x, getTabForColumn game x)

    let getAllTabs game = 
        getAllTabsWithColumn game |> List.map snd

    let updateTableaus tabs game = 
        tabs |> List.fold (fun game (c, t) -> updateColumn c t game ) game

    let findCardToPlay from card game = 
        let fromRun =  from |> getTabForColumn game |> Tableau.getRun
        let cardToMoveIndex = fromRun |> List.tryFindIndex (fun x -> x = card)
        cardToMoveIndex |> Option.map (fun i -> fromRun |> List.take (i + 1))
            
    let playMove coord game =
            findCardToPlay coord.From coord.Card game
            |> Option.filter (fun cardsToMove -> canAddCards (List.last cardsToMove) (coord.To |> getTabForColumn game))
            |> Option.map (fun cardsToMove -> cardsToMove, coord.To |> getTabForColumn game |> addCards cardsToMove )
            |> Option.map (fun (cardsToMove, (newTab, suitCompleted) ) -> 
                let game = updateColumn coord.To newTab game 
                let newFromTab = coord.From |> getTabForColumn game |> Tableau.moveCards cardsToMove
                let game = updateColumn coord.From newFromTab game
                suitCompleted |> Option.map (completeSuit game) |> Option.defaultValue game)

    let playStock game = 
        match game.Stock with 
        | [] -> None
        | xs when List.length xs < 10 -> None
        | xs -> 
            let stock = game.Stock |> List.take 10

            let game = {game with Stock = List.skip 10 game.Stock }

            List.map2 (fun card (c, tab) -> 
                c, Tableau.addStockCard card tab ) stock (getAllTabsWithColumn game) 
            |> (F.flip updateTableaus game)
            |> Some


    let flip coord game = 
        let tab = coord |> getTabForColumn game 
        updateColumn coord (Tableau.flipHiddenCard tab) game

    let isComplete game = 
        match game.Hearts, game.Spades, game.Diamonds, game.Clubs with 
        | Two, Two, Two, Two -> true
        | _ -> false   

type MoveType = 
    | Stock
    | Flip of Column
    | Move of Coords

type GameResult = 
    | Continue of (Game * MoveType list)
    | Lost of Game
    | Won

module GameResult = 

    let lostOrContinue f game newGame = 
        match newGame with 
        | None -> Lost game
        | Some game -> f game             

module GameMover = 
    type CardComparison = 
        { Source : Card.Card
          Target : Card.Card }
    
    open Game
    
    let canPlayStock game = 
        let hasCardsToPlay = game.Stock |> Seq.length > 0
        let cardIsOnAllSlots = Game.getAllTabs game |> List.forall (fun x -> x |> Tableau.getVisible |> List.length > 0)

        if hasCardsToPlay && cardIsOnAllSlots then Some Stock else None

    let getRuns game = 
        game
        |> Game.getAllTabsWithColumn 
        |> List.map (fun (c, t) -> c, Tableau.getRun t)
        |> List.filter (fun (c,t) -> List.isEmpty t |> not)


    let getValidMovesForCard fromColumn card game = 
        Coord.allColumns 
        |> List.map (game |> Game.getTabForColumn |> Tuple.append)
        |> List.map (Tuple.apply id (Tableau.canAddCard Tableau.canAddCardToTab card))
        |> List.filter snd
        |> List.map (Tuple.apply id (fun _ -> card))
        |> List.map (fun (toColumn,card) -> {From = fromColumn; To = toColumn; Card = card})
        |> List.map Move


    let validMoves game = 

        let cardMoves = 
            game 
            |> Game.getAllTabsWithColumn
            |> List.map (Tuple.apply id Tableau.getRun)
            |> List.map (fun (c,run) -> run |> List.map (fun card -> getValidMovesForCard c card game))
            |> List.concat
            |> List.concat

        let flip = 
            game 
            |> Game.getAllTabsWithColumn
            |> List.map (Tuple.apply id Tableau.canFlipHiddenCard)
            |> List.filter snd
            |> List.map fst
            |> List.map Flip

        let stockMoves = game |> canPlayStock |> Option.toList

        cardMoves @ stockMoves @ flip

    let isComplete f game = 
        match Game.isComplete game with 
        | true -> Won
        | false -> f game

    let lostOrContine game = 
        match validMoves game with 
        | [] -> Lost game
        | moves -> Continue (game, moves)
    
    let startGame () = 
        Game.createGame 5 Card.deck |> lostOrContine

    let playMove move game = 
        let toGameResult = isComplete lostOrContine
        let toGameRusultOption = GameResult.lostOrContinue lostOrContine game

        match move with 
        | Stock -> Game.playStock game |> toGameRusultOption
        | Flip column -> game |> Game.flip column |> toGameResult
        | Move coord -> game |> Game.playMove coord |> toGameRusultOption

module App = 
    open Card
    open Game
    open System

    type AppMove = 
        | GetMoves of AsyncReplyChannel<List<int * MoveType>>
        | PlayMove of int * AsyncReplyChannel<GameResult>
        | GetGame of AsyncReplyChannel<GameResult>
    
    let printCard (Card ((CardValue n), suit)) = 
        let number = 
            match n with
            | n when n = 1 -> "Ace"
            | n when n = 11 -> "Jack"
            | n when n = 12 -> "Queen"
            | n when n = 13 -> "King"
            | n -> sprintf "%d" n
        sprintf "%A:%A" number suit

    let printableGame game =
        game  
        |> Game.getAllTabs
        |> List.mapi (fun i tab -> 
            let visible = tab.Visible |> List.map printCard
            let hidden = tab.Hidden |> List.map (fun _ -> "***")
               
            (visible @ hidden)
            |> List.map (sprintf "%-12s")
            |> List.toSeq
            |> String.join ""
            |> (fun s -> sprintf "%d :: %s" (i + 1) s))
        |> String.join "\n"

    let printMove = function 
    | Stock -> "Stock"
    | Flip column -> sprintf "%A" column
    | Move coord -> sprintf "%A: %A -> %A" coord.Card coord.From coord.To

    let printMoves moves = 
        moves |> List.mapi (fun i m -> sprintf "%d -- %-20s" i  (printMove m))

    let printGameResult x = printfn "PRINTING GAME"; x |> function 
        | Lost game -> sprintf "LOST GAME\n" + (printableGame game)
        | Won -> "GAME HAS BEEN WON"
        | Continue (game, moves) -> 
            let add s y = sprintf "%s\n%s" s y  
            printableGame game + "\n" + (moves |> printMoves |> List.reduce (add))

    let myAgent = 
        MailboxProcessor.Start(fun inbox -> 

            let getMoves = GameMover.validMoves >> List.map (fun x -> Guid.NewGuid(), x) >> Map.ofList
            
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

            loop (GameMover.startGame ()))
    
    let start() = 
        myAgent.PostAndReply GetGame |> printGameResult |> printfn "%s"

    let playMoveAtIndex indexMove = 
        printfn "Playing: %d" indexMove
        (fun rc -> PlayMove(indexMove, rc))
        |> myAgent.PostAndReply
        |> printGameResult
        |> printfn "%s"

    let getGame() = 
        myAgent.PostAndReply GetGame |> function 
        | Lost g -> Some g
        | Won -> None
        | Continue (game, _) -> Some game



