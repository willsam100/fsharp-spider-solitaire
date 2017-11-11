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

//type Movablity = 
    //| Hidden
    //| Visible

//type GameCard = Card.Card * Movablity

//module GameCard = 
    //let isVisible (c, m) = m <> Hidden
    //let getCard = fst

    //let canAddCardToRun bottom top =    
    //    Card.canAddCardToRun (getCard bottom) (getCard top)

    //let cardWithMovability gameCard movability = 
        //getCard gameCard, movability

//type Run = {
//    Suit: Card.Suit
//    StartValue: int
//    EndValue: int
//}

//module RunCardValidation = 

    //let createRun card = 
    //    {Suit = Card.getSuit card; StartValue = Card.getValue card; EndValue = Card.getValue card;}

    //let addCardToRun card run =
    //    {run with EndValue = Card.getValue card}

    //let isNextCardInRun nextCard run = 
        //match run with 
        //| None -> true
        //| Some run -> 
            //run.Suit = Card.getSuit nextCard && run.EndValue + 1 = Card.getValue nextCard

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


    //let skipFirstCard tab = List.tail tab
    //let isEmpty = List.isEmpty
    //let getCards tab = List.map GameCard.getCard tab
    //let getSubset size tab = List.take size tab

    //let (bottomCard, moveablity) = List.head tableau
    //match moveablity with
    //| Visible -> (card, Visible) :: (bottomCard, Visible) :: (List.tail tableau)
    //| Hidden -> (card, Visible) :: tableau
    //| InRun -> 
        //let (firstCardNumber, firstSuit) = Card.getDetails bottomCard
        //if canAddCard bottomCard card then (card, Visible) :: tableau
        //else 
            //(card, Visible) :: (tableau |> List.map (fun (c, m) -> 
                                               //if moveablity = InRun then (c, Visible)
                                               //else (c, m)))

    //let canAddCardToRun (bottom, m) run = 
        //match run with 
        //| [] -> true
        //| run -> 
            //let (top, _) = run |> List.last
            //canAddCardToTab bottom top

        //let suit = tab |> firstCard |> Option.map (GameCard.getCard >> Card.getSuit)
        //match suit with 
        //| None -> None
        //| Some suit -> 
            //if (tab |> getRun canAddCardToRun  |> length = 13) then 
            //    (tab |> List.skip 13, suit) |> Some
            //else None
        
        //if isEmpty tableau then []
        //else 
            //let filterInvalidLength cards = 
            //    if List.length cards = 1 then []
            //    else cards
            
            //let folder run gameCard = 
            //    if isEmpty run then [gameCard]
            //    else 
            //        firstCard run 
            //        |> Option.exists (fun bottomCard -> GameCard.canAddCardToRun gameCard bottomCard)
            //        |> function 
            //        | true -> gameCard :: run
            //        | false -> run
            

            //let firstBlockedCard = Math.Min(0, List.findIndex (fun (x,vis) -> vis = Hidden) tableau)

            //tableau
            //|> List.take (firstBlockedCard + 1)
            //|> List.fold folder []
            //|> filterInvalidLength
            //|> List.rev

    //let getSuitForTab tab = 
        //tab |> firstCard |> Option.map (GameCard.getCard >> Card.getSuit)

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
    
    //let possibleMoves = 
        //seq { 
        //    for from in allColumns do
        //        for toCol in allColumns do
        //            if from <> toCol then 
        //                yield { From = from
        //                        To = toCol }
        //}
        //|> Seq.toList

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


    

    
    //let allRunMoves game = 
    //    let canMakeMove (card, coOrd) = 
    //        Game.getTabForColumn coOrd.To game
    //        |> Tableau.firstCard
    //        |> Log.printrtrnfn Debug
    //        |> Option.map (fun bottomCard -> GameCard.canAddCardToRun bottomCard card)
    //        |> Option.exists id
        
    //    let validMoves (column, run) = 
    //        Coord.possibleMoves
    //        |> List.filter (fun coOrd -> coOrd.From = column)
    //        |> List.collect (fun coOrd -> run |> Tableau.skipFirstCard |> List.map (fun c -> c, coOrd) )
    //        |> List.filter canMakeMove
    //        |> List.map (fun (gameCard,coord) -> GameCard.getCard gameCard, coord)
        
    //    let isvalidRunForMoveTab (_, tab) = tab |> List.length >= 2

    //    Coord.allColumns
    //    |> List.map (fun c -> c, Game.getTabForColumn c game)
    //    |> List.map (fun (c, t) -> c, Tableau.getRun Tableau.canAddCardToRun t)
    //    |> List.filter isvalidRunForMoveTab
    //    |> List.collect validMoves
    //    |> List.map Run
    
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


    //    let validMoves = 
    //        let movingFromEmpty coOrds = 
    //            Game.getTabForColumn coOrds.From game
    //            |> Tableau.firstCard
    //            |> Option.isSome
            
    //        let isValidMove coOrds = 
    //            let cardFrom = 
    //                Game.getTabForColumn coOrds.From game
    //                |> Tableau.firstCard
    //                |> Option.map fst
                
    //            let cardTo = 
    //                Game.getTabForColumn coOrds.To game
    //                |> Tableau.firstCard
    //                |> Option.map fst
                
    //            let canAddCardWithFromCard cardFrom = 
    //                cardTo
    //                |> Option.map (fun cardTo -> Tableau.canAddCardToTab cardTo cardFrom)
    //                |> Option.defaultValue false
                
    //            Option.exists canAddCardWithFromCard cardFrom
            
    //        Coord.possibleMoves
    //        |> List.filter movingFromEmpty
    //        |> List.filter isValidMove
    //        |> List.map Single
        
    //    let runMoves = allRunMoves game
   
    
    //let (|Completed|NoMoreMoves|InProgress|) game = 
    //    if (game.Stock
    //        |> List.length
    //        |> isZero)
    //       && (Game.getAllTabs game
    //           |> List.map List.length
    //           |> List.sum
    //           |> isZero)
    //    then Completed game
    //    else if game
    //            |> validMoves
    //            |> Seq.length
    //            |> isZero
    //    then NoMoreMoves game
    //    else InProgress game
    
    //let clearCompleteRuns getColumName getCompetedRun clearCompleted game = 

    //    let getTabForColumn c = getColumName c game
    //    let getCompletedRun c = 
    //        c |> getTabForColumn |> getCompetedRun |> Option.map (fun (tab: Tableau, suit: Card.Suit) -> c, tab, suit)

    //    Coord.allColumns 
    //    |> List.choose getCompletedRun
    //    |> List.fold (fun game (c, t, s) -> clearCompleted game c t s) game


    //let faceUpCards tab = 
    //    let folder acc gameCard = 
    //        if acc |> List.isEmpty then [GameCard.cardWithMovability gameCard Visible]
    //        else if acc |> List.exists (fun (_, m) -> m = Visible) then GameCard.cardWithMovability gameCard Visible :: acc
    //        else if GameCard.canAddCardToRun gameCard (acc |> List.head)
    //        then GameCard.cardWithMovability gameCard Visible :: acc
    //        else GameCard.cardWithMovability gameCard Visible :: acc
    //    tab
    //    |> List.takeWhile (fun (x, m) -> m <> Hidden)
    //    |> List.fold folder []
    //    |> List.rev
    
    //let updateCardMoveablity game = 
    //    let updateTab tab = 
    //        let blocked = tab |> List.skipWhile (fun (x, m) -> m <> Hidden)
    //        (faceUpCards tab @ blocked)
        
    //    let topIsVisible tab = 
    //        match tab with
    //        | [] -> []
    //        | (x, _) :: xs -> (x, Visible) :: xs
        
    //    let tabs = 
    //        Game.getAllTabsWithColumn game
    //        |> List.map (fun (c, t) -> c, updateTab t)
    //        |> List.map (fun (c, t) -> c, topIsVisible t)
        
    //    Game.updateTableaus tabs game

    //let playStock game = 
    //    let (stockCards, newStock) = game.Stock |> List.splitAt 10
    //    let newTabs = List.zip stockCards (Game.getAllTabsWithColumn game)
    //                    |> List.map (fun (card, (c,t)) -> c, Tableau.addCard Tableau.canAddCardToTab t card)
    //    Game.updateTableaus newTabs game

    //let playSingle game coords = 
    //    let fromIndex = Coord.getColumnIndex coords.From
    //    let toIndex = Coord.getColumnIndex coords.To
    //    let fromColumn = Game.getTabForColumn coords.From game

    //    let updateFromTab game = 
    //        let fromColumn = Game.getTabForColumn coords.From game |> List.tail
    //        Game.updateColumn coords.From fromColumn game
        
    //    let updateToTab game = 
    //        let card = Game.getTabForColumn coords.From game |> List.head
    //        let toColumn = Game.getTabForColumn coords.To game
    //        Game.updateColumn coords.To (card :: toColumn) game

    //    game |> updateFromTab |> updateToTab

    //let playRun game card coords = 

    //    let fromColumnHead = 
    //        let tab = Game.getTabForColumn coords.From game
            
    //        let cardIndex = 
    //            tab
    //            |> List.takeWhile (fun x -> fst x <> card)
    //            |> List.rev
    //        ((card, Visible) :: cardIndex) |> List.rev

    //    let updateFromTab game = 
    //        Game.updateColumn coords.From fromColumnHead game
        
    //    let updateToTab game = 
    //        let toColumn = Game.getTabForColumn coords.To game
    //        Game.updateColumn coords.To (fromColumnHead @ toColumn) game

    //    game |> updateFromTab |> updateToTab
    
    //let playMove updateCardMoveablity game move = 
        //let canPlayMoveForGame = validMoves >> (List.contains move)
        //if canPlayMoveForGame game |> not
        //then None
        //else 
            //let checkGameResult game =
            //    game
            //    //|> printCols "Before movability"
            //    |> updateCardMoveablity
            //    //|> printCols "Before clearing compeleted"
            //    |> clearCompleteRuns Game.getTabForColumn Tableau.getCompletedRun Game.clearCompletedRuns
            //    |> function 
            //    | InProgress g -> Continue g |> Some
            //    | Completed g -> Won |> Some
            //    | NoMoreMoves g -> Lost g |> Some

            //match move with
            //| Stock -> playStock game
            //| Single coords -> playSingle game coords
            //| Run (card, coords) -> playRun game card coords
            //|> checkGameResult


//type CompressedGame = 
//    { CStock : (int8 * int8) list
//      CTableaus : (int8 * int8 * int8) list list
//      Completed : int8 * int8 * int8 * int8 }

//module Serilization = 
    //open Game
    //open System.IO
    //open Option
    //open System.Runtime.Serialization
    //open System.Runtime.Serialization.Formatters.Binary
    
    //let formatter = new BinaryFormatter()
    //let memoryStream = new MemoryStream()
    //let serilizeGame game = formatter.Serialize(memoryStream, game)
    
    //let decompressGame game = 
    //    let decompressSuit (x : int8) = 
    //        match x |> int with
    //        | 1 -> Some Card.H
    //        | 2 -> Some Card.D
    //        | 3 -> Some Card.S
    //        | 4 -> Some Card.C
    //        | _ -> None

    //    let decompressMovability (x: int8) = 
    //        match x |> int with 
    //        | 1 -> Some Visible 
    //        | 2 -> Some Hidden  
    //        | 3 -> Some Visible  
    //        | 4 -> Some Visible
    //        | _ -> None

    //    let decompressSuitCompletion (x: int8) = 
    //        match x |> int with 
    //        | 0 -> Some Zero 
    //        | 1 -> Some One
    //        | 2 -> Some Two  
    //        | _ -> None

    //    let decompresCompletedSuits (c,d,h,s) game = 

    //        let map4 f a b c d = 
    //            a |> Option.bind (fun a -> 
    //                b |> Option.bind (fun b -> 
    //                    c |> Option.bind (fun c -> 
    //                        d |> Option.map (fun d -> f a b c d ))))

    //        let c = decompressSuitCompletion c 
    //        let d = decompressSuitCompletion d 
    //        let h = decompressSuitCompletion h 
    //        let s = decompressSuitCompletion s

    //        map4 (fun c d h s -> {game with Clubs = c; Diamonds = d; Hearts = h; Spades = s}) c d h s

    //    let decompressCard (n:int8,s:int8) = 
    //        decompressSuit s |> Option.map (fun s -> Card.Card ((n |> int |> Card.CardValue), s))
        
    //    let decompressStock (stock: (int8*int8) list): Card.Card list option = 
    //        List.map decompressCard stock |> List.sequenceOption

    //    let decompressTableaus = 
    //        let decompressTableau = 
    //            List.map (fun (x,y,z) -> decompressCard (x,y)  
    //                                     |> Option.map2 (fun movability card -> card, movability) (decompressMovability z)) 
    //            >> List.sequenceOption
    //        List.map decompressTableau >> List.sequenceOption

    //    let restoreTabs tabs game = 
    //        let cTabs = List.zip Coord.allColumns tabs
    //        Game.updateTableaus cTabs game

    //    decompressStock game.CStock 
    //    |> Option.map (fun stock -> {emptyGame with Stock = stock})
    //    |> Option.bind (fun g -> decompressTableaus game.CTableaus |> Option.map (fun x -> restoreTabs x g))
    //    |> Option.bind (fun g -> decompresCompletedSuits game.Completed g)
    
    //let compressGame game = 
        //let compressSuit = 
        //    function 
        //    | Card.H -> 1
        //    | Card.D -> 2
        //    | Card.S -> 3
        //    | Card.C -> 4

        //let compressMovability = function 
        //| Visible -> 1
        //| Hidden -> 2
        //| InRun -> 3
        //| Blocked -> 4

        //let compressSuitCompletion = function 
        //| Zero -> 0
        //| One -> 1
        //| Two -> 2

        //let compressCard x = 
        //                 Card.getDetails x
        //                 |> fst
        //                 |> int8, 
        //                 Card.getDetails x
        //                 |> snd
        //                 |> compressSuit |> int8
        
        //let compressStock = List.map compressCard

        //let compressTableaus = 
        //    let compressTab = 
        //        List.map (fun (c, m) -> compressCard c |> (fun (x,y) -> x,y, compressMovability m |> int8))
        //    List.map compressTab

        //let compresCompletedSuits game = 
        //    let compressSuitCompletionInt8 = compressSuitCompletion >> int8

        //    let c = game.Clubs |> compressSuitCompletionInt8
        //    let d = game.Diamonds |> compressSuitCompletionInt8
        //    let h = game.Hearts |> compressSuitCompletionInt8
        //    let s = game.Spades |> compressSuitCompletionInt8

        //    (c,d,h,s)
        
        //{ CStock = game.Stock |> compressStock
          //CTableaus =  game |> Game.getAllTabs |> compressTableaus
          //Completed = game |> compresCompletedSuits }

module App = 
    open Card
    open Game
    open System
    
    //let allPossilbeGames = permute Card.deck 
    //type GameResult = 
        //| Won
        //| InProgress
        //| InvalidMove
        //| Lost
    
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
    
    //let playMove move = 
    //    (fun rc -> PlayMove(move, rc))
    //    |> myAgent.PostAndReply
    //    |> (printfn "%A")
    //    start()

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



