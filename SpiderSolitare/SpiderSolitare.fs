namespace SpiderSolitare.Game
open System

//module Tuple = 
    //let apply f g (x,y) = 
    //    (f x, g y)

    //let append f x =
        //x, f x

module F = 
    let uncurry f (a,b) = f a b
    let curry f a b = f (a,b)
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

//module Guid = 
    //let TryParse s = 
        //match Guid.TryParse s with
        //| true, g -> Some g
        //| false, _ -> None


[<AutoOpen>]
module LogicUtils = 

    let rand = Random()

    let swap (a: _[]) x y =
        let tmp = a.[x]
        a.[x] <- a.[y]
        a.[y] <- tmp

    let shuffle (rand: System.Random) a =
        [1..10] |> List.iter (fun _ -> 
            Array.iteri (fun i _ -> swap a i (rand.Next(i, Array.length a))) a )


    let shuffleList rand a = 
        let arr = List.toArray a
        shuffle rand arr
        arr |> Array.toList

    //let isZero x = x = 0
    
    let rec distribute e = 
       function 
       | [] -> [ [ e ] ]
       | x :: xs' as xs -> 
           (e :: xs) :: [ for xs in distribute e xs' -> x :: xs ]
    
    let rec permute = 
        function 
        | [] -> [ [] ]
        | e :: xs -> List.collect (distribute e) (permute xs)

type Suit = H | D | S | C
// type Suit = S = 0 | C = 1 | H = 2 | D= 3

[<StructuredFormatDisplay("{DebugString}")>]
type Card = 
    Card of (int * Suit)
    with 
        override this.ToString() = 
            this |> function 
            | Card (n, suit) -> 
                let number = 
                    match n with
                    | n when n = 1 -> "A"
                    | n when n = 11 -> "J"
                    | n when n = 12 -> "Q"
                    | n when n = 13 -> "K"
                    | n -> sprintf "%d" n
                sprintf "%s:%A" number suit

        member x.DebugString = x.ToString()


type Difficulty = OneSuit | TwoSuit | FourSuit

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module CardModule = 

    let parseSuit s = 
        match s  with 
        | "H" -> H
        | "D" -> D
        | "S" -> S
        | "C" -> C
        | x -> failwithf "Invalid suit value: %s" x

    let private getDetails card = 
        let (Card (n, s)) = card
        (n, s)

    let getValue = getDetails >> fst
    let getSuit = getDetails >> snd

    let create n s = 
        match n with 
        | n when n <= 13 && n >= 1 -> 
            Card (n, s) |> Some
        | _ -> None    

    let deck numberOfSuits = 
        let suits = 
            match numberOfSuits with 
            | OneSuit -> [S; S; S; S]
            | TwoSuit -> [H; S; H; S]
            | FourSuit -> [ H; D; S; C ]

        let numbers = [ 1..13 ]
        seq { 
            for s in suits do
                for n in numbers do
                    yield Card(n, s)
        }
        |> Seq.toList

    let canAddCardToRun bottomCard topCard = 
        let (nB, sB) = getDetails bottomCard
        let (nT, sT) = getDetails topCard
        sB = sT && nB = (nT + 1)

    let printCard (card: Card) = card.ToString()

[<StructuredFormatDisplay("{DebugString}")>]
type Tableau = { 
    Visible: Card list
    Hidden: Card list
}
with 
    member x.AsString sep hiddenCards = 
        let visible = x.Visible |> List.map CardModule.printCard
        let hidden = x.Hidden |> List.map hiddenCards
           
        (visible @ sep @ hidden)
        |> List.map (sprintf "%-5s")
        |> String.concat ""

    override x.ToString() = x.AsString ["|*>"] CardModule.printCard
    member x.DebugString = x.ToString()
    member x.GameString() = x.AsString [] (fun _ -> "*")


module Tableau = 

    let gameString (tab: Tableau) = tab.GameString()
    let toString (tab: Tableau) = tab.ToString()

    let firstCard tab = 
        match tab.Visible with 
        | [] -> None 
        | xs -> List.head xs |> Some


    let length tab = List.length tab.Visible + List.length tab.Hidden
    let empty: Tableau = {Visible = List.empty; Hidden = List.empty}
    let hasHiddenCards tab = List.length tab.Hidden > 0 

    let getVisible tab = 
        tab.Visible

    let create cards = 
        match cards with 
        | [] -> empty
        | x::xs -> 
            {Visible = [x]; Hidden = xs}

    let canAddCardToTab bottomCard topCard = 
        CardModule.getValue bottomCard = CardModule.getValue topCard + 1

    let isSameSuit (xs: Card list) = 
        xs 
         |> List.map (CardModule.getSuit) |> List.distinct |> List.length = 1

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
            
        xs |> List.map (CardModule.getValue) |> folder None 

    let validate rules run = 
        rules 
        
        |> List.map (fun f -> f run) 
        |> List.reduce (&&)

    let isCompleteRun run = 
        let isFullLength xs = 
            List.length xs = 13
        run |> validate [isFullLength; isSameSuit; isAscendingInValue;]

    let dropLastCard (run: List<Card>): List<Card> = 
        run |> List.take (List.length run - 1) //|> fst

    let validateRun tab = 
        let rec recurse run: List<Card> =
            if List.length run > 13 then 
                run |> dropLastCard |> recurse
            else 
                if isCompleteRun run then 
                    tab |> List.skip 13
                else tab
        recurse tab

    let canFlipHiddenCard tab = 
        List.isEmpty tab.Visible && (List.isEmpty tab.Hidden |> not)

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

         //TODO:: consider perfomance
        let isValidRun run = run |> validate [isSameSuit; isAscendingInValue]

        let rec folder run = 
            match run, isValidRun run with 
            | [], true -> run
            | _, true -> run
            | [], false -> []
            | run, false -> run |> dropLastCard |> folder

        folder tableau.Visible 

    let addStockCard card tableau = 
        {tableau with Visible = validateRun (card :: tableau.Visible)}

    let moveCards cardsToMove tab =
        let rec removeCards visibleTab cards = 
            match visibleTab, cards with 
            | [], [] -> []
            | xs, [] -> xs
            | [], ys -> []
            | x::xs, y::ys when x = y -> removeCards xs ys
            | x::xs, y::ys -> xs

        {tab with Visible = removeCards tab.Visible cardsToMove}


    let unHideCards tab = 
        {tab with Visible = tab.Visible @ tab.Hidden; Hidden = []}

type Column = 
    C1 | C2 | C3 | C4 | C5 | C6 | C7 | C8 | C9 | C10

[<StructuredFormatDisplay("{DebugString}")>]
[<CLIMutable>]
type Coords = 
    { From : Column
      To : Column
      Card: Card }
    with 
        override x.ToString() = 
            sprintf "%A -> %A: %A" x.From x.To x.Card

        member x.DebugString = x.ToString()


module Coord = 
    let allColumns = [ C1; C2; C3; C4; C5; C6; C7; C8; C9; C10 ]

    let parseColumn c = 
        match c with 
        | 1 -> C1 
        | 2 -> C2 
        | 3 -> C3 
        | 4 -> C4 
        | 5 -> C5 
        | 6 -> C6 
        | 7 -> C7 
        | 8 -> C8 
        | 9 -> C9 
        | 10 -> C10 
        | x -> failwithf "Invalid card column: %d" x

type SuitCompletedStatus = 
    | Zero
    | One
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight

[<StructuredFormatDisplay("{DebugString}")>]
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
      Stock : Card list
      Hearts : SuitCompletedStatus
      Spades : SuitCompletedStatus
      Clubs : SuitCompletedStatus
      Diamonds : SuitCompletedStatus }

    with 

        member x.AsString tabToString = 
            let stockSize = List.length x.Stock
            [x.One; x.Two; x.Three; x.Four; x.Five; x.Six; x.Seven; x.Eight; x.Nine; x.Ten]
            |> List.mapi (fun i tab -> tab |> tabToString |> (fun s -> sprintf "%d::%s" (i + 1) s))
            |> String.concat "\n"
            |> (fun s -> sprintf "%s\nStock: %d, H = %A; D = %A, S = %A, C = %A" s stockSize x.Hearts x.Diamonds x.Spades x.Clubs)

        override x.ToString() = 
            x.AsString Tableau.toString

        member x.AsGameString() = 
            x.AsString Tableau.gameString

        member x.DebugString = x.ToString()

module Game = 
    
    let maxCardsInTableau = 54

    let canAddCards = Tableau.canAddCard Tableau.canAddCardToTab
    let addCard = Tableau.addCard (fun x y -> canAddCards y x) Tableau.validateRun
    let addCards xs tab = 

        let result = 
            xs |> List.rev |> List.fold addCard tab

        match Tableau.length result = (List.length xs + Tableau.length tab) with 
        | true -> result, None
        | false -> result, xs |> List.head |> CardModule.getSuit |> Some 

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
            | Two -> Three
            | Three -> Four
            | Four -> Five
            | Five -> Six
            | Six -> Seven
            | Seven -> Eight
            | Eight -> Eight

        match suit with 
            | Suit.H -> game.Hearts |> incrementSuit
                             |> (fun suitCompleteStatus -> { game with Hearts = suitCompleteStatus })
            | Suit.C -> game.Clubs |> incrementSuit
                             |> (fun suitCompleteStatus -> { game with Clubs = suitCompleteStatus })
            | Suit.D -> game.Diamonds |> incrementSuit
                             |> (fun suitCompleteStatus -> { game with Diamonds = suitCompleteStatus })
            | Suit.S -> game.Spades |> incrementSuit
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
    
    let createGame rand deck = 

        let createTableau count cards = Tableau.create (List.take count cards)

        let constructTableaus (game, cards) column = 
            match column with 
            | C1 | C2 | C3 | C4 -> 6
            | C5 | C6 | C7 | C8 | C9 | C10 -> 5
            |> (fun count -> 
                game |> updateColumn column (createTableau count cards), cards |> List.skip count)

        let constructStock (game, cards) = 
            {game with Stock = cards}

        let cards = 
            //List.replicate 2 deck 
            List.zip deck deck
            |> List.map (fun (x,y) -> [x;y])
            |> List.concat
            |> LogicUtils.shuffleList rand

        Coord.allColumns |> Seq.fold (constructTableaus) (emptyGame, cards) |> constructStock

    let getAllTabsWithColumn game = 
        Coord.allColumns |> List.map (fun x -> x, getTabForColumn game x)

    let getAllTabs game = 
        getAllTabsWithColumn game |> List.map snd

    let updateTableaus tabs game = 
        tabs |> List.fold (fun game (c, t) -> updateColumn c t game ) game


    let tabMap f game = 
        let tabs = game |> getAllTabsWithColumn |> List.map (fun (c,tab) -> c, f tab)
        game |> updateTableaus tabs

    let findCardToPlay from card game = 
        let fromRun =  from |> getTabForColumn game |> Tableau.getRun
        let cardToMoveIndex = fromRun |> List.tryFindIndex (fun x -> x = card)
        cardToMoveIndex |> Option.map (fun i -> fromRun |> List.take (i + 1))
            
    let playMove coord game =

        findCardToPlay coord.From coord.Card game
        |> Option.filter (fun cardsToMove -> canAddCards (List.last cardsToMove) (coord.To |> getTabForColumn game))
        |> Option.map (fun cardsToMove -> 
            let newTab, suitCompleted = coord.To |> getTabForColumn game |> addCards cardsToMove 
            let game = updateColumn coord.To newTab game 
            let newFromTab = coord.From |> getTabForColumn game |> Tableau.moveCards cardsToMove
            let game = updateColumn coord.From newFromTab game
            suitCompleted |> Option.map (completeSuit game) |> Option.defaultValue game )

    let playStock game = 
        match game.Stock with 
        | [] -> None
        | xs when List.length xs < 10 -> None
        | xs -> 
            let stock = game.Stock |> List.take 10 //|> fst
            let game = {game with Stock = List.skip 10 game.Stock }

            List.map2 (fun card (c, tab) -> 
                c, Tableau.addStockCard card tab ) stock (getAllTabsWithColumn game) 
            |> (F.flip updateTableaus game)
            |> Some

    let flip coord game = 
        let tab = coord |> getTabForColumn game 
        updateColumn coord (Tableau.flipHiddenCard tab) game

    let isComplete game =
        match game |> getAllTabs |> List.sumBy Tableau.length with 
        | 0 -> true
        | _ -> 
            // match game.Hearts, game.Spades, game.Diamonds, game.Clubs with 
            // | Two, Two, Two, Two -> true
            // | Eight, _, _, _ -> true
            // | _, Eight, _, _ -> true
            // | _, _, Eight, _ -> true
            // | _, _, _,Eight -> true
            // | _ -> false   

            match game.Spades with 
            | One -> true
            | _ -> false

    let toString game =
        game.ToString()

type MoveType = 
    | Stock
    | Flip of Column
    | Move of Coords

module MoveType = 

    let fold s f m = function 
        | Stock -> s
        | Flip c -> f c
        | Move c -> m c

    let foldMove a f = function 
    | Stock -> a
    | Flip _ -> a
    | Move c -> f c

type GameResult = 
    | Continue of (Game * MoveType list)
    | Lost of Game
    | Won of Game

module GameResult = 

    let lostOrContinue f game newGame = 
        match newGame with 
        | None -> Lost game
        | Some game -> f game     

    // let fold lost won f = function 
    // | Lost _ -> lost 
    // | Won -> won
    // | Continue (g,m) -> f g m

    // let existsTrue = fold true true
    // let existsFalse = fold false false
    // let existsWinTrue = fold false true

    // let map f = function 
    // | Lost x -> Lost x
    // | Won -> Won
    // | Continue (g, m) -> f g m |> Continue 

    // let iter g f = fold () () f g

module GameMover = 
    type CardComparison = 
        { Source : Card
          Target : Card }
    
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
        let canAddCard = Tableau.canAddCard Tableau.canAddCardToTab

        let getCard a = a, Game.getTabForColumn game a
        let canAddCard (x,y) = x, canAddCard card y

        Coord.allColumns 
        |> List.filter (fun c -> c <> fromColumn)
        |> List.map (getCard >> canAddCard)
        |> List.filter snd
        |> List.map (fun (toColumn,_) -> {From = fromColumn; To = toColumn; Card = card} |> Move)


    let validMoves game = 

        let cardMoves = 
            game 
            |> Game.getAllTabsWithColumn
            
            |> List.map (fun x -> fst x, Tableau.getRun <| snd x)
            |> List.map (fun (c,run) -> run |> List.map (fun card -> getValidMovesForCard c card game))
            |> List.concat
            |> List.concat

        let flip = 
            game 
            |> Game.getAllTabsWithColumn
            |> List.map (fun (c, tab) -> c, Tableau.canFlipHiddenCard tab )
            |> List.filter snd
            |> List.map (fst >> Flip)

        let stockMoves = game |> canPlayStock |> Option.toList
        flip @ cardMoves @ stockMoves

    let isComplete f game = 
        match Game.isComplete game with 
        | true -> Won
        | false -> f game

    let lostOrContine game = 
        match validMoves game with 
        | [] -> Lost game
        | moves -> Continue (game, moves)
    
    let startGame deck rand = 
        Game.createGame rand deck |> lostOrContine

    let createValidGame difficulty rand = 
        let rec loop () = 
            match startGame difficulty rand with 
            | Continue (g,m) -> g
            | _ -> loop ()
        loop()

    let playMove move game = 
        // let toGameResult = isComplete lostOrContine
        // let toGameResult = isComplete (fun g -> Continue (g, []))
        // let toGameResultOption = 
        //     // GameResult.lostOrContinue (isComplete lostOrContine) game
        //     GameResult.lostOrContinue (isComplete (fun g -> Continue (g, []))) game

        match validMoves game |> List.contains move with 
        | false -> Lost game
        | true -> 
            let gameOption = 
                match move with 
                | Stock -> Game.playStock game //|> toGameResultOption
                | Flip column -> game |> Game.flip column |> Some //|> toGameResult
                | Move coord -> game |> Game.playMove coord //|> toGameResultOption

            gameOption 
            |> Option.map (fun game -> 
                match Game.isComplete game with 
                | true -> Won game
                | false -> Continue (game, []) )
            |> Option.defaultValue (Lost game)




    let unHideGame game = 
        Game.tabMap (Tableau.unHideCards) game



// To pixels on the screen 


// matrix 
// rows and a set of columns, a type as well 
// 3d matrix -> 










