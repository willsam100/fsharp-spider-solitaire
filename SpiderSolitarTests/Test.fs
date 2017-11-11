namespace SpiderSolitarTests
open System
open NUnit.Framework
open SpiderSolitare.App
open SpiderSolitare.Game
open SpiderSolitare.Card
open FsCheck.NUnit
open FsCheck
open FsUnit
open SpiderSolitare
open SpiderSolitare.GameMover
open Swensen.Unquote

[<TestFixture>]
type GameStateTest() = 

    let getStock game = game.Stock

    [<Test>]
    member x.``first four tableaus have 6 cards`` () =
        createGame 1 deck |> Game.getAllTabs |> List.take 4 |> List.map Tableau.length |> List.sum |> should be (equal (6 * 4))

    [<Test>]
    member x.``last five tableaus have 5 cards`` () =
        createGame 1 deck |> Game.getAllTabs |> List.skip 4 |> List.map Tableau.length |> List.sum |> should be (equal (5 * 6))

    [<Test>]
    member x.``Each card only appears twice`` () =

        let generatedGame = createGame 1 deck
        true
        //printfn "%A" generatedGame

        //let cardFolder map (card, isVisible) = 
        //    match Map.tryFind card map with 
        //    | Some count -> Map.add card (count + 1 ) map
        //    | None -> Map.add card 1 map

        //let folder map tableau = 
        //    tableau |> List.fold cardFolder map 

        //let map =
        //    generatedGame 
        //    |> Game.getAllTabs |> List.fold folder (Map.empty<Card, int>)
        //    |> (fun map -> generatedGame.Stock |> List.map (fun x -> (x, false)) |> List.fold cardFolder map)

        //map |> Map.toList |> List.iter (printfn "%A")

        //map |> Map.forall (fun key count -> count = 2) |> should be True

    [<Test>]
    member x.``Entire game constains exactly 104 cards`` () =

        let generatedGame = createGame 1 deck
        let countOfTableus = generatedGame |> Game.getAllTabs |> List.map (Tableau.length) |> List.sum
        let stockCount = generatedGame |> (fun x -> x.Stock) |> List.length

        countOfTableus + stockCount |> should be (equal 104)

    [<Test>]
    member x.``Tableaus contain 54 cards`` () =
        createGame 1 deck |> Game.getAllTabs |> List.map (Tableau.length) |> List.sum |> should be (equal 54)

    [<Test>]
    member x.``Tableaus contain 50 cards`` () =
        createGame 1 deck |> getStock |> List.length |> should be (equal 50)


module GameGenerators = 
    let restrictToOnlyTwoOfTheSameCard gameCards = 

        let folder acc card = 
            match Map.tryFind card acc with 
            | Some c -> Map.add card (c + 1) acc
            | None -> Map.add card 1 acc

        let filterCards map =
            match Map.toList map |> List.map snd |> List.max with 
            | n when n < 3 -> gameCards
            | _ -> gameCards |> List.take 2

        match List.isEmpty gameCards with 
        | true -> gameCards
        | false -> 
            gameCards |> List.fold folder Map.empty |> filterCards

    let card = 
        gen {
            let! cardValue = Gen.choose (1, 13)
            let! suit = Arb.generate<Suit>
            return Card(CardValue cardValue, suit)
        }

    let hiddenCards = 
        gen {
            let! n = Gen.choose (0, 4)
            let! cards = Gen.listOfLength n card

            return restrictToOnlyTwoOfTheSameCard cards
        }

    let tableau canBeEmpty = 
        gen {
            let! hiddenCards = hiddenCards
            let startIndex = if canBeEmpty then 0 else 1
            let! sizeOfCards = Gen.choose (startIndex, 20)
            let! cards = Gen.listOfLength sizeOfCards card
            let gameCards = cards |> List.sort

            let validVisibleCards = restrictToOnlyTwoOfTheSameCard gameCards
            return {Visible = validVisibleCards; Hidden = hiddenCards}
        }

    let tableauWithRun = 
        gen {

            let! suit = Arb.generate<Suit>
            let cardValues = [1..13] |> List.map (fun number -> Card (CardValue number, suit))
            let! startIndex = Gen.choose (1,12)
            let! numberToTake = Gen.choose (0, 13-startIndex)
            let! suitChangeIndex = Gen.choose (0,numberToTake)
            let! secondSuit = Arb.generate<Suit>

            let swapSuit index (Card (number, suit)) = 
                if index = suitChangeIndex then (Card (number, secondSuit))
                else (Card (number, suit))

            let gameCards = 
                cardValues 
                |> List.skip startIndex 
                |> List.take numberToTake 
                |> List.mapi swapSuit

            return {Visible = restrictToOnlyTwoOfTheSameCard gameCards; Hidden = []}
        }

    let gameWithTableaus tableau = 
        gen {
            let! one = tableau
            let! two = tableau
            let! three = tableau
            let! four = tableau
            let! five = tableau
            let! six = tableau
            let! seven = tableau
            let! eight = tableau
            let! nine = tableau
            let! ten = tableau

            return{
                emptyGame with 
                    One = one
                    Two = two
                    Three = three
                    Four = four
                    Five = five
                    Six = six
                    Seven = seven
                    Eight = eight
                    Nine = nine
                    Ten = ten }
        }

module GameTests =

    type GameTableaus() = 
        static member Game() = Arb.fromGen <| GameGenerators.gameWithTableaus (GameGenerators.tableau true)

    type GameTableausNonEmpty() = 
        static member Game() = Arb.fromGen <| GameGenerators.gameWithTableaus (GameGenerators.tableau false)

    [<Property(Verbose=true, MaxTest = 10)>]
    let ``Game updates card movability`` suit =    

        let game = Game.completeSuit emptyGame suit
        test <@  game.Hearts = One || game.Clubs = One || game.Spades = One || game.Diamonds = One @>

    [<Property(Verbose=true, MaxTest = 10)>]
    let ``A suit can be update twice`` suit =    

        let game = Game.completeSuit emptyGame suit |> (fun game -> Game.completeSuit game suit)

        test <@ game.Hearts = Two || game.Clubs = Two || game.Spades = Two || game.Diamonds = Two @>


    [<Property(Verbose=true, MaxTest = 10, Arbitrary=[|typeof<GameTableausNonEmpty>|])>]
    let ``A tab can be retrieved`` game column =    

        let tab = Game.getTabForColumn game column
        test <@  game |> Game.getAllTabs |> List.exists (fun x -> x = tab) @>

    [<Property(Verbose=true, MaxTest = 5, Arbitrary=[|typeof<GameTableaus>|])>]
    let ``Get all tabs always returns 10 columns`` game  =    
        test <@ game |> Game.getAllTabs |> List.length = 10 @>

    [<Property(Verbose=true, MaxTest = 5, Arbitrary=[|typeof<GameTableaus>|])>]
    let ``Get all tabs with columns always returns 10 columns`` game  =  
        test <@ game 
                |> Game.getAllTabsWithColumn 
                |> List.map fst 
                |> Set.ofList 
                |> Set.count = 10 @>

    [<Property(Verbose=true, MaxTest = 5, Arbitrary=[|typeof<GameTableaus>|])>]
    let ``GetTabForColumn is consitent with getAllTabsWithColum`` game =  

        let allTabs = game |> Game.getAllTabsWithColumn |> List.map snd
        let columnTabs = game |> Game.getAllTabsWithColumn 
                         |> List.map fst 
                         |> List.map (Game.getTabForColumn game)

        test <@ 
                List.zip allTabs columnTabs
                |> List.forall (fun (x,y) -> x = y)
                @>

    [<Property(Verbose=true, MaxTest = 5, Arbitrary=[|typeof<GameTableaus>|])>]
    let ``FindCardToPlay never returns some with an empty list`` column card game =
        (column |> Game.getTabForColumn game |> Tableau.getRun |> List.exists (fun x -> x = card) |> not) ==> 
            lazy (test <@ Game.findCardToPlay column card game = None @>)

    [<Property(Verbose=true, MaxTest = 10, Arbitrary=[|typeof<GameTableausNonEmpty>|])>]
    let ``FindCardToPlay can find the card`` column game =
        let card = column |> Game.getTabForColumn game |> Tableau.getRun |> List.head
        test <@ Game.findCardToPlay column card game = Some ([card ]) @>

    [<Property(Verbose=true, MaxTest = 10, Arbitrary=[|typeof<GameTableausNonEmpty>|])>]
    let ``FindCardToPlay can find the card when there is a run`` column game =
        let tabRun = column |> Game.getTabForColumn game |> Tableau.getRun
        let card =  tabRun |> List.last
        (List.length tabRun > 1) ==> 
            lazy (test <@ 
                            Game.findCardToPlay column card game 
                            |> Option.exists (fun cardsToPlay -> 
                                List.length cardsToPlay = List.length tabRun && (List.last cardsToPlay = card))  @>)
             
    [<Property(Verbose=true, MaxTest = 50 )>]
    let ``Playing stock does not change card count`` game = 

        let cardCount game = List.length game.Stock + (game |> Game.getAllTabs |> List.map Tableau.length |> List.sum)
        (game.Stock |> List.length >= 10) ==>
            lazy (test <@ 
                            game 
                            |> Game.playStock 
                            |> Option.map cardCount
                            |> Option.exists (fun count -> count = cardCount game)  @>)

    [<Property(Verbose=true, MaxTest = 50 )>]
    let ``Playing stock then top card of each tableau must equal the stock`` game = 
            (game.Stock |> List.length >= 10) ==>
            lazy (test <@ 
                            game 
                            |> Game.playStock 
                            |> Option.map (fun game -> 
                                game |> Game.getAllTabs 
                                |> List.map Tableau.getVisible 
                                |> List.map List.head)
                            |> Option.exists (fun topCards -> topCards = List.take 10 game.Stock)  @>)

    [<Property(Verbose=true, MaxTest = 50, Arbitrary=[|typeof<GameTableaus>|])>]
    let ``PlayMove handles invalid move`` coord game = 
        test <@ Game.playMove {coord with From = coord.To} game = None @>

module CardTests =

    type CardValueGen() = 
        static member CardValue() = Arb.fromGen <| Gen.choose (1, 13)

    [<Property(Verbose=true, Arbitrary=[|typeof<CardValueGen>|] )>]
    let ``Can add cards together`` suit number =  

        let topCard = Card.create (number - 1) suit
        let bottomCard = Card.create number suit
         
        (number <> 1) ==> lazy (
            test <@ 
                    let topCard = Option.get topCard
                    let bottomCard = Option.get bottomCard

                    Card.canAddCardToRun bottomCard topCard @>)

    [<Property(Verbose=true, Arbitrary=[|typeof<CardValueGen>|] )>]
    let ``Can not add the same card onto itself`` suit number =  

            test <@ 
                    let card = Card.create number suit |> Option.get
                    Card.canAddCardToRun card card |> not @>

    [<Property(Verbose=true, Arbitrary=[|typeof<CardValueGen>|] )>]
    let ``Can not add King onto any card`` suit number =  

            test <@ 
                    let card = Card.create number suit |> Option.get
                    let king = Card.create 13 suit |> Option.get
                    Card.canAddCardToRun card king |> not @>

    [<Property(Verbose=true, Arbitrary=[|typeof<CardValueGen>|] )>]
    let ``Can not add any card onto a Ace`` suit number =  

            test <@ 
                    let card = Card.create number suit |> Option.get
                    let ace = Card.create 1 suit |> Option.get
                    Card.canAddCardToRun ace card |> not @>

    [<Property(Verbose=true, Arbitrary=[|typeof<CardValueGen>|] )>]
    let ``Can not add card that is not of adjacent value`` suit first second =  

            let skipEqualOrAdjacentValues = 
                let difference = first - second |> float
                difference ** 2.

            skipEqualOrAdjacentValues > 1. ==> 
                lazy (test <@ 
                            let firstCard = Card.create first suit |> Option.get
                            let secondCard = Card.create second suit |> Option.get
                            if first > second then 
                                Card.canAddCardToRun firstCard secondCard |> not 
                            else 
                                Card.canAddCardToRun secondCard firstCard |> not 
                            @>)
module TableauTests =

    type CardValueGen() = 
        static member CardValue() = Arb.fromGen <| Gen.choose (1, 13)

    type TableauNonEmpty() = 
        static member Tableau() = Arb.fromGen <| GameGenerators.tableau false 

    type Tableau() = 
        static member Tableau() = Arb.fromGen <| GameGenerators.tableau true 

    [<Property(Verbose=true, Arbitrary=[|typeof<CardValueGen>|] )>]
    let ``Can add cards together`` suit number =  

        let topCard = Card.create (number - 1) suit
        let bottomCard = Card.create number suit
         
        (number <> 1) ==> lazy (
            test <@ 
                    let topCard = Option.get topCard
                    let bottomCard = Option.get bottomCard

                    Tableau.canAddCardToTab bottomCard topCard @>)

    [<Property(Verbose=true, Arbitrary=[|typeof<CardValueGen>|] )>]
    let ``Can not add the same card onto itself`` suit number =  

            test <@ 
                    let card = Card.create number suit |> Option.get
                    Tableau.canAddCardToTab card card |> not @>

    [<Property(Verbose=true, Arbitrary=[|typeof<CardValueGen>|] )>]
    let ``Can not add King onto any card`` suit number =  

            test <@ 
                    let card = Card.create number suit |> Option.get
                    let king = Card.create 13 suit |> Option.get
                    Tableau.canAddCardToTab card king |> not @>

    [<Property(Verbose=true, Arbitrary=[|typeof<CardValueGen>|] )>]
    let ``Can not add any card onto a Ace`` suit number =  

            test <@ 
                    let card = Card.create number suit |> Option.get
                    let ace = Card.create 1 suit |> Option.get
                    Tableau.canAddCardToTab ace card |> not @>

    [<Property(Verbose=true, Arbitrary=[|typeof<CardValueGen>|] )>]
    let ``Can not add card that is not of adjacent value`` suit first second =  

            let varience = 
                let difference = first - second |> float
                difference ** 2.

            varience > 1. ==> 
                lazy (test <@ 
                            let firstCard = Card.create first suit |> Option.get
                            let secondCard = Card.create second suit |> Option.get
                            if first > second then 
                                Tableau.canAddCardToTab firstCard secondCard |> not 
                            else 
                                Tableau.canAddCardToTab secondCard firstCard |> not 
                            @>)

    [<Property(Verbose=true, Arbitrary=[|typeof<CardValueGen>|] )>]
    let ``Can add card to tableau that is of different suit`` suitOne suitTwo number =  

        (number <> 1 && suitOne <> suitTwo) ==> 
            lazy (test <@ 
                        let firstCard = Card.create (number - 1) suitOne |> Option.get
                        let secondCard = Card.create number suitTwo |> Option.get
                        Tableau.canAddCardToTab secondCard firstCard
                        @>)

    [<Property(Verbose=true, MaxTest = 10 )>]
    let ``Card is always added to head of Tableau and marked visible`` (card: Card) tableau =  

        test <@ 
                Tableau.addCard (fun _ _ -> true) id tableau card 
                |> Tableau.firstCard 
                |> Option.get = card
                @>

    [<Property(Verbose=true, MaxTest = 10, Arbitrary=[|typeof<TableauNonEmpty>|] )>]
    let ``Card is not added when not allowed`` (card: Card) tableau =  

        test <@ 
                let newTab = Tableau.addCard (fun _ _ -> false) id tableau card
                newTab = tableau
                @>

    [<Property(Verbose=true, MaxTest = 10, Arbitrary=[|typeof<CardValueGen>|] )>]
    let ``Given a list of ascending cards then getRun should return them all`` suit cardValue =  

        let expectedCount = 13 - cardValue + 1
        test <@ 
                [1..13] 
                |> List.filter (fun x -> x >= cardValue) 
                |> List.choose (fun x -> Card.create x suit)
                |> (fun xs -> {Visible = xs; Hidden = []})
                |> Tableau.getRun
                |> List.length = expectedCount
            @>

    [<Property(Verbose=true, MaxTest = 5  )>]
    let ``Given a tab when getRun is called canAddCard should be called in order`` suit =  

        test <@
                [1..13]  
                |> List.choose (fun x -> Card.create x suit)
                |> (fun xs -> {Visible = xs; Hidden = []})
                |> Tableau.getRun
                |> List.length = 13
            @>

    [<Property(Verbose=true, MaxTest = 50, Arbitrary=[|typeof<TableauNonEmpty>|] )>]
    let ``Given visible cards when getRun then 1 card should be returned`` tableau =  
        test <@ tableau |> Tableau.getRun |> List.isEmpty |> not @>

module GameMoverTests =
        
    type Tableau() = 
        static member Tableau() = Arb.fromGen <| GameGenerators.tableau true 

    type TableauWithRun() = 
        static member Tableau() = Arb.fromGen GameGenerators.tableauWithRun

    type GameTableaus() = 
        static member Game() = Arb.fromGen <| GameGenerators.gameWithTableaus (GameGenerators.tableau true)

    type GameTableausNonEmpty() = 
        static member Game() = Arb.fromGen <| GameGenerators.gameWithTableaus (GameGenerators.tableau false)

    type StockGen() = 
        static member Stock() = Arb.fromGen <| Gen.listOfLength 10 GameGenerators.card

    [<Property(MaxTest = 20, Arbitrary=[|typeof<GameTableausNonEmpty>|])>]
    let ``Given a game when getRuns is called then there is a run for every tab`` game =
        test <@ game |> GameMover.getRuns |> List.length = 10 @>

    [<Property(MaxTest = 20, Arbitrary=[|typeof<GameTableaus>|])>]
    let ``Given a game when getRuns is called then each run has at least one card`` game =
        test <@ game 
                |> GameMover.getRuns 
                |> List.map (snd >> List.length) 
                |> List.forall (fun x -> x>= 1) @>

    [<Property(MaxTest = 20, Arbitrary=[| typeof<GameTableaus> |])>]
    let ``GetRuns are the first cards in each tab from the game`` game =

        test <@ 
                game 
                |> GameMover.getRuns 
                |> List.forall (fun (c, t) -> 
                    c 
                    |> Game.getTabForColumn game 
                    |> Tableau.getVisible 
                    |> List.take (List.length t) = t)
                @>

    [<Property(MaxTest = 50, Arbitrary=[| typeof<GameTableaus>; typeof<StockGen> |])>]
    let ``Given a game canStock can be played if the column contains a visible card`` game stock =
        let gameWithStock = {game with Stock = stock}

        test <@ 
                gameWithStock 
                |> GameMover.canPlayStock 
                |> Option.isSome
                |> (fun x -> x = (game |> Game.getAllTabs 
                                 |> List.map Tableau.getVisible 
                                 |> List.map List.isEmpty 
                                 |> List.reduce (||)
                                 |> not )) @>

    [<Property(MaxTest = 5, Arbitrary=[| typeof<GameTableaus> |])>]
    let ``Given with no stock cannot play stock move`` game =
        test <@ game |> GameMover.canPlayStock |> Option.isNone @>

    [<Property(Verbose=true, MaxTest = 50, Arbitrary=[|typeof<GameTableaus>|])>]
    let ``PlayMove does does not change total card count`` game = 

        let isMove = function | Move _ -> true | _ -> false
        let moves = game |> GameMover.validMoves |> List.filter (isMove)
        let cardCount game = game |> Game.getAllTabs |> List.map Tableau.length |> List.sum

        (List.length moves >= 1) ==> 
            lazy (
                    let move = List.head moves 
                                |> (function | Move c -> Some c | _ -> None) 
                                |> Option.get
            
                    (test <@ 
                            game 
                            |> Game.playMove move
                            |> Option.map cardCount
                            |> Option.exists (fun newCardCount -> newCardCount = cardCount game) @>))

    [<Property(Verbose=true, MaxTest = 50, Arbitrary=[|typeof<GameTableaus>|])>]
    let ``PlayMove changes only two columns by the same length`` game = 

        let isMove = function | Move _ -> true | _ -> false
        let moves = game |> GameMover.validMoves |> List.filter (isMove)

        (List.length moves >= 1) ==> 
            lazy (
                    let move = List.head moves 
                                |> (function | Move c -> Some c | _ -> None) 
                                |> Option.get
            
                    (test <@ 
                            game 
                            |> Game.playMove move
                            |> Option.map (Game.getAllTabs )
                            |> Option.map (fun allTabs -> List.zip allTabs (Game.getAllTabs game))
                            |> Option.map (List.map (fun (x,y) -> x = y, Math.Abs (Tableau.length x - Tableau.length y)))
                            |> Option.map (List.filter (fst >> not))
                            |> Option.map (List.map snd)
                            |> Option.exists (fun xs -> 
                                xs |> List.length = 2 && 
                                    (List.head xs = (xs |> List.skip 1 |> List.head)) ) 
                                @>))