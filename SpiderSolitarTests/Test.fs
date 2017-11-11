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
        createGame deck |> Game.getAllTabs |> List.take 4 |> List.map Tableau.length |> List.sum |> should be (equal (6 * 4))

    [<Test>]
    member x.``last five tableaus have 5 cards`` () =
        createGame deck |> Game.getAllTabs |> List.skip 4 |> List.map Tableau.length |> List.sum |> should be (equal (5 * 6))

    //[<Test>]
    //member x.``Each card only appears twice`` () =

        //let generatedGame = createGame deck
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

        let generatedGame = createGame deck
        let countOfTableus = generatedGame |> Game.getAllTabs |> List.map (Tableau.length) |> List.sum
        let stockCount = generatedGame |> (fun x -> x.Stock) |> List.length

        countOfTableus + stockCount |> should be (equal 104)

    [<Test>]
    member x.``Tableaus contain 54 cards`` () =
        createGame deck |> Game.getAllTabs |> List.map (Tableau.length) |> List.sum |> should be (equal 54)

    [<Test>]
    member x.``Tableaus contain 50 cards`` () =
        createGame deck |> getStock |> List.length |> should be (equal 50)


//[<TestFixture>]
//type GameMoverTest() = 

    //[<Test>]
    //member x.``moves are returned`` () =
    //    createGame deck |> validMoves |> Seq.length |> should be (greaterThanOrEqualTo 2)

    //[<Test>]
    //member x.``no run moves are returned`` () =
        //createGame deck |> allRunMoves |> List.length |> should be (equal 0)


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
            let gameCards = cards |> List.sort //|> List.map (fun c -> c, Visible) |> faceUpCards

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

    //[<Property(Verbose=true, Arbitrary=[|typeof<CardValueGen>|] )>]
    //let ``Can not add card to RUN that is of different suit`` suitOne suitTwo number =  

        //(number <> 1 && suitOne <> suitTwo) ==> 
            //lazy (test <@ 
                        //let firstCard = Card.create (number - 1) suitOne |> Option.get
                        //let secondCard = Card.create number suitTwo |> Option.get
                        //Tableau.canAddCardToTab secondCard firstCard |> not
                        //@>)

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

    //[<Property(Verbose=true, MaxTest = 10, Arbitrary=[|typeof<Tableau>|] )>]
    //let ``Get first card returns visible cards if card present`` tableau visible =  

        //test <@ 
                //let expected visible firstCard = 
                //    match tableau, visible with
                //    | _, Hidden -> firstCard = None
                //    | [], _ -> firstCard = None
                //    | _, Visible ->
                //        firstCard |> Option.get = Tableau.firstCard tableau 
                    
                //tableaus
                ////|> List.map (fun (c: Card) -> c, visible) 
                //|> Tableau.firstCard 
                //|> expected visible
                //@>

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

    //[<Property(Verbose=true, MaxTest = 10, Arbitrary=[|typeof<CardValueGen>|] )>]
    //let ``Given a tab when getRun called with a count all the same count of cards is returned`` tableau count =  

        //(List.length tableau > count) ==> 
            //lazy (
            //        let lastCard = tableau |> List.skip count |> List.head |> fst
            //        let expectedCount = tableau |> List.take count |> List.length
            //        test <@ 
            //                tableau
            //                |> List.map (fun (c,_:Movablity) -> (c, Visible))
            //                |> Tableau.getRun
            //                |> List.length = expectedCount
            //@>)

    //[<Property(Verbose=true, MaxTest = 20 )>]
    //let ``Given a tab when getRun called only visible cards are returned`` tableau (NonNegativeInt count) =  

        //(List.length tableau > count) ==> 
            //lazy (
            //        let expectedCount = count
            //        test <@ 
            //                tableau
            //                |> List.mapi (fun i (c, _: Movablity) -> if (i + 1) <= count then (c,Visible) else (c, Hidden))
            //                |> Tableau.getRun (fun c _ -> true) = 
            //                          (tableau |> List.take count |> List.map (fun (c, _) -> c, Visible))
            //@>)

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





    //[<Property(Verbose=true, Arbitrary=[|typeof<Tableau>|])>]
    //let ``Calculating faceup cards never has a blocked card on top`` (cards: (Card * Movablity) list) =     

        //let validCards = cards |> List.filter (not << cardMoveabiltiyIs Hidden)

        //validCards
        //|> faceUpCards 
        //|> function
        //| [] -> true
        //| xs -> xs |> List.head |> (not << cardMoveabiltiyIs Visible)

    //[<Property(Verbose=true, Arbitrary=[|typeof<Tableau>|])>]
    //let ``Calculating faceup cards returns only faceup cards in the same order`` (cards: (Card * Movablity) list) =     

        //let getCard (c, _) = c  
        //let validCards = cards |> List.filter (not << cardMoveabiltiyIs Hidden)
        //let hiddenCards = cards |> List.filter (cardMoveabiltiyIs Hidden)
        //let result = (validCards @ hiddenCards |> faceUpCards |> List.map getCard)
        //printfn "%A" result

        //validCards |> List.map getCard = result

    //[<Property(Verbose=true, Arbitrary=[|typeof<Tableau>|])>]
    //let ``Calculating faceup cards returns blocked when suit is not the same`` (cards: (Card * Movablity) list) =     

        //let getCard (c, _) = c  
        //let validCards = cards |> List.filter (not << cardMoveabiltiyIs Hidden)

        //let shouldHaveVisible = validCards |> List.map (getCard >> Card.getDetails >> snd) |> Set.ofList |> Set.count > 2

        //let assertion () = 
        //    let result = validCards |> faceUpCards
        //    let values = result |> List.mapi (fun i (_, m) -> i, m = Visible)
        //    let firstVisible = values |> List.skipWhile (fun (i, b) -> not b) |> List.head |> fst

        //    values |> List.skip firstVisible |> List.fold (fun acc (i, x) -> x = false) true

        //shouldHaveVisible ==> lazy (assertion ())

    //[<Property(Verbose=true, Arbitrary=[|typeof<TableauWithRun>|])>]
    //let ``Calculating faceup cards returns InRun When cards are in a run`` (cards: (Card * Movablity) list) =     

        //let result = cards |> faceUpCards

        //let assertion() =
        //    printfn "running assertion"
        //    let values = result |> List.mapi (fun i (_, m) -> i, m = Visible)
        //    let firstVisibleOrHiden = values |> List.skipWhile (fun (i, b) -> not b) |> List.head |> fst

        //    let firstIsVisible = result |> List.head |> snd = Visible
        //    let secondIsInRun = result |> List.skip 1 |> List.head |> snd = InRun

        //    let areOfSameSuit = 
        //        let folder acc (c, m) = 
        //            if m = Visible || m = InRun && List.isEmpty acc then [c]
        //            else 
        //                let lastSuit = acc |> List.head |> Card.getDetails |> snd
        //                let suit = c |> Card.getDetails |> snd

        //                if m = Visible || m = InRun && lastSuit = suit then 
        //                    c :: acc
        //                else 
        //                    acc

        //        let countOfSameSuit = result |> List.fold folder [] |> List.length
        //        let countOfVisibleAndInRun = 
        //            result |> List.takeWhile (fun x -> cardMoveabiltiyIs Visible x || cardMoveabiltiyIs InRun x) |> List.length

        //        countOfSameSuit = countOfVisibleAndInRun

        //    if result |> List.filter (cardMoveabiltiyIs Visible) |> List.length >= 1 then values |> List.skip firstVisibleOrHiden |> List.fold (fun acc (i, x) -> x = false) true
        //    else true
        //    && firstIsVisible && secondIsInRun && areOfSameSuit

        //result |> List.filter (cardMoveabiltiyIs InRun) |> List.length > 1 ==> lazy (assertion ())   


    //[<Property(Verbose=true, Arbitrary=[|typeof<TableauWithRun>|])>]
    //let ``A run is always of the same suit`` tableau = 
            
    //    let run = tableau |> Tableau.getRun |> List.map fst
    //    Seq.length run <> 0 ==> lazy (run |> Seq.map (fun (Card (n, s)) -> s) |> Set.ofSeq |> Seq.length|> (fun length -> length = 1))

    //[<Property(Verbose=true, Arbitrary=[|typeof<TableauWithRun>|])>]
    //let ``The length of a run is correct`` tableau = 
    //    let allVisibleCards = tableau |> List.where (fun (c,v) -> v = Visible) |> List.length
    //    let listOfExpectedLengths = 0 :: [2..allVisibleCards]

    //    tableau |> Tableau.getRun |> Seq.length |> (fun length -> listOfExpectedLengths |> List.contains length)

    //[<Property(Verbose=true, Arbitrary=[|typeof<TableauWithRun>|])>]
    //let ``A run is always acending by one in card value`` tableau = 
            
        //let run = tableau |> Tableau.getRun |> List.map fst

        //printfn "Run: %A" run

        //let cardValueIsAcending run = 
        //    let getCardValue (Card ((CardValue cv), _)) = cv
    
        //    let lastCardValue = run |> Seq.last |> getCardValue
        //    let firstCardValue = run |> Seq.head |> getCardValue

        //    printfn "%d - %d = %d" lastCardValue firstCardValue (Seq.length run)
    
        //    lastCardValue - firstCardValue + 1 = Seq.length run

        //List.length run <> 0 ==> lazy (cardValueIsAcending run)

    //[<Property(Verbose=true, Arbitrary=[|typeof<TableauWithRun>|])>]
    //let ``Given only a subset of a run can be moved to all other columns then Nine valid moves should be returned`` tableau = 

        //let lengthOfRun = tableau |> Tableau.getRun |> List.length
        //let generator = Gen.choose (2, lengthOfRun - 1)

        //let assertion split = 
        //    lengthOfRun > 2 ==> lazy
        //        let otherCard = List.skip split tableau |> List.head
        //        test <@
        //                {emptyGame with Tableaus = (tableau) :: (List.replicate 9 [otherCard])}
        //                |> allRunMoves 
        //                |> List.length = 9 @>

        //Prop.forAll (Arb.fromGen generator) assertion


    //[<Property(Verbose=true, MaxTest = 20, Arbitrary=[|typeof<GameTableausNonEmpty>|])>]
    //let ``Valid moves is correct for a single card without empty tableaus `` game = 
    //    let enabled = true

    //    let safeTake take (xs: (Card *Movablity) list) = 
    //        if List.length xs >= take then List.take take xs else []

    //    test <@ game |> Game.getAllTabs 
    //        |> List.map (fun tab -> List.takeWhile (hasMovability Visible) tab |> safeTake 1)
    //        |> List.concat
    //        |> List.sort
    //        |> List.map getCardValue
    //        |> List.groupBy (fun x -> x)
    //        |> List.pairwise
    //        |> List.filter (fun (x,y) -> fst x + 1 = fst y)
    //        |> List.map (fun ((_, x), (_, y))  -> (List.length x) * (List.length y))
    //        |> List.sum = (validMoves game |> List.filter (function | Single x-> true | _ -> false) |> Seq.length) @>

    //[<Property(MaxTest = 5)>]
    //let ``Stock move is added when there are cards`` hasStock = 

    //    let result = 
    //        deck 
    //        |> createGame 
    //        |> (fun g -> {g with Stock = if hasStock then g.Stock else []}) 
    //        |> validMoves 
    //        |> List.contains Stock

    //    hasStock = result

    //[<Property(Verbose=true, Arbitrary=[|typeof<GameTableausNonEmpty>|])>]
    //let ``Stock move is not added when there is an empty space`` emptyTableau game column = 

    //    let game =
    //        if emptyTableau then
    //            let tabs = game |> Game.getAllTabsWithColumn |> List.map (fun (c, tab) -> c, if c = column then [] else tab)
    //            Game.updateTableaus tabs game
    //        else 
    //            game
            
    //    let result = 
    //        game 
    //        |> validMoves 
    //        |> List.contains Stock

    //    emptyTableau = not result

    //[<Property(Verbose=true)>]
    //let ``Game mover only accepts valid moves`` move = 

        //let game = createGame deck
        //let moves = game |> validMoves 

        //let f x = x

        //moves |> List.contains move = false ==> lazy (playMove f game move = None)

    //[<Property(Verbose=true, Arbitrary=[|typeof<TableausNonEmpty>; typeof<StockGen>|])>]
    //let ``Game mover moves correct card`` tabs cards = 

        //let game = {emptyGame with Stock = cards; Tableaus = tabs} 
        //let assertion () = 
        //    let isPlayable = 
        //        function 
        //        | Lost lg -> false
        //        | Won -> false
        //        | Continue newGame -> 
    
        //            let getColumnForOperator op = 
        //                List.zip (newGame.Tableaus |> List.map List.length) (game.Tableaus |> List.map List.length)
        //                |> List.mapi (fun i (x,y) -> (i, op x y)) 
        //                |> List.filter (fun (i, y) -> y) 
        //                |> List.map (fun (i, x) -> i) 
        //                |> List.head
    
        //            let getTopCard g column = 
        //                g.Tableaus |> List.skip column |> List.head |> List.head
    
        //            let longerColumn = getColumnForOperator (>)  
        //            let shorterColumn = getColumnForOperator (<)
    
        //            game <> newGame && getTopCard game shorterColumn = getTopCard newGame longerColumn

        //    game
        //    |> validMoves |> List.filter isSingleMove 
        //    |> List.map (fun x -> playMove (fun x -> x) game x |> Option.map isPlayable |> function | Some x -> x | None -> false)
        //    |> List.fold (&&) true

        //game |> validMoves |> List.filter isSingleMove |> List.length > 0 ==> lazy (assertion ())

    //[<Property(Verbose=true, Arbitrary=[|typeof<TableausNonEmpty>; typeof<StockGen>|])>]
    //let ``Game mover updates card movability`` tabs cards = 

        //let upateMoveCards g = 
        //    let tabs = g.Tableaus |> List.map (fun tab -> tab |> List.map (fun (x,y) -> x, Hidden))
        //    {g with Tableaus = tabs}

        
        //let game = {emptyGame with Stock = (if List.length cards > 10 then cards else []); Tableaus = tabs} 
        //let assertion () = 
        //    let isPlayable = 
        //        function 
        //        | Won -> printfn "Game Won; Impossilbe. Bug Found!"; false
        //        | Lost lostGame -> 
        //                let hasOneColumen = game.Tableaus |> List.filter (fun x -> List.length x = 1) |> List.length |> (fun x -> x >= 1)
        //                let stockIsEmtpy = List.isEmpty lostGame.Stock 
        //                hasOneColumen || stockIsEmtpy
            
        //        | Continue newGame -> 

        //            let allAreHidden = 
        //                newGame.Tableaus 
        //                |> List.map (List.map snd)
        //                |> List.concat 
        //                |> List.filter (fun x -> x <> Hidden) 
        //                |> List.length 
        //                |> (fun x -> x = 0)

        //            let getAllCards g =
        //                let tabs = 
        //                    g.Tableaus 
        //                    |> List.map (List.map fst)
        //                    |> List.concat
        //                tabs @ g.Stock

        //            printfn "%b length: %d, length %d" allAreHidden (game |> getAllCards |> List.length) (newGame |> getAllCards |> List.length)
        //            allAreHidden && game |> getAllCards |> List.length = (newGame |> getAllCards |> List.length)

        //    game
        //    |> validMoves
        //    |> List.map (fun x -> playMove upateMoveCards game x |> Option.map isPlayable |> function | Some x -> x | None -> true)
        //    |> List.fold (&&) true

        //let thereIsASingleMove = 
        //    game |> validMoves |> List.length > 0 
            
        //thereIsASingleMove ==> assertion ()


    //type SmallInt() = 
        //static member SamllInt() = Arb.fromGen <| Gen.choose (2, 4)

    //[<Property(Verbose=true, Arbitrary=[|typeof<TableausNonEmpty>; typeof<SmallInt>|])>]
    //let ``Playing a run moves the correct number of cards`` tabs size = 

    
        //let suit = tabs |> List.head |> List.head |> fst |> Card.getSuit
        //let run = [1..5] |> List.map (fun x -> (Card.create x suit), InRun)

        //let card = run |> List.take size |> List.last |> fst 
        //let move = Run (card, {To = C1; From = C10})
        //let tabs = 
        //    tabs 
        //    |> List.mapi (fun i tab -> if i = 9 then run @ tab else tab)
        //    |> List.mapi (fun i tab -> if i = 0 then 
        //                                 (((Card.create ((Card.getValue card) + 1) suit), Visible) :: tab) |> faceUpCards 
        //                               else tab)

        //let game = {emptyGame with Tableaus = tabs}
        //let getCards g = g |> List.map fst
        //let getSectionOfRun f g = g.Tableaus |> List.head |> f size |> getCards


        //let assertion g = 
        //    g.Tableaus |> List.head |> List.length = size + (game.Tableaus |> List.head |> List.length) &&
        //    (getSectionOfRun List.take g) = (run |> getCards |> List.take size) &&
        //    (getSectionOfRun List.skip g) = (game.Tableaus |> List.head |> getCards)

        //playMove id game move |> Option.exists (function 
            //| Won -> false
            //| Lost g -> assertion g
            //| Continue g -> assertion g )

    //[<Property(Verbose=true, MaxTest=1, Arbitrary=[|typeof<TableauWithRun>|])>]
    //let ``When a card in a run is played then there are only cards of immdidate ascending order`` tab = 

        
        //let game = {emptyGame with Tableaus = tabs}

        //let getGameTableau index = game.Tableaus |> List.skip index |> List.head

        //let isNextCardOfAscendingValue bottomCard topCard = 
        //    Card.getValue bottomCard = Card.getValue topCard + 1

        //let assertion = 
        //    let isCorrectOrder tabSubset = 
        //        tabSubset |> Log.printrtrnfn Info |> List.fold (fun acc card -> 
        //            if List.isEmpty acc then (true, card) :: []
        //            else 
        //                let topCard = acc |> List.head |> snd
        //                if isNextCardOfAscendingValue card topCard then 
        //                    (true, card) :: acc
        //                else (false, card) :: acc) []
        //        |> List.map fst
        //        |> List.reduce (&&)

        //    let getTargetChangedColumn = 
        //        List.filter (fun x -> List.length x > 0) 
        //        >> List.mapi (fun i x -> i, x)
        //        >> List.skipWhile (fun (i, tab) -> List.length <| getGameTableau i <= List.length tab)
        //        >> List.head

        //    let getTabSubset (i, tab) = 
        //        let gameTab = getGameTableau i
        //        Tableau.getSubset (List.length tab - List.length gameTab + 1) tab

        //    let getGame = function 
        //    | Won -> emptyGame.Tableaus
        //    | Lost game -> game.Tableaus 
        //    | Continue game -> game.Tableaus 

        //    Option.exists (getGame >> List.map Tableau.getCards >> getTargetChangedColumn >> getTabSubset >> isCorrectOrder)

        //let allMoves = game |> allRunMoves 
        //allMoves |> List.length > 0 ==> lazy 
            ////allMoves |> List.head |> Log.printrtrnfn Info |> List.map (fun m -> playMove id game m) |> List.map assertion |> List.reduce (&&)
            //allMoves |> List.head |> Log.printrtrnfn Info |> (fun m -> playMove id game m) |> assertion

    //[<Property(Verbose=true, Arbitrary=[|typeof<StockGen>; typeof<TableausNonEmpty>|])>]
    //let ``Game mover can play deck move`` stock tabs = 

        //let ensureGameShouldNeverBeLost = stock @ stock
        //let game = {emptyGame with Stock = ensureGameShouldNeverBeLost; Tableaus = tabs}
        //let isPlayable = 
        //    function 
        //    | Lost lg -> false
        //    | Won -> false
        //    | Continue newGame -> 

        //        let oldStock = newGame.Tableaus |> List.map (fun x -> List.head x |> fst)

        //        (newGame.Stock |> List.length) + 10 = List.length game.Stock && 
        //        game.Stock |> List.take 10 = oldStock

        //playMove (fun x -> x) game Stock |> Option.map isPlayable |> Option.isSome

    //[<Property(Verbose=true, Arbitrary=[|typeof<Column>; typeof<GameCardGen>|])>]
    //let ``A single card can be moved to an empty colum`` ((card, m): GameCard) col = 

        //let enabled = true

        //let game = 
        //    let tabs = (List.replicate 10 List.empty) |> List.mapi (fun i x -> if i = col then [(card, Visible)] else x)
        //    {emptyGame with Tableaus = tabs}

        //let moves = 
        //    game |> validMoves

        //log enabled moves |> ignore
        //moves |> List.length = 9

    //[<Property(Verbose=true, MaxTest = 20, Arbitrary=[|typeof<Column>;|])>]
    //let ``A game can be won`` suit col1 col2 = 

        //let enabled = true 

        //let lastCards = (Card (CardValue 2, suit), Visible) :: ([3..13] |> List.map (fun value -> (Card (CardValue value, suit)), InRun))
        //let topCard = (Card (CardValue 1, suit), Visible)

        //let tabs = 
        //    List.replicate 10 List.empty
        //    |> List.mapi (fun i x -> 
        //        match i with 
        //        | x when x = col1 -> lastCards
        //        | x when x = col2 -> [topCard]
        //        | _ -> x)

        //let game = 
        //    let gameWithTabs = {emptyGame with Tableaus = tabs; Clubs = Two; Hearts = Two; Spades = Two; Diamonds = Two} 

        //    match suit with
        //    | H -> {gameWithTabs with Hearts = One;}
        //    | S -> {gameWithTabs with Spades = One;}
        //    | D -> {gameWithTabs with Diamonds = One}
        //    | C -> {gameWithTabs with Clubs = One}

        //let toSingleItem = 
        //    function
        //    | [] -> None
        //    | x::[] -> Some x 
        //    | xs -> None

        //let assertion =
        //    function 
        //    | Some Won -> true 
        //    | _ -> false

        //let isSingle = 
        //    function
        //    | Single x -> true
        //    | _ -> false

        //col1 <> col2 ==> lazy (game 
            //|> log enabled
            //|> validMoves 
            //|> List.filter isSingle
            //|> log enabled
            //|> List.map (playMove (fun x -> x) game)
            //|> List.map (log enabled)
            //|> List.map (assertion)
            //|> List.map (log enabled)
            //|> List.reduce (||))

    //[<Property(Verbose=true, MaxTest=10, Arbitrary=[|typeof<Column>; typeof<TableausNonEmpty>|])>]
    //let ``A completed tableau is cleared`` suit col1 col2 split tableaus = 

        //let enabled = true
        //let inline logg x = log enabled x
        //let inline logV x = logg x |> ignore 

        //let printCols game = 
        //    let tabs = game.Tableaus |> List.skip 1 |> List.take 2
        //    logV tabs
        //let logColumns enabled result = 
        //    result |> Option.iter (fun result -> 
        //                            match result with 
        //                            | Won -> printfn "Won"
        //                            | Lost g -> logV "Lost"; printCols g
        //                            | Continue g -> logV "Continue";  printCols g;)
        //    result

        //let cards = ([1..13] |> List.map (fun value -> (Card (CardValue value, suit)), InRun))

        //let tabs = 
        //    tableaus
        //    |> List.mapi (fun i tab -> 
        //        match i with 
        //        | x when x = col1 -> (cards |> List.take split) @ tab |> faceUpCards
        //        | x when x = col2 -> (cards |> List.skip split) @ tab |> faceUpCards
        //        | _ -> tab)

        //let game = {emptyGame with Tableaus = tabs}

        //printCols game

        //let isSingle = 
        //    function
        //    | Single x -> true
        //    | _ -> false

        //let cardCount g = 
        //    g.Tableaus |> List.map List.length |> List.sum

        //let oneOfSuitsCompleted = 
        //    function
        //    | {Hearts = One} -> true
        //    | {Clubs = One} -> true
        //    | {Spades = One} -> true
        //    | {Diamonds = One} -> true
        //    | _ -> false

        //let assertion g = 
        //    game.Tableaus |> List.map List.length |> List.iter (printf "%d")
        //    printfn ""
        //    g.Tableaus |> List.map List.length |> List.iter (printf "%d")
        //    printfn "%b %d %d" (oneOfSuitsCompleted g) (cardCount g + 13) (cardCount game)
        //    oneOfSuitsCompleted g && (cardCount g) + 13 = (cardCount game)

        //let filterInvalidGames acc x = 
        //    match x with 
        //    | None -> acc
        //    | Some x -> 
        //        match x with 
        //        | Won -> acc
        //        | Lost x -> x :: acc
        //        | Continue x -> x :: acc

        //col1 <> col2 ==> lazy (
            //game 
            //|> validMoves
            //|> List.map (playMove updateCardMoveablity game)
            //|> List.map (logColumns enabled)
            //|> List.fold filterInvalidGames []
            //|> List.map assertion
            //|> (log false)
            //|> List.reduce (||) )


    //[<Property(Verbose=true, MaxTest=50, Arbitrary=[|typeof<Column>|])>]
    //let ``A run with two cards of the same type terminates a run`` suit split = 

    //    let run = [1..13] |> List.mapi (fun i x -> i, createGameCard x suit Visible)
    //              |> List.fold (fun acc (i, x) -> 
    //                    if i = (split + 1) && List.isEmpty acc |> not then 
    //                        let last = List.head acc
    //                        x :: last :: acc
    //                    else x :: acc ) []
    //              |> List.rev

    //    logV true run
    //    let newRun =  run |> faceUpCards |> (log true)

    //    (newRun |> List.takeWhile (fun (x, m) -> m = Visible) 
    //    |> (log true) 
    //    |> List.length |> (fun x ->  x < 13)) && (newRun |> List.length = 14)

    //[<Property(Verbose=true, MaxTest=5, Arbitrary=[|typeof<GameTableaus>|])>]
    //let ``Suit is never changed when there is no completed run`` game hearts spades clubs diamonds = 

    //    let game = { game with Hearts = hearts; Spades = spades; Diamonds = diamonds; Clubs = clubs }
    //    let newGame = GameMover.clearCompleteRuns Game.getTabForColumn (fun _ -> None) Game.clearCompletedRuns game
    //    newGame = game

    //[<Property(Verbose=true, MaxTest=5, Arbitrary=[|typeof<GameTableausNonEmpty>|])>]
    //let ``Suit is updated when run is completed`` game hearts clubs diamonds = 

    //    let game = { game with Hearts = hearts; Diamonds = diamonds; Clubs = clubs }
    //    let tabs = Game.getAllTabs game
    //    let getCompleted tab = if tab = List.head tabs then Some (tab, Suit.S) else None
    //    let updateSuit g c t s = Game.completeSuit g s 

    //    let newGame = GameMover.clearCompleteRuns Game.getTabForColumn getCompleted updateSuit game
    //    test <@ 
    //            newGame.Clubs = game.Clubs && 
    //            newGame.Diamonds = game.Diamonds &&
    //            newGame.Spades = One &&
    //            newGame.Hearts = game.Hearts @>

    //[<Property(Verbose=true, MaxTest=50, Arbitrary=[|typeof<GameTableausNonEmpty>|])>]
    //let ``Tableau is updated when run is completed`` game column = 

        //let randomTab = Game.getTabForColumn column game
        //let updateTab g c t s = Game.updateColumn c t game

        //let getCompleted tab = if tab = randomTab then Some ([], Suit.S) else None

        //let newGame = GameMover.clearCompleteRuns Game.getTabForColumn getCompleted updateTab game

        //test <@
                //newGame 
                //|> Game.getAllTabs 
                //|> List.filter Tableau.isEmpty 
                //|> List.length 
                //|>  (fun x -> x = 1) @>


//module Serilization = 
    //open Serilization
    //open GameMoverTests

    //[<Property(Verbose=true, Arbitrary=[|typeof<Tableaus>; typeof<StockGen>|])>]
    //let ``round trip for compression returns same game`` tabs stock hearts clubs diamonds spades = 
        //let game = {Stock = stock; Tableaus = tabs; Hearts = hearts; Clubs = clubs; Diamonds = diamonds; Spades = spades}

        //game 
        //|> compressGame 
        //|> decompressGame 
        //|> Option.exists (fun roundTrippedGame -> game = roundTrippedGame)


        
