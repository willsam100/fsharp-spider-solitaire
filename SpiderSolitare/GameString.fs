module SpiderSolitare.GameString
open SpiderSolitare.Game
open System

type CardStatus = 
    | Known of Card
    | Unknown of Card
    | Empty

module CardStatus = 
    let toOption = function | Known c -> Some c | Unknown c -> Some c | _ -> None    

    let isKnown = function | Known _ -> true | _ -> false
    let isUnknown = function | Unknown _ -> true | _ -> false
    let isEmpty = function | Empty _ -> true | _ -> false

module List = 
    let removeItemAtIndex i list = 
        let head, tail = List.splitAt i list
        head @ List.tail tail

    let insertAtIndex i x xs = 
        let head, tail  = List.splitAt i xs
        head @ x :: tail

    let replaceAtIndex i x xs = 
        let head, tail  = List.splitAt i xs
        match tail with 
        | [] -> head @ [x]
        | _::xs -> head @ x :: xs

    let foldUntil f xs = 
        let rec loop f acc xs = 
            match xs with 
            | [] -> acc
            | x::xs -> 
                let r, x = f x
                let acc = x :: acc
                if r then List.rev xs @ acc
                else loop f acc xs 
        loop f [] xs
        
let parseNumber suit (input: string) = 
    match Int32.TryParse input with 
    | true, x -> 
        if x >= 1 && x <= 13 then 
            Card (x, suit)
        else failwithf "Bad number: %s" input
    | false, _ -> failwithf "String is not valid number: %s" input

let parseSuit (input:String) = 
    match input.[0] with 
    | 's' -> S
    | 'h' -> H
    | 'd' -> D
    | 'c' -> C
    | x ->  failwithf "Bad card: %c" x

let parseCard (input: string) = 
    let input = input.ToLower().Trim().Replace("k", "13").Replace("q", "12").Replace("j", "11").Replace("a", "1")
    match input.Length with 
    | 1 -> parseNumber S input 
    | 2 | 3 -> 
        if input.ToCharArray() |> Array.exists Char.IsLetter then 
            let suit = parseSuit input
            input.Substring(1) |> parseNumber suit
        else 
            parseNumber S input
    | _  -> failwithf "Invalid card format: %s" input

let parseCardString (input: string) = 
    match input with 
    | "-" -> Empty
    | _ -> parseCard input |> Known
    
// Can the game string be used at all? 
let isValidString suitCount (input: string) = 
    input.Split "\n" 
    |> Array.map (fun x -> x.Trim().Split " ")
    |> Array.concat
    |> Array.map (fun x -> x.ToLower().Trim())
    |> Array.filter (String.IsNullOrEmpty >> not)
    |> Array.forall (fun x -> 
        try parseCard x |> ignore; true
        with | _ -> false
    )

let rec invertRowToColumns cols rows = 
    match rows with 
    | [] -> cols
    | xs -> 
        let xs = 
            xs 
            |> List.map (function 
                | [] -> failwithf "Bad input: %A" xs
                | x::xs -> x, xs)
        let col = xs |> List.map fst
        let cols = (col :: cols)
        xs 
        |> List.map snd 
        |> List.filter (List.isEmpty >> not)
        |> invertRowToColumns cols

let padRows columCount (rows: string list list) = 
    rows 
    |> List.map (fun x -> 
        let itemsToAdd = columCount - x.Length
        if itemsToAdd = 0 then x 
        else 
            x @ List.replicate itemsToAdd "-" )

let rowsToTabs (rows: CardStatus list list) = 
    rows 
    |> List.map (fun xs -> 
        // let visible = xs |> List.takeWhile (CardStatus.isKnown) |> List.choose CardStatus.toOption
        // let hidden = xs |> List.skipWhile (CardStatus.isKnown >> not) |> List.choose CardStatus.toOption
        // {Visible = visible; Hidden = hidden} 
        xs 
        |> List.choose CardStatus.toOption
        |> Tableau.create )
    |> List.mapi (fun i tab -> Coord.parseColumn (i + 1), tab)

let removeUsedCards deck (cards: CardStatus list list) = 
    let cards = cards |> List.concat |> List.choose CardStatus.toOption

    deck
    |> List.fold (fun (deck, cards) x -> 
        cards |> List.tryFindIndex (fun y -> x = y)
        |> Option.bind (fun cardIndex -> 
            deck 
            |> List.tryFindIndex (fun y -> x = y)
            |> Option.map (fun deckIndex -> 
                let deck = List.removeItemAtIndex deckIndex deck
                let cards = List.removeItemAtIndex cardIndex cards
                deck, cards ) )
        |> Option.defaultValue (deck, cards)
    ) (deck, cards)
    |> fst


let populateUnkownCards deck (cards: CardStatus list list) = 
    let cardsInsertCount = cards |> List.concat |> List.filter (fun x -> x = Empty) |> List.length

    let cards = 
        deck 
        |> List.take cardsInsertCount
        |> List.fold (fun cards card -> 
            cards
            |> List.foldUntil (fun tab -> 
                tab 
                |> List.tryFindIndex (fun x -> x = Empty)
                |> Option.map (fun i -> true, tab |> List.replaceAtIndex i (Unknown card))
                |> Option.defaultValue (false, tab) ) 
            |> List.rev
        ) cards
    deck |> List.skip cardsInsertCount, cards

let padToLength l x (xs: 'a list) = 
    let missingcount = l - xs.Length 
    xs @ List.replicate missingcount x

let padInput (cards: CardStatus list list) = 
    let head, tail = 
        cards 
        |> padToLength 10 []
        |> List.splitAt 4
    let head = head |> List.map (padToLength 6 Empty)
    let tail = tail |> List.map (padToLength 5 Empty)
    head @ tail

let splitInput (input: string) =   
    input.Split "\n" 
    |> Array.map (fun x -> x.Trim().Split " ")
    |> Array.map Array.toList
    |> Array.toList
    |> List.map (List.filter (String.IsNullOrEmpty >> not))
    |> List.filter (List.isEmpty >> not)

let padDeck (inputDeck: CardStatus list list) = 
    inputDeck
    |> padToLength 5 []
    |> List.map (padToLength 10 Empty)

let toRows inputCards = 
    let rows = splitInput inputCards 
            
    invertRowToColumns [] rows 
    |> List.rev
    |> List.map (List.map parseCardString)
    |> List.map List.rev

let parseGame suitCount (inputCards: string, inputDeck: string) = 
    let stock = splitInput inputDeck |> List.map (List.map parseCardString)
    let rows = toRows inputCards

    let deck = CardModule.deck suitCount |> List.replicate 2 |> List.concat
    let deck = removeUsedCards deck rows |> (fun d -> removeUsedCards d stock)

    let deck, rows = rows |> padInput |> populateUnkownCards deck
    let deck, stock = stock |> padDeck |> populateUnkownCards deck

    if deck <> [] then 
        failwithf "Deck is not empty. Rows:\n%A\nStock:\n%A\nDeck:%A" rows stock deck

    rows
    |> rowsToTabs
    |> fun tabs -> 
        Game.updateTableaus tabs Game.emptyGame |> Game.setStock (stock |> List.concat |> List.choose CardStatus.toOption)

               

