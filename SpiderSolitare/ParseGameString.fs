module SpiderSolitare.GameString
open SpiderSolitare.Game
open System

let parseNumber suit (input: string) = 
    match Int32.TryParse input with 
    | true, x -> 
        if x >= 1 && x <= 13 then 
            Card (x, suit) |> Some
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
    let input = input.ToLower().Trim()
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
    | "-" -> None
    | _ -> parseCard input
    
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

let rowsToTabs (rows: Card list list) = 
    rows 
    |> List.map Tableau.create
    |> List.mapi (fun i tab -> Coord.parseColumn (i + 1), tab)

let parseGame (input: string) = 
    let rows = 
        input.Split "\n" 
        |> Array.map (fun x -> x.Trim().Split " ")
        |> Array.map Array.toList
        |> Array.toList

    let maxLength = rows |> List.map List.length |> List.max
    let rows = padRows maxLength rows
            
    invertRowToColumns [] rows 
    |> List.rev
    |> List.map (List.choose parseCardString)
    |> List.map List.rev
    |> rowsToTabs
    |> fun tabs -> Game.updateTableaus tabs Game.emptyGame
        
               
let input = 
    """ s4 s5
        s6 s7 s9"""
