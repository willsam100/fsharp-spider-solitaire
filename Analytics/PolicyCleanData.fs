module PolicyCleanData
open SpiderSolitare.Game
open SpiderSolitare.MonteCarloTreeSearch
open Common
open System.IO
open XPlot.GoogleCharts
open System
open SpiderSolitare.Representation
open SpiderSolitare.Brain
open XPlot.Plotly
open SpiderSolitare.Representation
[<Struct>]
type Policy = {
    RawString: string
    Game: Game
    GameShifted: string
    Column: Column
}

// let gameDecoded (game: string) = 
//     let game = game.Split "," |> Array.toList |> List.map Int32.Parse 
//     let tableau = 
//         game 
//         |> List.take (13 * 10) 
//         |> List.chunkBySize 13
//         |> List.collect (fun xs -> 
//             let emtpy =  List.replicate (96 - xs.Length) 1
//             List.append xs emtpy
//             )
//         // |> String.concat ","
//     // let stock = game |> Array.skip (24 * 10) |> Array.truncate 10 |> String.concat ","
//     // sprintf "%s,%s" tableau stock
//     tableau
//     // |> fun x -> x.Split ","
//     |> decodeKeyedGame gameDecoder

let sequenceDataPolicy rowNumber (x: string) = 
    let row = x.Split ","

    try 
        let game = row |> Array.take 130 |> String.concat "," |> gameDecoded 13
        // let gameshifted = row |> Array.skip 240 |> Array.take 240 |> String.concat "," //|> gameDecoded
        // let move = row |> Array.tryLast |> Option.get |> moveEncoder.Decode
        let column = row |> Array.tryLast |> Option.get |> int |> (fun x -> x + 1) |> Coord.parseColumn
    
        {
            RawString = x
            Game = game
            GameShifted = "" //gameshifted
            Column = column 
        } 
    with 
    | e -> 
        printfn "RowNumber:%d" rowNumber
        printfn "Length: %d" row.Length
        printfn "%s" x
        raise e
        
let processData file =
    File.ReadAllLines file
    |> Array.mapi sequenceDataPolicy

let showChartsForPolicyCleanData policyCleanFile = 
    let data = processData policyCleanFile


    // data
    // |> Array.toList
    // |> List.map (fun x -> 
    //     try 

    //         let name = x.GetHashCode() |> string
    //         let (c, l) = 
    //             x.Game
    //             |> GameMover.getRuns
    //             |> List.map (fun (x,y)->  x, List.length y)
    //             |> List.maxBy snd

    //         name, c, x.Column, l
    //     with 
    //     | e -> 
    //         printfn "%A" x.Game
    //         printfn "%s" x.RawString
    //         raise e        
    // )
    // |> List.groupBy (fun (x,c,col, l) ->  c)
    // |> List.sortBy fst
    // |> List.map (fun (c, xs) -> 
    
    //     printfn "%A %A" c
    //         (xs |> List.map (fun (_,_,c,_) -> c) |> List.distinct )

    //     c, xs)

    // |> List.map (snd >> List.map (fun (x,y,_, z) -> x,z))
    // |> fun x -> 
    //     x
    //     |> Chart.Bar
    //     |> Chart.WithSize (3000, 4000)
    //     |> Chart.WithLabels ["C1";"C2";"C3";"C4";"C5";"C6";"C7";"C8";"C9";"C10";   ]
    //     |> Chart.Show

    data
    |> Array.groupBy (fun x -> x.Column)
    |> Array.sortBy fst
    |> Array.map (fun (x, xs) -> sprintf "%A" x, float xs.Length / float data.Length)
    |> fun x -> 
        x
        |> Chart.Bar
        |> Chart.WithSize (1200, 1000)
        |> Chart.Show

        x
        |> Chart.Pie
        |> Chart.WithSize (1200, 1000)
        |> Chart.WithLegend true
        |> Chart.Show

let getFirst (a,b,c) = a
let getSecond (a,b,c) = b
let getThird (a,b,c) = c

let validate file = 
    let lines = File.ReadAllLines file
    Array.shuffle lines

    printfn "%d" lines.Length

    lines 
    // |> Array.truncate 1000 //  (965707 / 8)
    |> Seq.map (fun (row:string) -> 
        let items = row.Split "," |> Array.map int |> Array.toList
        let game = 
            items 
            |> List.skip (1 + 10 + 10 + 2) 
            |> decodeOneHotGame 13 1

        let value = 
            items 
            |> List.skip (1 + 10 + 10) 
            |> List.take 2

        let targetCards  = [Card(1, S); ]

        let moves = 
            game
            |> GameMover.validMoves
            |> List.filter (MoveType.foldMove false (fun c -> targetCards |> List.contains c.Card))

        // let move = 
        //     items 
        //     |> List.head
        //     |> MoveEncoding.oneHotToMove game
        //     |> Option.get

        let validMoves = game |> GameMover.validMoves
        game, validMoves, value, moves
        )
    |> Seq.iter (fun (game, validMoves, value, moves) -> 


        if value = [0;1] then 

            let encodeMove encode m = 
                let column, cTo, value = 
                    m |> MoveType.foldMove (C1, C1, -1) (fun c -> c.From, c.To, c.Card |> CardModule.getValue)
                
                let valueOneHot = encode 2 (value - 1)
                printfn "V:%d %A" value valueOneHot
                valueOneHot

            let v = 
                match moves with 
                | [] -> 
                    OneHot.makeEmpty 2 |> OneHot.toString
                | [m] -> encodeMove OneHot.oneHotString m
                | moves -> 
                    printfn "encoding moves"
                    let encodedMoves = moves |> List.map (encodeMove OneHot.toOneHot)
                    OneHot.logicalOr encodedMoves
            printfn "%s" v

            printfn "%A" game
            printfn "Val:%A" value
            printfn "All Moves:\n%A" validMoves
            printfn "Filtered  Moves:\n%A" moves
            printfn "Moves:\n%A" value
            printfn "--" 
        )

let plotValue file = 
    let lines = File.ReadAllLines file
    Array.shuffle lines

    printfn "%d" lines.Length

    let rows = 
        lines 
        // |> Array.truncate 1000 //  (965707 / 8)
        |> Array.map (fun (row:string) -> 
            let items = row.Split "," |> Array.map int |> Array.toList
            let game = 
                items 
                |> List.skip (MoveEncoding.oneHotViaCountCount * 2)
                |> decodeOneHotGame 13 1

            let move = 
                items 
                |> List.take MoveEncoding.oneHotViaCountCount
                |> OneHot.toInt
                |> MoveEncoding.oneHotToMove game
                |> Option.get

            move |> MoveType.foldMove 0 (MoveEncoding.getMoveCount game) )
        |> Array.groupBy id
        |> Array.map (fun (c, xs) -> c, xs |> Array.length)
        // |> Array.map (fun (x,y) -> string x, y |> Array.map (fst >> float) |> Array.average)
        
        // |> Array.choose (fun x -> 

        //     match x.Split "," with 
        //     | [|y; x; z |] -> (Some (int x, int z, float y))
        //     | _ -> None )
        // |> Array.groupBy getSecond
        // |> Array.map (fun (x,xs) -> 

        //     Scatter(
        //         x = (xs |> Array.map getFirst),
        //         y = (xs |> Array.map getThird),
        //         name = string x,
        //         mode = "markers"
        //     ))
        // |> Array.truncate 100

    // let scatter = 
    //     Scatter3d(
    //         x = (rows |> Array.map getFirst), 
    //         z = (rows |> Array.map getSecond),  
    //         y = (rows |> Array.map getThird), 
    //         text = "trace 1",
    //         mode = "markers",
    //         marker =
    //             Marker(
    //                 size = 12.,
    //                 line =
    //                     Line(
    //                         color = "rgba(217, 217, 217, 0.14)",
    //                         width = 0.5
    //                     ),
    //                 opacity = 0.8
    //             ) )

    // scatter
    // |> Array.singleton
    // rows
    // |> Chart.Plot
    // |> Chart.WithSize (1200, 1000)
    // |> Chart.Show

    // rows 
    // // |> Array.filter (fun (m,g) -> g |> Game.getAllTabs |> List.head |> Tableau.getVisible |> List.head = Card(13, S) )
    // |> Array.iter (fun (m,g) -> 
    //     printfn "%A" g

    //     // g 
    //     // |> GameMover.validMoves
    //     // |> List.iter (printfn "%A")

    //     printfn "%A ---------------------------" m
    // )

    rows
    |> Array.map (fun (x,y) -> string x, y)
    |> Chart.Column
    |> Chart.WithSize (1200, 1000)
    |> Chart.Show

    