module PolicyCleanData
open SpiderSolitare.Game
open SpiderSolitare.MonteCarloTreeSearch
open Common
open System.IO
open XPlot.GoogleCharts
open System
open SpiderSolitare.Representation
open SpiderSolitare.Brain

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