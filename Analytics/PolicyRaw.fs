module PolicyRaw

open System
open System.IO
open SpiderSolitare.Game
open Common
open SpiderSolitare.MonteCarloTreeSearch
open XPlot.GoogleCharts
open SpiderSolitare.Brain

[<Struct>]
type Policy = {
    Game: string
    Move: string
    MoveOrder: int
    GameNumber: int
    RowNumber: int
    MoveCount: int
    ScoredGame: float
    GameDecoded: Game
    TotalCount: int
    GameSeriesId: Guid
}

let sequenceDataPolicy rowNumber (x: string) = 
    let row = x.Split ","

    let moveOrder = Array.head row |> Int32.Parse
    let game = row |> Array.skip 1 |> Array.take (row.Length - 2) |> String.concat ","
    let move = row.[row.Length - 3] |> Int32.Parse |> decodeMove |> Array.map string  |> String.concat ","
    let gameNumber = row.[row.Length - 2] |> Int32.Parse
    let moveCount = Array.last row |> Int32.Parse
    
    // format game, move, outcome, gameNumber, moveOrder
    try 
        {
            Game = format13 game
            Move = move
            MoveOrder = moveOrder
            GameNumber = gameNumber
            RowNumber = rowNumber
            MoveCount = moveCount
            ScoredGame = 0.
            GameDecoded = Game.emptyGame
            TotalCount = 0
            GameSeriesId = Guid.Empty
        } 
    with 
    | e -> 
        printfn "RowNumber:%d" rowNumber
        raise e
        
let processData file =
    File.ReadAllLines file
    |> Array.mapi sequenceDataPolicy

let setGameSeriesId data =
    
    data
    |> Array.groupBy (fun x -> x.GameNumber)
    |> Array.collect (fun (gn, xs) ->
        
        xs
        |> Array.sortBy (fun x -> x.RowNumber, x.MoveOrder)
        |> Array.fold (fun acc nextStateMove -> 

            match acc with 
            |[] -> [[nextStateMove]]
            | []::rest ->
                if nextStateMove.MoveOrder = 1 then 
                    [nextStateMove] :: rest
                else []::rest 
            | (lastStateMove::tail)::rest ->
                if nextStateMove.MoveOrder < lastStateMove.MoveOrder then
                    
                    if nextStateMove.MoveOrder = 1 then 
                        // next move order was less than the last move. Start a new game since this move does not belong to this current game run
                        [nextStateMove] :: (lastStateMove :: tail) :: rest
                    else
                        [] ::(lastStateMove :: tail) :: rest
                else 
                    (nextStateMove :: lastStateMove :: tail) :: rest                            
        
        ) []
        |> List.map List.toArray
        |> List.toArray
        |> Array.collect (fun xs ->
            let id = Guid.NewGuid()
            xs |> Array.map (fun x -> { x with GameSeriesId = id }) )
        )
    |> Array.sortBy (fun x -> x.RowNumber, x.MoveOrder)
    

let showChartsForRawPolicyData policyRawFile = 
    let data = processData policyRawFile
    
    data
    |> Array.map (fun x -> { x with ScoredGame = x.Game |> gameDecoded 13 |> reward })
    |> Array.sortBy (fun x -> x.RowNumber, x.MoveOrder)
    |> Array.map (fun x ->
        let g = gameDecoded 13 x.Game
        let m = x.Move.Replace(",", "" ) |> moveEncoder.Decode
        
        let result =
            match GameMover.playMove m g with
            | Continue (g, _) -> reward g
            | Won g -> reward g
            | Lost g ->
                if m = Stock then 0. else -1.

        printfn "RN:%d GN:%d MO:%d MC:%d TC:%d S:%f NS:%f"
            x.RowNumber x.GameNumber x.MoveOrder x.MoveCount x.TotalCount x.ScoredGame result
        x, result
        )
    |> fun xs -> 
            [
                xs |> Array.map (fun (x, _) -> x.RowNumber, x.GameNumber :> value )
                xs |> Array.map (fun (x, _) -> x.RowNumber, x.MoveOrder :> value )
                xs |> Array.map (fun (x, _) -> x.RowNumber, x.MoveCount :> value)
                xs |> Array.map (fun (x, result) -> x.RowNumber, result * 100. :> value )
            ]
    |> Chart.Table
    |> Chart.WithLabels ["Row Number"; "GameNumber"; "MoveOrder"; "MoveCount"; "NextScore"]
    |> Chart.Show
    
    data
    |> Array.map (fun x ->
        let g = gameDecoded 13 x.Game
        let m = x.Move.Replace(",", "" ) |> moveEncoder.Decode
        
        let result =
            match GameMover.playMove m g with
            | Continue (g, _) -> reward g
            | Won g -> reward g
            | Lost _ -> if m = Stock then 0. else -1.
    
        let score = Math.Min(1.5, (result) * 100.)
        { x with ScoredGame =  score })
    |> Array.sortBy (fun x -> x.RowNumber, x.MoveOrder)
    |> Array.map (fun x -> float x.MoveOrder / float x.MoveCount, x.ScoredGame)
    |> Chart.Scatter
    |> Chart.WithSize (1200, 1000)
    |> Chart.Show
            
            
    data
    |> Array.sortBy (fun x -> x.RowNumber, x.MoveOrder)
    |> Array.groupBy (fun x ->
        let g = gameDecoded 13 x.Game
        x.Move.Replace(",", "" ) |> moveEncoder.Decode)
    |> Array.map (fun (x, xs) -> sprintf "%A" x, xs.Length)
    |> Array.sortByDescending snd
    |> Chart.Bar
    |> Chart.WithSize (1200, 1000)
    |> Chart.Show

    data
    |> Array.map (fun x -> x.Move.Replace(",", "" ) |> moveEncoder.Decode )
    |> Array.choose (fun x ->     
        match x with 
        | MoveType.Move c -> Some c.Card
        | MoveType.Flip _ -> None
        | MoveType.Stock -> None )
    |> Array.groupBy id
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
    
    
    data
    |> setGameSeriesId
    |> Array.groupBy (fun x -> x.GameSeriesId)
    |> Array.sortByDescending (fun (x, xs) -> 
        let gameNumber = xs |> Array.head |> fun x -> x.GameNumber
        let tc = xs |> Array.map (fun x -> x.MoveCount) |> Array.distinct
        gameNumber, tc
    )
    |> Array.map (fun (x, xs) -> 
        let gameNumber = xs |> Array.head |> fun x -> x.GameNumber
        let tc = xs |> Array.map (fun x -> string x.MoveCount) |> Array.distinct |> String.concat ","
        sprintf "%d - %s" gameNumber tc, xs.Length)
    |> Chart.Bar
    |> Chart.WithSize (1200, 2000)
    |> Chart.Show


    data
    |> setGameSeriesId
    |> Array.map (fun x -> x.Move.Replace(",", "" ) |> moveEncoder.Decode, x )
    |> Array.choose (fun (move, x) ->     
        match move with 
        | MoveType.Move c -> (c, x) |> Some
        | MoveType.Flip _ -> None
        | MoveType.Stock -> None )


    |> Array.groupBy (fun (_, x) -> x.GameSeriesId)
    |> Array.sortByDescending (fun (x, xs) -> 
        let gameNumber = xs |> Array.head |> snd |> fun x -> x.GameNumber
        let tc = xs |> Array.map (fun (_, x) -> x.MoveCount) |> Array.distinct
        gameNumber, tc
    )
    |> Array.map (fun (_, xs) -> 

        let averageCardCountMove = 
            xs 
            |> Array.averageBy (fun (move, x) -> 
                let game = x.Game |> gameDecoded 13

                let countOfCardsMoved = 
                    game 
                    |> GameMover.getRuns
                    |> List.find (fun (c, _) -> c = move.From)
                    |> snd
                    |> List.findIndex (fun x -> x = move.Card)
                    |> fun x -> x + 1 // starts at zero index so add 1 to get count of cards moved
                    |> float

                countOfCardsMoved )          

        xs.Length, averageCardCountMove )
    |> Chart.Scatter
    |> Chart.WithSize (1200, 1000)
    |> Chart.Show
