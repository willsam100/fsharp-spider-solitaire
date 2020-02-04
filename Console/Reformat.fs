module Reformat

// #r "/Users/willsam100/projects/SpiderSolitare/SpiderSolitare/bin/Debug/netstandard2.0/SpiderSolitare.dll"
// #r "/Users/willsam100/projects/SpiderSolitare/Console/bin/Debug/netcoreapp3.0/Console.dll"

open System
open System.IO
open System.Collections.Generic
open SpiderSolitare.Representation
open SpiderSolitare.Game
open SpiderSolitare.MonteCarloTreeSearch

let moveEncoder = ActionEncoder()
let gameDecoder = CardEncoderKeyed()

[<Struct>]
type Policy = {
    Game: string
    Move: string
    MoveOrder: int
    GameNumber: int
    RowNumber: int
    MoveCount: int
}

[<Struct>]
type Value = {
    Game: string
    Reward: int
    // MoveOrder: int
    GameNumber: int
    RowNumber: int
}

[<Struct>]
type LegacyFile = {
    Game: string
    Move: int
    Reward: bool
    MoveOrder: int
    GameNumber: int
    RowNumber: int
}

let encodeMove (m: int16[]) = 
    let move = int16 1
    m |> Array.findIndex (fun x -> x = move)

let decodeMove int = 
    let x = Array.replicate 1171 0
    x.[int] <- 1
    x

let format (game:string) = 
    let game = game.Split ","
    let tableau = game |> Array.take (96 * 10) |> String.concat ","
    let stock = game |> Array.skip (96 * 10) |> Array.truncate 10 |> String.concat ","
    sprintf "%s,%s" tableau stock

let sequenceDataLegacy rowNumber (x: string) = 
    let row = x.Split ","
    let game = row |> Array.skip 1 |> Array.take (row.Length - 1171 - 2) |> String.concat ","
    let move = row |> Array.skip (row.Length - 1171 - 2) |> Array.take 1171 |> Array.map Int16.Parse
    let outcome = 
        match row.[row.Length - 2] with 
        | "true" ->  true
        | "false" ->  false
        | _ -> failwith "Software error for reading files!"
    let moveOrder = Array.head row |> Int32.Parse
    let gameNumber = Array.last row |> Int32.Parse

    {
        Game = game
        Move = move |> encodeMove
        Reward = outcome
        MoveOrder = moveOrder
        GameNumber = gameNumber
        RowNumber = rowNumber
    }   

let sequenceDataPolicy rowNumber (x: string) = 
    let row = x.Split ","

    let moveOrder = Array.head row |> Int32.Parse
    let game = row |> Array.skip 1 |> Array.take (row.Length - 2) |> String.concat ","
    let move = row.[row.Length - 2] |> Int32.Parse |> decodeMove |> Array.map string  |> String.concat ","
    let moveCount = row.[row.Length - 2] |> Int32.Parse
    let gameNumber = Array.last row |> Int32.Parse

    // format game, move, outcome, gameNumber, moveOrder
    {
        Game = format game
        Move = move
        MoveOrder = moveOrder
        GameNumber = gameNumber
        RowNumber = rowNumber
        MoveCount = moveCount
    } 

let sequenceDataValue rowNumber (x: string) = 

    try 
        let row = x.Split ","
        let game = row |> Array.skip 1 |> Array.take (row.Length - 2) |> String.concat ","
        let reward =  row.[row.Length - 2] |> Int32.Parse
        let moveOrder = Array.head row |> Int32.Parse

        let gameNumber = Array.last row |> Int32.Parse

        if reward <> 1 && reward <> 0 then 
            failwithf "Parsing failed. Reward is not in acceptable range (0 - 1), actual: %d" reward

        {
            Game = format game
            Reward = reward
            // MoveOrder = moveOrder
            GameNumber = gameNumber
            RowNumber = rowNumber
        }
    with 
    | e -> 
        printfn "%s" x
        let row = x.Split ","
        row.Length |> printfn "%d"
        Array.last row |> printfn "%s"

        raise e


let trimCommonMoves maxCount map = 
    map  
    |> Map.map (fun m (gs: Policy []) -> 
        gs
        |> Array.sortByDescending (fun x -> x.RowNumber)
        |> Array.truncate maxCount

    )
let oversample map = 

    let rec oversample targetCount gs = 
        if List.length gs < targetCount then 
            let gs = List.replicate ( targetCount + 1 / gs.Length ) gs |> List.concat 
            oversample targetCount gs
        else
            List.truncate targetCount gs

    let maxCount =
        if Map.count map = 0 then 
            0 
        else 
            map 
            |> Map.toList 
            |> List.map (fun (_, gs) -> gs |> List.distinct |> List.length)
            |> List.max

    printfn "Max: %d" maxCount

    map 
    |> Map.map (fun (m: string) (gs: string list) -> 
        // let gs = List.distinct gs

        if gs.Length = maxCount then 
            gs
        else 
            oversample maxCount gs
    )

let printMoves map = 

    printfn "TotalMoves: %d" <| Map.count map

    map 
    // |> Map.filter (fun k (_, count) -> count = 108)
    |> Map.toList
    |> List.map (fun (move: string, games) -> 
        moveEncoder.Decode <| move.Replace(",", ""), games |> List.distinct |> List.length  )
    |> List.sortByDescending (snd)
    |> List.iter (fun (move, count) -> 
        // printfn "%s" (List.replicate 80 "-" |> String.concat "")
        printfn "%A -> %d" move count 
        // games 
        // |> List.take 10
        // |> List.map (fun (x:string) -> 
        //     x.Split "," |> Array.map Int32.Parse |> Array.toList |>  decodeKeyedGame gameDecoder )
        // |> List.iter (printfn "%A")
    )


let savePolicy map = 
    let rows = 
        map 
        |> Map.toList 
        |> List.collect (fun (m, gs) -> gs |> List.map (fun (g: string) -> 
            sprintf "%s,%s" g m ) )
    File.WriteAllLines ("/Users/willsam100/Desktop/spider-policy-net.csv", rows)

    // Delete the binary file as we have generated a new csv file with more data.
    if File.Exists "/Users/willsam100/Desktop/spider-policy-net.csv-binary.npy" then 
        File.Delete "/Users/willsam100/Desktop/spider-policy-net.csv-binary.npy"

let saveValueNet data = 
    let rows = data |> Array.map (fun (g, outcome) -> sprintf "%s,%d" g outcome )
    File.WriteAllLines ("/Users/willsam100/Desktop/spider-value-net.csv", rows)

type ResultType = 
    | All
    | WonAndLost of winningRowNumbers: int array

let filterDuplicateGamesStateRewards (data: Value[]) = 

    let gameNumbersToWin = 
        data 
        |> Array.groupBy (fun stateAction -> stateAction.GameNumber)
        |> Map.ofArray
        |> Map.map (fun k v ->  

            // let rec splitWhile acc games : Value list list = 
            //     match games with 
            //     | [] -> acc
            //     | game::_ -> 
            //         let (n: Value list) = games |> List.takeWhile (fun x -> x.Reward = game.Reward)
            //         let tail = games |> List.skipWhile (fun x -> x.Reward = game.Reward)
            //         splitWhile (n :: acc) tail

            // let games = 
            //     v
            //     |> Array.sortBy (fun x -> x.RowNumber)
            //     |> Array.toList
            //     |> splitWhile []

            // if games.Length = 1 then 
            //     if v |> Array.forall (fun x -> x.Reward = 1) then 
            //         SingleWin
            //     else Lost
            // else         


            if v |> Array.exists (fun x -> x.Reward = 1) && v |> Array.exists (fun x -> x.Reward = 0) then 
                
                v |> Array.filter (fun x -> x.Reward = 1) |> Array.map (fun x -> x.RowNumber)
            else v |> Array.map (fun x -> x.RowNumber)  )
        |> Map.toArray
        |> Array.collect snd
        |> Set.ofArray


    // let validRowNumbers = 
    data 
    // |> Array.filter (fun x -> gameNumbersToWin |> Set.contains x.RowNumber)
    // |> Array.map (fun stateAction -> stateAction.Game, stateAction.Reward, stateAction.GameNumber, stateAction.RowNumber)
    |> Array.groupBy (fun x -> x.Game)
    // |> Map.ofArray
    // |> Map.map (fun g v ->
    //     v 
    //     |> Array.map (fun (g,r,gn, rn) -> rn, gameNumbersToWin |> Map.find gn) 
    //     // |> Array.distinctBy snd 
    //     )
    |> Array.map (fun (g,v) -> 
        if v |> Array.exists (fun x -> x.Reward = 1) then 
            g,1
        else g,0 )

    // |> Array.collect (fun (g,v) -> v |> Array.distinctBy (fun x -> x.Reward))

        // |> Map.filter (fun g v -> v |> Array.length > 1 )
        // |> Map.map (fun k v -> 
        //     v
        //     |> Array.collect (fun (rn,isWin) -> 
        //         match isWin with 
        //             | All -> [|rn|]
        //             | WonAndLost games -> games
        //         ) )
        // |> Map.toArray
        // |> Array.collect snd
        // |> Set.ofArray

    

let filterDuplicateGamesStateMoves (data: Policy[]) = 

    // data 
    // |> Array.sortBy (fun x -> x.RowNumber, x.MoveOrder) 
    // |> Array.fold (fun acc nextStateMove -> 

    //     match acc with 
    //     |[] -> [[nextStateMove]]
    //     | []::rest -> [nextStateMove] :: rest
    //     | (lastStateMove::tail)::rest -> 
    //         if nextStateMove.MoveOrder < lastStateMove.MoveOrder then 
    //             // next move order was less than the last move. Start a new game since this move does not belong to this current game run
    //             [nextStateMove] :: (lastStateMove :: tail) :: rest
    //         else 
    //             (nextStateMove :: lastStateMove :: tail) :: rest                            
    
    // ) []
    // |> List.collect (fun xs -> 
    //     let l = xs.Length
    //     xs |> List.map (fun x -> x, l))
    // |> List.groupBy (fun (x,_) -> x.Game)
    // |> List.map (fun (k,v) ->  v |> List.minBy (fun (_,x) -> x)  |> fun (x,_) -> x ) 
    // |> List.toArray
    // |> Array.distinct

    data 
    |> Array.groupBy (fun x -> x.Game)
    |> Array.map (fun (g,xs) -> xs |> Array.minBy (fun x -> x.MoveCount))

        
let readAndFormatPolicy file = 

    File.ReadAllLines file 
    |> Array.mapi sequenceDataPolicy
    |> filterDuplicateGamesStateMoves
    |> (fun x -> Array.shuffle x; x)
    |> Array.truncate 2000 // random fast training. 
    |> Array.groupBy (fun x -> x.Move)
    |> Map.ofArray
    // |> trimCommonMoves 1000
    |> Map.map (fun k v -> v |> Array.toList |> List.map (fun x -> x.Game))
    |> oversample
    |> savePolicy

let readAndFormatValue file = 

    File.ReadAllLines file 
    |> Array.mapi sequenceDataValue
    |> filterDuplicateGamesStateRewards
    |> (fun x -> Array.shuffle x; x)
    |> Array.truncate 2000 // random fast training. 

    // |> Array.map (fun x -> x.Game, x.Reward)
    // |> Array.distinctBy (fun (g,_) -> g)
    |> saveValueNet