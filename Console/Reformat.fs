module Reformat

// #r "/Users/willsam100/projects/SpiderSolitare/SpiderSolitare/bin/Debug/netstandard2.0/SpiderSolitare.dll"
// #r "/Users/willsam100/projects/SpiderSolitare/Console/bin/Debug/netcoreapp3.0/Console.dll"

open SpiderSolitare
open System
open System.IO
open System.Collections.Generic
open SpiderSolitare.Representation
open SpiderSolitare.Game
open SpiderSolitare.MonteCarloTreeSearch
open SpiderSolitare.Brain

let log m f = 
    printfn m 
    f

[<Struct>]
type Policy = {
    Game: string
    Move: string
    MoveOrder: int
    GameNumber: int
    RowNumber: int
    MoveCount: int
    ScoredGame: float
    LongestColumn: Column
}

[<Struct>]
type Value = {
    Game: string
    Reward: int
    MoveOrder: int
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

type ColumnGame = {
    Game: Game
    Column: Column
}

let getLongestColumnForGame g = g |> GameMover.getRuns |> List.maxBy (fun (c, cs) -> cs.Length) |> fst

let getLongestColumn tabsize x = 
    x |> gameDecoded tabsize |> getLongestColumnForGame

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
    async {
        do! Async.SwitchToThreadPool()
        
        let row = x.Split ","

        let moveOrder = Array.head row |> Int32.Parse
        let game = row |> Array.skip 1 |> Array.take (row.Length - 2) |> String.concat ","
        let move = row.[row.Length - 3] |> Int32.Parse |> decodeMove |> Array.map string  |> String.concat ""
        let gameNumber = row.[row.Length - 2] |> Int32.Parse
        let moveCount = Array.last row |> Int32.Parse

        return 
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
                    LongestColumn = format13 game |> getLongestColumn 13
                } 
            with 
            | e -> 
                printfn "RowNumber:%d" rowNumber
                raise e    
    }

let sequenceDataColumn rowNumber (x: string) = 
    async {
        do! Async.SwitchToThreadPool()
        
        let row = x.Split ","
        let game = 
            row 
            |> Array.skip 1 
            |> Array.take (96 * 10) 
            |> String.concat ","
            |> gameDecoded 96

        return 
            // format game, move, outcome, gameNumber, moveOrder
            try 
                {
                    Game = game
                    Column = game |> getLongestColumnForGame
                } 
            with 
            | e -> 
                printfn "RowNumber:%d" rowNumber
                raise e    
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
            Game = format96 game
            Reward = reward
            MoveOrder = moveOrder
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
    |> Array.map (fun (m, (gs: 'a [])) -> 
        m, gs |> Array.truncate maxCount
    )

let permuteColumns (game, move:int, nextGame) =
    // async {
    //     do! Async.SwitchToThreadPool()        
        
        let game = game |> gameDecoded 26
        let nextGame = nextGame |> gameDecoded 26
        let move = move |> decodeMove |> Array.map string |> String.concat "" |> moveEncoder.Decode

        let allTransforms =
            (C1, C1) :: // prepend identify to keep the original in the output
                (Coord.allColumns
                    |> List.collect (fun x -> Coord.allColumns |> List.map (fun y -> x,y) )
                    |> List.filter (fun (x,y) -> x <> y) )
                     
        allTransforms
        |> List.map (fun (left, right) ->
            
            let game' =                
                let tabLeft = Game.getTabForColumn game left
                let tabRight = Game.getTabForColumn game right
                Game.updateColumn left tabRight game
                |> Game.updateColumn right tabLeft

            let nextGame' =                
                let tabLeft = Game.getTabForColumn nextGame left
                let tabRight = Game.getTabForColumn nextGame right
                nextGame |> Game.updateColumn left tabRight |> Game.updateColumn right tabLeft
                    
            let move =
                match move with
                | Move c ->
                    match c.From, c.To with
                    | f, t when f = left && t = right -> { c with From = right; To = left } |> Move
                    | f, t when f = right && t = left -> { c with From = left; To = right } |> Move
                    | f, _ when f = left -> { c with From = right; } |> Move
                    | f, _ when f = right -> { c with From = left; } |> Move
                    | _, t when t = left -> { c with To = right; } |> Move
                    | _, t when t = right -> { c with To = left; } |> Move
                    | _, _ -> c |> Move
                | x -> x
                
            game' |> encodeKeyGame cardEncoder |> Seq.map string |> String.concat "," |> format26Stock, 
                move |> moveEncoder.Encode  |> encodeMove, 
                    nextGame' |> encodeKeyGame cardEncoder |> Seq.map string |> String.concat "," |> format26Stock
            )
        |> List.distinct
    // }

let permuteColumnsGame (policy: ColumnGame) =
    async {
        do! Async.SwitchToThreadPool()        
        
        let game = policy.Game

        let allTransforms =
            (C1, C1) :: // prepend identify to keep the original in the output
                (Coord.allColumns
                    |> List.collect (fun x -> Coord.allColumns |> List.map (fun y -> x,y) )
                    |> List.filter (fun (x,y) -> x <> y) )
                
        return         
            allTransforms
            |> List.toArray
            |> Array.map (fun (left, right) ->
                
                let game' =                
                    let tabLeft = Game.getTabForColumn game left
                    let tabRight = Game.getTabForColumn game right
                    Game.updateColumn left tabRight game
                    |> Game.updateColumn right tabLeft

                let longest = 
                    match policy.Column with 
                    | x when x = left -> right
                    | x when x = right -> left
                    | x -> x
                    
                { policy with Game = game';  Column = longest }
                )
            |> Array.distinct
    }
    
let permuteCardsGame (policy: ColumnGame) =
    async {
        do! Async.SwitchToThreadPool()

        let newGame = policy.Game |> encodeGame |> format13

        let makeGame increment = 
            let game' = shortGamePlusOne increment newGame |> gameDecoded 13
            {policy with 
                Game = game'
                Column = game' |> getLongestColumnForGame}

        return         
            [|
                policy
                makeGame 1
                makeGame 2
                makeGame 3
                makeGame 4
                makeGame 5
                makeGame 6
                makeGame 7
                makeGame 8
                makeGame 9
                makeGame 10
            |]
    }

let rec oversample (targetCount: int) (gs: 'a []) =
    let delta = targetCount - gs.Length
    if delta <= 0 then Array.truncate targetCount gs 
    else 
        if delta < gs.Length then 
            Array.append gs (gs |> Array.truncate delta)
        else 
            oversample targetCount (Array.append gs gs)

let getOversampleCount map = 
    map
    |> Array.map (fun (_, gs) -> gs |> Array.distinct |> Array.length)
    |> Array.max

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

let getPolicyStreamWriter () = 
    // File.WriteAllLines ("/Users/willsam100/Desktop/spider-policy-net.csv", rows)
    if File.Exists "/Users/willsam100/Desktop/spider-policy-net.csv" then 
        File.Delete "/Users/willsam100/Desktop/spider-policy-net.csv"

    // Delete the binary file as we have generated a new csv file with more data.
    if File.Exists "/Users/willsam100/Desktop/spider-policy-net.csv-binary.npy" then 
        File.Delete "/Users/willsam100/Desktop/spider-policy-net.csv-binary.npy"

    new StreamWriter("/Users/willsam100/Desktop/spider-policy-net.csv")

let isValidGame (game:Game) = 
    async {
        do! Async.SwitchToThreadPool()
        if game |> Game.getAllTabs |> List.map Tableau.length |> List.sum = 0 then 
            return false, game
        else 
            return 
                game
                |> GameMover.getRuns 
                |> List.map (fun (c, cs) -> c,cs.Length) // Get the length of run for each column
                |> List.groupBy snd // group be length
                |> List.maxBy (fun (x,xs) -> x)  // get the longest run
                |> snd 
                |> List.length = 1 //the longest run should only have 1 column
                , game
    }
    
let isValidGamePolicy (x: ColumnGame) = 
    async {
        let! (a,b) = isValidGame x.Game
        return a, x
    }

let startsWithAce (x: Policy) = 
    let longesColumn = x.Game |> getLongestColumn 13
    x.Game 
    |> gameDecoded 13
    |> GameMover.getRuns
    |> List.find (fun (c,_) -> c = longesColumn )
    |> snd 
    |> fun x -> CardModule.getValue x.[0] = 1

let appendPolicy (writer: StreamWriter) count (outputFormat: string -> string -> string) (rows: ColumnGame []) = 

    printfn "Count for move: %d - max count: %d" rows.Length count

    let outputFormat (g,m) = outputFormat m g

    let zeroOutOtherColumns (x: ColumnGame) = 
        let game =  x.Game 
        let emptyTab = Tableau.create []

        {x with 
            Game = 
                game
                |> Game.getAllTabsWithColumn
                |> List.filter (fun (c,_) -> x.Column <> c)
                |> List.map fst
                |> List.fold (fun game col -> Game.updateColumn col emptyTab game ) game
        }

    rows   
    |> (fun x -> 
        if x.Length > count then 
            failwithf "Not an even distributin for oversampling. Valuable data is being thrown away - Real lenght: %d, Expected oversample count:%d" x.Length count
   
        x )
    // |> Array.map (zeroOutOtherColumns)
    |> Array.distinct
    |> oversample (count)
    |> log "Oversampled"
    |> Array.map (fun x -> x.Game |> encodeGame |> format13, x.Column |> Coord.toInt |> (fun x -> x - 1 ) |> string)
    |> (fun x -> Array.shuffle x; x)
    |> Array.iter (outputFormat >> writer.WriteLine)

    // File.AppendAllLines ("/Users/willsam100/Desktop/spider-policy-net.csv", output)

let saveValueNet data = 
    let rows = data |> Array.map (fun (g, outcome) -> sprintf "%s,%d" g outcome )
    File.WriteAllLines ("/Users/willsam100/Desktop/spider-value-net.csv", rows)

let saveValidMove data = 
    let rows = data |> Array.map (fun (g, validMoves) -> sprintf "%s,%s" g validMoves )
    File.WriteAllLines ("/Users/willsam100/Desktop/spider-moves-net.csv", rows)

let saveValidRunCount data = 
    let rows = data |> Array.map (fun (g, validMoves) -> sprintf "%s,%s" g validMoves )
    File.WriteAllLines ("/Users/willsam100/Desktop/spider-run-net.csv", rows)

    if File.Exists "/Users/willsam100/Desktop/spider-run-net.csv-binary.npy" then 
        File.Delete "/Users/willsam100/Desktop/spider-run-net.csv-binary.npy"

type ResultType = 
    | All
    | WonAndLost of winningRowNumbers: int array

let filterDuplicateGamesStateRewards (data: Value[]) = 

    // let gameNumbersToWin = 
    //     data 
    //     |> Array.groupBy (fun stateAction -> stateAction.GameNumber)
    //     |> Map.ofArray
    //     |> Map.map (fun k v ->  

    //         // let rec splitWhile acc games : Value list list = 
    //         //     match games with 
    //         //     | [] -> acc
    //         //     | game::_ -> 
    //         //         let (n: Value list) = games |> List.takeWhile (fun x -> x.Reward = game.Reward)
    //         //         let tail = games |> List.skipWhile (fun x -> x.Reward = game.Reward)
    //         //         splitWhile (n :: acc) tail

    //         // let games = 
    //         //     v
    //         //     |> Array.sortBy (fun x -> x.RowNumber)
    //         //     |> Array.toList
    //         //     |> splitWhile []

    //         // if games.Length = 1 then 
    //         //     if v |> Array.forall (fun x -> x.Reward = 1) then 
    //         //         SingleWin
    //         //     else Lost
    //         // else         


    //         if v |> Array.exists (fun x -> x.Reward = 1) && v |> Array.exists (fun x -> x.Reward = 0) then 
                
    //             v |> Array.filter (fun x -> x.Reward = 1) |> Array.map (fun x -> x.RowNumber)
    //         else v |> Array.map (fun x -> x.RowNumber)  )
    //     |> Map.toArray
    //     |> Array.collect snd
    //     |> Set.ofArray

    let zeroOutFirstGames xs = 
        if Array.length xs > 5 then 
            xs |> Array.mapi (fun i (x: Value) -> 
                if i <= 5 then {x with Reward = 0} else x
                )
        else xs


    // let validRowNumbers = 
    data 
    |> Array.groupBy (fun x -> x.GameNumber) 
    |> Array.collect (fun (game, games) -> 

        games
        |> Array.sortBy (fun x -> x.RowNumber)
        |> Array.fold (fun acc nextStateMove -> 
            match acc with 
            |[] -> [[nextStateMove]]
            | []::rest -> [nextStateMove] :: rest
            | (lastStateMove::tail)::rest -> 
                if nextStateMove.MoveOrder < lastStateMove.MoveOrder then 
                    // next move order was less than the last move. Start a new game since this move does not belong to this current game run
                    [nextStateMove] :: (lastStateMove :: tail) :: rest
                else 
                    (nextStateMove :: lastStateMove :: tail) :: rest                            
            ) []
        |> List.map List.toArray
        |> List.toArray
        |> fun games -> 
            if games |> Array.concat |> Array.exists (fun x -> x.Reward = 1) then 
                games |> Array.filter (fun xs -> xs |> Array.exists (fun x -> x.Reward = 1 ))    
            else 
                games

        |> Array.concat
    )


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
    |> Array.groupBy (fun x -> x.GameNumber) 
    |> Array.collect (fun (game, games) -> 

        games
        |> Array.map (fun x -> 
            let g = x.Game.Split "," |> Array.map Int32.Parse |> Array.toList |>  decodeKeyedGame cardEncoder
            {x with ScoredGame = reward g} )

        |> Array.fold (fun acc nextStateMove -> 
            match acc with 
            |[] -> [[nextStateMove]]
            | []::rest -> [nextStateMove] :: rest
            | (lastStateMove::tail)::rest -> 
                if nextStateMove.MoveOrder < lastStateMove.MoveOrder then 
                    // next move order was less than the last move. Start a new game since this move does not belong to this current game run
                    [nextStateMove] :: (lastStateMove :: tail) :: rest
                else 
                    (nextStateMove :: lastStateMove :: tail) :: rest                            
            ) []
        |> List.map List.toArray
        |> List.toArray

            // if games |> Array.exists (fun x -> x.ScoredGame >=  0.0025 * 50.) then 
            //     row |> Array.filter (fun xs -> xs |> Array.exists (fun x -> x.ScoredGame >=  0.0025 * 50.) )
            // else 
            //     [||]
        |> Array.maxBy (fun xs ->
                xs |> Array.maxBy (fun x -> x.ScoredGame ) |> (fun x -> x.ScoredGame) )
//        |> Array.concat            
//            
//             |> Array.map (fun x -> 
//             {x with ScoredGame = reward g }
        )
    
    |> Array.groupBy (fun x -> x.Game)
    |> Array.map (fun (g,xs) ->  xs |> Array.maxBy (fun x -> x.ScoredGame) )
    
    
let buildMap (data: Policy[]) =
    data
    |> Array.distinctBy (fun x -> x.GameNumber, x.Move, x.Game)
    |> Array.groupBy (fun x -> x.GameNumber)
    |> Array.map (fun (game, games) ->
        let minGameMoves = 
            games
            |> Array.fold (fun acc nextStateMove -> 
                match acc with 
                |[] -> [[nextStateMove]]
                | []::rest -> [nextStateMove] :: rest
                | (lastStateMove::tail)::rest -> 
                    if nextStateMove.MoveOrder < lastStateMove.MoveOrder then 
                        // next move order was less than the last move. Start a new game since this move does not belong to this current game run
                        [nextStateMove] :: (lastStateMove :: tail) :: rest
                    else 
                        (nextStateMove :: lastStateMove :: tail) :: rest                            
                ) []
            |> List.map List.length
            |> List.min
        game, minGameMoves
        )
    |> Map.ofArray
    
let filterDuplicateeMoves (data: Policy[]) =

    data
    |> Array.groupBy (fun x -> x.Game)
    |> Array.choose (fun (_, games) ->
        match games |> Array.map (fun x -> x.Move) |> Array.distinct |> Array.length with
        | 1 -> Some games
        | _ ->
//            let m = buildMap data
            
            let validMove = 
                games
                |> Array.distinctBy (fun x -> x.Move, x.GameNumber, x.Game)
                |> Array.map (fun x -> x, x.MoveCount)
                |> Array.minBy snd
                |> fst
                |> fun x -> x.Move
                
            games |> Array.filter (fun x -> x.Move = validMove) |> Some )
    |> Array.collect id
    
let filterBottomPerformance (data: Policy[]) =

    data
    |> Array.groupBy (fun x -> x.GameNumber)
    |> Array.collect (fun (g, xs) ->
        xs
        |> Array.sortBy (fun x -> x.MoveCount)
        |> Array.truncate (float xs.Length * 0.5 |> int) )

let outputFormat m (g: string) = 

    if (shortGamePlusOne 0 g |> fun x -> x.Split "," |> Array.forall (fun x -> x = "1")) then 
        failwith "empty game"

    sprintf "%s,%s" (shortGamePlusOne 0 g) m

let formatPolicyData data =
    data
    |> Array.map permuteCardsGame |> Async.Parallel |> Async.RunSynchronously |> Array.collect id
    |> Array.map (isValidGamePolicy) |> Async.Parallel |> Async.RunSynchronously
    |> log "Computed valid games"
    |> Array.filter fst
    |> Array.map snd
    |> Array.distinctBy (fun x -> x.Game)
    |> Array.map (permuteColumnsGame) |> Async.Parallel |> Async.RunSynchronously |> Array.collect id
    |> log "Permuted cards"
    |> fun xs -> 
        printfn "Games Count: %d" xs.Length
        xs
    |> Array.distinctBy (fun x -> x.Game)
    |> (fun x -> Array.shuffle x; x)
    |> Array.groupBy (fun x ->  x.Column )
    |> log "trimmed game states"
    |> fun map -> getOversampleCount map, map
    
let readAndFormatPolicy file = 

    File.ReadAllLines file   
    |> log "Loaded file"
    |> Array.take 3000
    |> Array.mapi sequenceDataColumn
    |> Async.Parallel
    |> Async.RunSynchronously
    |> log "Converted to policy"
    |> formatPolicyData
    |> log "Saving"
    |> fun (oversampleCount, map) -> 
        let writer = getPolicyStreamWriter() 
        writer, oversampleCount, map
    |> fun (writer: StreamWriter, oversampleCount, map) -> 
        map |> Array.iter (fun (m, ms) ->  
            printfn "Move: %A" m
            ms |> appendPolicy writer oversampleCount outputFormat )
        writer.Flush()
        writer.Close()

let readAndFormatValue file = 

    File.ReadAllLines file
    |> Array.take 3000
    |> Array.mapi sequenceDataValue
    |> filterDuplicateGamesStateRewards
    |> Array.map (fun (g,value) -> 
        sprintf "%s,%s" g (shortGamePlusOne 0 g), value )
    // |> (fun x -> Array.shuffle x; x)
    // |> Array.truncate 2000 // random fast training. 

    // |> Array.map (fun x -> x.Game, x.Reward)
    // |> Array.distinctBy (fun (g,_) -> g)
    |> saveValueNet

let readAndFormatValidMoves file = 

    File.ReadAllLines file 
    |> Array.mapi sequenceDataPolicy
    |> Async.Parallel
    |> Async.RunSynchronously
    |> Array.groupBy (fun x -> x.Game)
    |> Array.map (fun g -> 

        let game = 
            (fst g).Split "," 
            |> Array.map Int32.Parse 
            |> Array.toList 
            |> decodeKeyedGame cardEncoder

        let mutliLabelMoves = 
            game
            |> GameMover.validMoves 
            |> List.filter (fun move -> 
                let topCards = game |> Game.getAllTabsWithColumn |> List.choose (fun (c, x) -> List.tryHead x.Visible |> Option.map (fun x -> c,x)) 

                match move with 
                | Stock -> false 
                | Flip _ -> false
                | Move m -> 
                    topCards 
                    |> List.forall (fun (col, card) -> col = m.From && card = m.Card)
                    |> not )
            |> List.map (fun x -> x |> moveEncoder.Encode)
            |> List.fold (fun acc x -> 
                let t = Array.zip acc x 
                t |> Array.map (fun (l,r) -> 
                    let one = int16 1 
                    if l = one || r = one then one else int16 0)
            ) (Array.replicate 1171 (int16 0))

            |> Array.map string 
            |> String.concat ","

        let g = 
            (fst g).Split "," 
            |> Array.take (96 * 10)
            |> Array.chunkBySize 96
            |> Array.collect (Array.truncate 12)
            |> String.concat ","


        g, mutliLabelMoves )   
    |> saveValidMove

let readAndFormatRun file = 

    let oversample xs = 
        xs 
        |> Array.groupBy snd
        |> Array.collect (fun (_,rows) -> 
            let size = 1000

            let rows = Array.truncate 1000 rows
            if rows.Length < size then 
                Array.replicate (size / rows.Length) rows 
                |> Array.concat
                |> Array.truncate size
            else
                rows        
        )

    File.ReadAllLines file 
    |> Array.mapi sequenceDataPolicy
    |> Async.Parallel
    |> Async.RunSynchronously
    |> Array.collect (fun x -> x.Game.Split "," |> Array.take (96 * 10) |> Array.chunkBySize 96)
    |> Array.map (fun x -> 
        let rec count acc xs = 
            match xs with 
            | [] -> acc
            | xHead::xTail -> 
                match acc with 
                | [] -> count [xHead] xTail
                | head::_ -> 
                    if head + 1 = xHead then 
                        count (xHead :: acc) xTail
                    else acc                 
        x |> Array.take 12 |> String.concat ",", 
            x 
            |> Array.map Int32.Parse 
            |> Array.toList 
            |> count [] 
            |> List.length 
            |> string 
        
        )
    |> oversample
    |> saveValidRunCount
