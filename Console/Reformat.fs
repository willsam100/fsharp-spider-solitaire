module Reformat

open SpiderSolitare
open System
open System.IO
open System.Collections.Generic
open SpiderSolitare.Representation
open SpiderSolitare.Game
open SpiderSolitare.MonteCarloTreeSearch
open SpiderSolitare.Brain
open System.Text

module MoveEncoding = 

    let intToMove move = 
        move
        |> decodeMove 
        |> Array.map string 
        |> String.concat ""
        |> moveEncoder.Decode

    let moveToInt move = 
        move |> moveEncoder.Encode |> encodeMove

let log m f = 
    printfn m 
    f

[<Struct>]
type Policy = {
    Game: Game
    Move: int
    MoveOrder: int
    GameNumber: int
    RowNumber: int
    MoveCount: int
    ScoredGame: float
    LongestColumn: Column
}

[<Struct>]
type Value = {
    Game: Game
    Reward: float
    MoveOrder: int
    MoveCount: int
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
    let isDone = 
        match row.[row.Length - 2] with 
        | "true" ->  true
        | "false" ->  false
        | _ -> failwith "Software error for reading files!"
    let moveOrder = Array.head row |> Int32.Parse
    let gameNumber = Array.last row |> Int32.Parse

    {
        Game = game
        Move = move |> encodeMove
        Reward = isDone
        MoveOrder = moveOrder
        GameNumber = gameNumber
        RowNumber = rowNumber
    }   

let sequenceDataPolicy rowNumber (x: string) = 
    async {
        
        let row = x.Split ","

        let moveOrder = Array.head row |> Int32.Parse
        let game = row |> Array.skip 1 |> Array.take 286 |> String.concat "," |> gameDecoded 26
        let move = row.[row.Length - 3] |> Int32.Parse
        let gameNumber = row.[row.Length - 2] |> Int32.Parse
        let moveCount = Array.last row |> Int32.Parse

        return 
            // format game, move, isDone, gameNumber, moveOrder
            try 
                {
                    Game = game 
                    Move = move
                    MoveOrder = moveOrder
                    GameNumber = gameNumber
                    RowNumber = rowNumber
                    MoveCount = moveCount
                    ScoredGame = 0.
                    LongestColumn = getLongestColumnForGame game
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
            // format game, move, isDone, gameNumber, moveOrder
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
        let moveOrder = Array.head row |> Int32.Parse
        let game = row |> Array.skip 1 |> Array.take 286 |> String.concat "," |> gameDecoded 26
        let reward =  row.[row.Length - 2] |> Int32.Parse
        let gameNumber = Array.last row |> Int32.Parse

        if reward <> 1 && reward <> 0 then 
            failwithf "Parsing failed. Reward is not in acceptable range (0 - 1), actual: %d" reward

        {
            Game = game
            Reward = float reward
            MoveOrder = moveOrder
            GameNumber = gameNumber
            RowNumber = rowNumber
            MoveCount = 0
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

let allTransforms =
    (C1, C1) :: // prepend identify to keep the original in the output
        (Coord.allColumns
            |> List.collect (fun x -> Coord.allColumns |> List.map (fun y -> x,y) )
            |> List.filter (fun (x,y) -> x <> y) )

let applyTransform t =     
    allTransforms
    |> List.map (fun (left, right) -> t left right )
    |> List.distinct
    

let transformGame game left right = 
    let tabLeft = Game.getTabForColumn game left
    let tabRight = Game.getTabForColumn game right
    Game.updateColumn left tabRight game
    |> Game.updateColumn right tabLeft

let transformMove move left right = 
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

let permuteColumnsForPolicy (p: Policy) =
    let move = p.Move |> MoveEncoding.intToMove
                 
    let transform left right =        
        let game = transformGame p.Game left right       
        let move = transformMove move left right

        {
            Game = game
            Move = move |> MoveEncoding.moveToInt
            MoveOrder = p.MoveOrder
            GameNumber = p.GameNumber
            RowNumber = p.RowNumber
            MoveCount = p.MoveCount
            ScoredGame = 0.
            LongestColumn = getLongestColumnForGame game
        }
        
    applyTransform transform |> List.toArray

let permuteColumnsForValue (p: Value) =
    let transform left right =        
        left, right, {p with Game = transformGame p.Game left right }
    applyTransform transform |> List.toArray

let permuteColumnsGame (policy: ColumnGame) =
    async {
        do! Async.SwitchToThreadPool()        
        
        let game = policy.Game
                
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

let perumuteCardsForPolicy (p: Policy) = 

    let rec incrementGame policies (p: Policy) = 
        if policies |> List.length = 13 || p.Move = 0 then policies
        else 
            let move = p.Move |> MoveEncoding.intToMove
            match movePlusOne 1 move with 
            | None -> policies
            | Some move -> 
                let policy = 
                    { p with 
                        Game = p.Game |> encodeKeyGame 26 cardEncoder |> Seq.map string |> String.concat "," |> gamePlusOne 1 |> gameDecoded 26
                        Move = move |> MoveEncoding.moveToInt
                    } 
                incrementGame (policy :: policies) policy

    let rec decrementGame policies (p: Policy) = 
        if policies |> List.length = 13 || p.Move = 0 then policies
        else 
            let move = p.Move |> MoveEncoding.intToMove
            match movePlusOne -1 move with 
            | None -> policies
            | Some move -> 
                let policy = 
                    { p with 
                        Game = p.Game |> encodeKeyGame 26 cardEncoder |> Seq.map string |> String.concat "," |> gamePlusOne (-1) |> gameDecoded 26
                        Move = move |> MoveEncoding.moveToInt
                    } 
                incrementGame (policy :: policies) policy

    let up = incrementGame [p] p 
    let down = decrementGame [] p
    up @ down |> List.toArray


let cutoutGame (g: Game) = 

    let columns = 
        g 
        |> Game.getAllTabsWithColumn 
        |> List.choose (fun (c, tab) -> 
            match Tableau.getCardsAfterRun tab with 
            | [] -> None
            | afterRun ->  
                let run = Tableau.getRun tab
                Some (c, run, afterRun))
    let columnIndex = rand.Next(0, columns.Length)

    try 
        let (column, run, afterRun) =  columns.[columnIndex]
        let cardIndex = rand.Next(0, afterRun.Length)
        let card = afterRun.[cardIndex]

        let tab = 
            run @ (afterRun |> List.map (fun x -> 
                if x = card then 
                    CardModule.increment 1 x
                else x ))

        g 
        |> Game.getAllTabsWithColumn 
        |> List.map (fun (c, t) -> 
            if c = column then 
                c, tab |> Tableau.create |> Tableau.unHideCards
            else c,t)
        |> fun x -> Game.updateTableaus x g
    with 
    | e -> 
        printfn "columnIndex: %d" columnIndex
        printfn "Columns length: %A" columns.Length
        printfn "Columns:\n %A" columns
        g.ToString() |> printfn "%s"
        raise e


let rec oversample (targetCount: int) gameOp (gs: ('a * float) list) =
    let lengthGs = List.length gs
    let delta = targetCount - lengthGs
    if delta <= 0 then 
        List.truncate targetCount gs
    else 
        if delta < lengthGs then 
            let cutted = gs |> List.truncate delta |> List.map (fun (g, m) -> gameOp g, m)
            List.append gs cutted
        else 
            let count = delta / lengthGs
            let cutted = gs |> List.replicate count |> List.concat |> List.map (fun (g, m) -> gameOp g, m)
            oversample targetCount gameOp (List.append gs cutted)

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


let saveValueNet data = 
    let rows = data |> Array.map (fun (g, isDone) -> sprintf "%s,%d" g isDone )
    File.WriteAllLines ("/Users/willsam100/Desktop/spider-value-net.csv", rows)

let saveBalancedPolicyNet (writer: StreamWriter) data =
    data |> List.iter (fun (row: string) -> writer.WriteLine row )

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

    let zeroOutFirstGames xs = 
        if Array.length xs > 5 then 
            xs |> Array.mapi (fun i (x: Value) -> 
                if i <= 5 then {x with Reward = 0.} else x
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
            if games |> Array.concat |> Array.exists (fun x -> x.Reward = 1.) then 
                games |> Array.filter (fun xs -> xs |> Array.exists (fun x -> x.Reward = 1. ))    
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
        if v |> Array.exists (fun x -> x.Reward = 1.) then 
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

    

// let filterDuplicateGamesStateMoves (data: Policy[]) = 

//     // data 
//     // |> Array.sortBy (fun x -> x.RowNumber, x.MoveOrder) 
//     // |> Array.fold (fun acc nextStateMove -> 

//     //     match acc with 
//     //     |[] -> [[nextStateMove]]
//     //     | []::rest -> [nextStateMove] :: rest
//     //     | (lastStateMove::tail)::rest -> 
//     //         if nextStateMove.MoveOrder < lastStateMove.MoveOrder then 
//     //             // next move order was less than the last move. Start a new game since this move does not belong to this current game run
//     //             [nextStateMove] :: (lastStateMove :: tail) :: rest
//     //         else 
//     //             (nextStateMove :: lastStateMove :: tail) :: rest                            
    
//     // ) []
//     // |> List.collect (fun xs -> 
//     //     let l = xs.Length
//     //     xs |> List.map (fun x -> x, l))
//     // |> List.groupBy (fun (x,_) -> x.Game)
//     // |> List.map (fun (k,v) ->  v |> List.minBy (fun (_,x) -> x)  |> fun (x,_) -> x ) 
//     // |> List.toArray
//     // |> Array.distinct

//     data 
//     |> Array.groupBy (fun x -> x.GameNumber) 
//     |> Array.collect (fun (game, games) -> 

//         games
//         |> Array.map (fun x -> 
//             let g = x.Game.Split "," |> Array.map Int32.Parse |> Array.toList |>  decodeKeyedGame 96 cardEncoder
//             {x with ScoredGame = reward g} )

//         |> Array.fold (fun acc nextStateMove -> 
//             match acc with 
//             |[] -> [[nextStateMove]]
//             | []::rest -> [nextStateMove] :: rest
//             | (lastStateMove::tail)::rest -> 
//                 if nextStateMove.MoveOrder < lastStateMove.MoveOrder then 
//                     // next move order was less than the last move. Start a new game since this move does not belong to this current game run
//                     [nextStateMove] :: (lastStateMove :: tail) :: rest
//                 else 
//                     (nextStateMove :: lastStateMove :: tail) :: rest                            
//             ) []
//         |> List.map List.toArray
//         |> List.toArray

//             // if games |> Array.exists (fun x -> x.ScoredGame >=  0.0025 * 50.) then 
//             //     row |> Array.filter (fun xs -> xs |> Array.exists (fun x -> x.ScoredGame >=  0.0025 * 50.) )
//             // else 
//             //     [||]
//         |> Array.maxBy (fun xs ->
//                 xs |> Array.maxBy (fun x -> x.ScoredGame ) |> (fun x -> x.ScoredGame) )
// //        |> Array.concat            
// //            
// //             |> Array.map (fun x -> 
// //             {x with ScoredGame = reward g }
//         )
    
//     |> Array.groupBy (fun x -> x.Game)
//     |> Array.map (fun (g,xs) ->  xs |> Array.maxBy (fun x -> x.ScoredGame) )
    
    
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
    |> Array.concat

let filterDuplicateValues (data: Value[]) =

    data
    |> Array.groupBy (fun x -> x.Game)
    |> Array.choose (fun (_, games) -> games |> Array.tryFind (fun x -> x.Reward = 1. ) )
    
let filterBottomPerformance (data: Policy[]) =

    data
    |> Array.groupBy (fun x -> x.GameNumber)
    |> Array.collect (fun (g, xs) ->
        xs
        |> Array.sortBy (fun x -> x.MoveCount)
        |> Array.truncate (float xs.Length * 0.5 |> int) )



let outputFormat m (g: string) = 

    if (gamePlusOne 0 g |> fun x -> x.Split "," |> Array.forall (fun x -> x = "1")) then 
        failwith "empty game"

    sprintf "%s,%s" (gamePlusOne 0 g) m


let inParallel f data = 
    data
    |> Array.map (fun x -> async { return f x })
    |> Async.Parallel
    |> Async.RunSynchronously    

let inParallelConcat f data = 
    inParallel f data
    |> Array.concat



let augment gameNumber prefix file = 
    let xs = File.ReadAllLines file

    let rows = 
        xs
        |> Array.mapi sequenceDataPolicy
        |> Async.Parallel
        |> Async.RunSynchronously
        |> filterDuplicateeMoves
        |> inParallelConcat perumuteCardsForPolicy
        |> inParallelConcat permuteColumnsForPolicy

    printfn "Before %d, After: %d" xs.Length rows.Length

    let targetFile = sprintf "%s/data-%d.csv" prefix gameNumber
    if File.Exists targetFile then
        File.Delete targetFile

    let writer = new StreamWriter(targetFile, false, Encoding.ASCII, 65536)
    let data = 
        rows 
        |> Array.map (fun (x: Policy) -> x.Game, float x.Move) 
        |> Array.filter (fun x -> snd x <> 0.)  
        |> Array.distinct
        |> Array.groupBy snd
        |> Array.map (fun (x,y) -> x, y |> Array.distinct)
        
    let count = 
        data 
        |> Array.filter (fun x -> fst x <> 0.)
        |> Array.map (fun x -> x |> snd |> Array.length)
        |> Array.max

    printfn "Oversample size:%d" count

    let toOutputFormat (game, isDone) =

        let game = 
            game 
            |> encodeKeyGame 26 cardEncoder 
            |> List.truncate (26 * 11)
            |> List.map string 
            |> String.concat ","
        
        sprintf "%d,%s" (int isDone) game

    // let toOutputFormat (game, isDone) =
    //     let game = game |> encodeKeyGame 13 cardEncoder |> List.truncate (13 * 10) |> Seq.map string |> String.concat ","
    //     sprintf "%d,%s" (int isDone) game 

    data 
    |> Array.map (snd >> Array.toList)
    |> inParallel (oversample count cutoutGame >> List.map toOutputFormat)
    // |> inParallel (List.map toOutputFormat)

    |> Array.iter (saveBalancedPolicyNet writer)
    writer.Flush()
    writer.Close()

let groupByGameInstance (values: Value[]) = 
    values
    |> Array.toList
    |> List.sortBy (fun x -> x.RowNumber)
    |> List.fold (fun acc x -> 
        match acc with 
        | [] -> [[x]]
        | []::xs -> [x] :: xs
        | totalMoves::xs -> 
            match totalMoves with 
            | [] -> [x] :: xs
            | z::_ -> 
                if z.MoveOrder > x.MoveOrder then 
                    [x] :: acc
                else 
                    (x :: totalMoves) :: xs
    ) []


let applyToGameInstances f values = 
    values
        |> groupByGameInstance
        |> List.map f
        |> List.concat
        |> List.toArray

let setReward last (v:Value) = 
    {v with Reward = (last * 0.99) + v.Reward }


let transformReward (vs:Value list) = 
    vs 
    
    |> List.map (fun x ->
        let reward = 
            if x.MoveCount = x.MoveOrder then 
                if x.Reward = 0. then -1. else 1.
            else -0.01

        { x with Reward = reward })
    |> List.sortByDescending (fun x -> x.MoveOrder)
    |> List.fold (fun acc next -> 
        match acc with 
        | [] -> [next]
        | last::_ -> setReward last.Reward next :: acc ) []
    |> List.sortBy (fun x -> x.MoveOrder)

let updateMoveCount (values: Value[]) = 
    values 
    |> applyToGameInstances 
        (fun gameInstance -> 
            gameInstance 
            |> List.map (fun x -> { x with MoveCount = gameInstance.Length;})
            |> transformReward
            |> List.toArray
            |> Array.map permuteColumnsForValue
            |> Array.concat
            |> Array.groupBy (fun (x,y,z) -> x,y)
            |> Array.toList
            |> List.map snd
            |> List.map Array.toList
            |> List.concat )
    |> Array.map (fun (_, _, x) -> x)



// let resetScoreForIrrelevantMoves values = 
//     values
//     |> applyToGameInstances 
//         (fun gameInstance ->  

//             gameInstance 
//             |> List.map (fun game -> 

//                 if game.Reward = 0. then 
//                     game
//                 else

//                     let games = gameInstance |> List.filter (fun  x -> x <> game)
//                     let variations = permuteColumnsForValue game |> Array.map (fun x -> x.Game)

//                     let irrelevantGames = 
//                         games 
//                         |> List.filter (fun x -> 
//                             variations |> Array.exists (fun game -> x.Game = game) )
//                         |> function     
//                             | [] -> game
//                             | xs -> 
//                                 printfn "-----"
//                                 printfn "%A" game.Game
//                                 printfn "%A" game.Reward

//                                 printfn "Matched with:"
//                                 xs |> List.iter (fun g -> 
//                                     printfn "%A" g.Game
//                                     printfn "%A" g.Reward
//                                     printfn "--" )
//                                 printfn "-----" 

//                                 xs |> List.minBy (fun x -> x.Reward)
                       
//                     {game with Reward = irrelevantGames.Reward}
//             ))

let augmentValue gameNumber prefix file = 

    // let scores = [1 .. 500] |> List.fold (fun (sequence, inc) _ -> inc::sequence, inc * 0.9) ([], 1.) |> fst |> List.rev

    let xs = File.ReadAllLines file

    let rows = 
        xs
        |> Array.mapi sequenceDataValue
        |> updateMoveCount
        // |> Array.map (fun (x: Value) -> 
        //     if x.Reward = 0. then x
        //     else 
        //         let mc = Math.Max(x.MoveCount, x.MoveOrder)
        //         { x with Reward = scores.[mc - x.MoveOrder] } )   
        // |> resetScoreForIrrelevantMoves
        // |> inParallelConcat permuteColumnsForValue
        // |> inParallelConcat permuteColumnsForPolicy
        

    // rows
    // |> Array.groupBy (fun x -> x.Move)
    // |> Array.map (fun (x,xs) -> x, xs |> Array.length)
    // |> Array.sortByDescending snd
    // |> Array.iter (printfn "%A")

    let targetFile = sprintf "%s/data-value-%d.csv" prefix gameNumber
    if File.Exists targetFile then
        File.Delete targetFile

    let data = 
        rows 
        |> Array.map (fun x -> x.Game, (if x.MoveCount = x.MoveOrder then 1. else 0.), x.Reward)
                // x.Game, (float 1 / (mc - float x.MoveOrder) * float x.MoveOrder))   
        // |> Array.groupBy (fun (_,y) -> y = float 0.)

    // let maxCount = 
    //     rows
    //     |> Array.map (fun (x: Value) ->  x.Game, x.Reward = 0.)
    //     |> Array.groupBy snd
    //     |> Array.map (fun (x,xs) -> x, xs.Length)
    //     |> Array.maxBy snd
    //     |> snd

    let toOutputFormat (game, isDone, reward) =

        let gameString = 
            game 
            |> encodeKeyGame 26 cardEncoder 
            |> List.truncate (26 * 11)
            |> List.map string 
            |> String.concat ","

        // let fixReward = reward game
        // let reward = 
        //     if isDone = 1. && isWin = 1. then 1.
        //     else -1.
        
        // sprintf "%f,%f,%s" isDone reward gameString

        sprintf "%f,%s" reward gameString

        // let game = game |> encodeKeyGame 13 cardEncoder |> List.truncate (13 * 10) |> Seq.map string |> String.concat ","
        // sprintf "%.2f,%d,%d,%d,%s" (float isDone) longesColumn.Length shortestColumn cardCount game 

    printfn "Before %d, After: %d" xs.Length rows.Length
    let writer = new StreamWriter(targetFile, false, Encoding.ASCII, 65536)

    data 
    |> Array.toList
    |> List.map (List.singleton >> List.map toOutputFormat)
    |> List.toArray
    // |> inParallel (oversample maxCount id >> List.map toOutputFormat)
    |> Array.iter (saveBalancedPolicyNet writer)

    writer.Flush()
    writer.Close()
       