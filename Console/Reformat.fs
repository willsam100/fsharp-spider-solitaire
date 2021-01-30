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
open System.Diagnostics
open SpiderSolitare.Game
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
    Value: float
    PredValue:float list
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

// [<Struct>]
// type LegacyFile = {
//     Game: string
//     Move: int
//     Reward: bool
//     MoveOrder: int
//     GameNumber: int
//     RowNumber: int
// }

type ColumnGame = {
    Game: Game
    Column: Column
}

module CardCompleted = 

    let totalCards = 104
    let startTabSize = 54
    let stockIncrement = 10
    let completedRunSize = 13

    let permutations = 

        startTabSize
        |> List.replicate 6
        |> List.mapi (fun i x -> 
            if i = 0 then x 
            else x + (i * stockIncrement) )
        |> List.replicate 8
        |> List.mapi (fun i xs -> 
            if i = 0 then (i, xs)
            else 
                i, xs |> List.mapi (fun ii x -> x - (i * completedRunSize)) |> List.filter (fun x -> x >= 0) ) 
        |> List.map (fun (x, xs) -> xs |> List.map (fun y -> y,x) )
        |> List.concat
        |> Map.ofList


let getLongestColumnForGame g = g |> GameMover.getRuns |> List.maxBy (fun (c, cs) -> cs.Length) |> fst

let getLongestColumn tabsize x = 
    x |> gameDecoded tabsize |> getLongestColumnForGame


let sequenceDataPolicy rowNumber (x: string) = 
    async {
        
        let row = x.Split ","

        let isWin = Array.head row |> Int32.Parse |> float
        let moveOrder = row |> Array.skip 1 |> Array.head |> Int32.Parse
        let game = row |> Array.skip 2 |> Array.take 286 |> String.concat "," |> gameDecoded 26
        let move = row.[288] |> Int32.Parse
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
                    ScoredGame = isWin
                    LongestColumn = getLongestColumnForGame game
                    PredValue = []
                    Value = -1.
                } 
            with 
            | e -> 
                printfn "RowNumber:%d" rowNumber
                raise e    
    }

let isValidPolicy (policy: Policy) = 
    let validMoves = GameMover.validMoves policy.Game
    let move = policy.Move |> MoveEncoding.intToMove
    if move = Stock then true 
    else 
        let r = validMoves |> List.contains move
        if not r then
            printfn "bad policy: %A" move
        r //&& policy.ScoredGame = 1.


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
            PredValue = p.PredValue
            Value = p.Value
        }
        
    applyTransform transform 
    |> List.toArray

let sortColumnsForPolicy (p: Policy) =

    let mapping, newGame = p.Game |> MoveEncoding.sortGame
    let move = 
        p.Move 
        |> MoveEncoding.intToMove 
        |> MoveEncoding.sortMoveWithMappings mapping

    {p with Game = newGame; Move = move |> MoveEncoding.moveToInt}
    

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

let perumuteSaferCardsForPolicy (p: Policy) = 

    let rec incrementGame incrementValue policies (p: Policy) = 
        if policies |> List.length = 12 || p.Move = 0 then policies
        else 
            match p.Move |> MoveEncoding.intToMove with 
            | Stock | Flip _ -> policies
            | Move coord -> 
                let gameIncremented = 
                    p.Game
                    |> Game.getAllTabsWithColumn
                    |> List.map (fun (c,t) -> 
                        if c = coord.From || c = coord.To then c, t
                        else c, t |> Tableau.map (CardModule.increment incrementValue) )
                    |> fun tabs -> Game.updateTableaus tabs p.Game

                let policy = 
                    { p with 
                        Game = gameIncremented
                        Move = p.Move
                    } 
                incrementGame incrementValue (policy :: policies) policy

    let up = incrementGame 1 [p] p 
    let down = incrementGame -1 [] p
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

let sameGameValidate (games: Policy[]) = 
    let gameNumber = games.[0].GameNumber
    if games |> Array.forall (fun x -> x.GameNumber = gameNumber) |> not then 
        failwithf "You're doing it wrong. The input should be instances for a single game number"

// let toInstances (games: Policy[]) = 
//     sameGameValidate games

//     games
//     |> Array.sortBy (fun x -> x.RowNumber)
//     |> Array.fold (fun (map, current) nextStateMove -> 
//         match map |> Map.tryFind current with 
//         | None -> map |> Map.add current [nextStateMove], current
//         | Some series -> 
//             match series with 
//             | [] ->  map |> Map.add current [nextStateMove], current
//             | series -> 

//                 let nextMove = 
//                     series 
//                     |> List.exists (fun x -> 
//                         let move = MoveEncoding.intToMove x.Move
//                         let pNext = 
//                             GameMover.playMove move x.Game |> GameResult.fold id id (fun game _ -> game)

//                         Game.getAllTabs nextStateMove.Game = Game.getAllTabs pNext ) // && x.MoveCount + 1 = nextStateMove.MoveCount)

//                 match nextMove with 
//                 | false -> 
//                     let current = Guid.NewGuid()
//                     map |> Map.add current [nextStateMove], current
//                 | true -> 
//                     map |> Map.add current (nextStateMove :: series), current

//         ) (Map.empty, Guid.NewGuid())
//     |> fst
//     |> Map.map (fun _ instance -> instance |> List.sortBy (fun x -> x.MoveOrder ) )

    // printfn "Building series"
    // games 
    // |> Array.sortBy (fun x -> x.RowNumber) 
    // |> Array.head
    // |> Array.unfold (fun (previousState: Policy) -> 
    //     printfn "%A" previousState.Game
    //     let move = previousState.Move |> MoveEncoding.intToMove
    //     GameMover.playMove move previousState.Game
    //     |> GameResult.fold (fun _ -> printfn "Game Lost"; None) (fun _ -> printfn "Game Won";  None) (fun nextGame _ -> 
    //         printfn "Looking for in %d games:" games.Length
    //         printfn "%A" nextGame
    //         games |> Array.tryFind (fun p -> Game.getAllTabs p.Game = Game.getAllTabs nextGame) )
    //     |> Option.map (fun x -> x,x) )
    // |> Array.iter (fun x -> printfn "%A" x)

let isNextGame (pPolicy: Policy) nextGame (p: Policy) = 
    Game.getAllTabs p.Game = Game.getAllTabs nextGame && p.MoveOrder = pPolicy.MoveOrder + 1 && pPolicy.RowNumber < p.RowNumber

let findNextPolicy (pPolicy: Policy) gamePool = 
    GameResult.fold 
        (fun _ -> printfn "Game Lost"; None) 
        (fun _ -> printfn "Game Won";  None) 
        (fun nextGame _ ->  
            gamePool 
            |> List.tryFind (isNextGame pPolicy nextGame) 
            |> Option.map (fun pNext -> 
                    [pPolicy; pNext], pNext) ) 

let playMove gamePool (pPolicy: Policy)  = 
    GameMover.playMove (MoveEncoding.intToMove pPolicy.Move) pPolicy.Game
    |> findNextPolicy pPolicy gamePool

let toSeries (games: Policy[]) = 
    sameGameValidate games
    let games = List.ofArray games |> List.sortBy (fun x -> x.MoveOrder)

    let buildSeries (allSeries, gamePool) (game: Policy) = 
        if gamePool |> List.contains game then 
            let series = 
                game 
                |> List.unfold (fun pPolicy -> playMove gamePool pPolicy ) 
                |> List.collect id
                |> List.distinct
                |> List.sortBy (fun (x: Policy) -> x.MoveOrder)

            if List.isEmpty series then 
                // This means that the data quality is poor. The next move was not found for the first game. 
                // No reason to keep the first game, there is little to learn here as the series appears to be incomplete
                // Be sure to remove the game from the game pool. 
                allSeries, gamePool |> List.filter (fun x -> x <> game)
            else 
                // let allSeries = allSeries |> Map.add (Guid.NewGuid()) series

                let gamePool = 
                    gamePool 
                    |> List.filter (fun gpPolicy -> series |> List.contains gpPolicy |> not)

                series :: allSeries, gamePool
        
        else allSeries, gamePool

    games 
    |> List.fold buildSeries ([], games) 
    |> fst

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

let stockMovesPlayed (ps: Policy list) = 
    ps 
    |> List.map (fun x -> x.Move)
    |> List.map MoveEncoding.intToMove
    |> List.filter MoveType.isStock
    |> List.length

let getCardsCompleted (ps:Policy list) =
    let cardCount = 
        ps 
        |> List.last 
        |> fun x -> x.Game 
        |> Game.tableauCardCount

    CardCompleted.permutations |> Map.find cardCount


let matchDuplicates (games: Policy[]) =
    sameGameValidate games

    let map = 
        games
        |> toSeries
        |> List.map (fun series -> getCardsCompleted series,  series.Length, series)

    // let cardCount = 
    //     map 
    //     |> Map.toArray
    //     |> Array.distinctBy (fun (_, (x, _, _)) -> x)
    //     |> Array.length 


    // if cardCount >= 2 then 
    //     map |> Map.iter (fun _ (cardCount, count, ls) -> 

    //         let stock = stockMovesPlayed ls
    //         let cc = ls |> List.skip (ls.Length - 4) |> List.head |> fun x -> x.Game |> Game.tableauCardCount

    //         if count = 1 then 
    //             let game = ls |> List.last |> fun x -> x.Game
    //             let move = ls |> List.last |> fun x -> x.Move |> MoveEncoding.intToMove
    //             printfn "%A" game
    //             printfn "%A" <| GameMover.playMove move game
    //             printfn ""
                

    //         printfn "CC:%3d Count:%3d StockPlayed:%d lastGameTabCount:%3d" cardCount count stock cc)

    //     // printfn ""
    //     // games |> Array.iter (fun p -> printfn "%A" p.Game)

        

    //     failwithf "Found one to check"
    

    let duplicateGames = 
        games 
        |> Array.groupBy (fun p -> p.Game)
        |> Array.map (fun (g, xs) -> g, xs |> Array.distinctBy (fun x -> x.Move) )
        |> Array.filter (fun (g, xs) -> xs.Length >= 2)
        
    let duplicates = 
        duplicateGames
        |> Array.collect (fun (_, xs) -> xs)
        |> Array.collect (fun p -> 
            map 
            |> List.choose (fun (cardCound, count, instances) -> 
            
                instances 
                |> List.tryFind (fun i -> i.Game = p.Game) 
                |> Option.map (fun x -> cardCound, count, x)

                )
            |> List.sortBy (fun (cardCount, count, _) -> cardCount, count)
            |> List.skip 1 // This is the best, so keep this one
            |> List.toArray
            )
        |> Array.map (fun (_, _, x) -> x)

    map, duplicateGames, duplicates

let validateDuplicate (data: Policy[]) =

    data 
    |> Array.groupBy (fun x -> x.GameNumber) 
    |> Array.iter (fun (_, games) -> 
        let map, dupGames, duplicates = matchDuplicates games

        let games' = games |> Array.filter (fun x -> duplicates |> Array.contains x |> not)

        let map', dupGames', duplicates' = matchDuplicates games'

        if dupGames' |> Array.isEmpty |> not then 
            failwith "Logic error"

        if duplicates' |> Array.isEmpty |> not then 
            failwith "Logic error"

        let getDuplicates m = 
            m
            |> List.filter (fun (_, _, xs) -> 
                xs 
                |> List.exists (fun (p: Policy) -> 
                    duplicates |> Array.exists (fun d -> d.Game = p.Game) ) )

        dupGames
        |> Array.forall (fun (_, xs) -> xs.Length = (xs |> Array.distinctBy (fun x -> x.Move) |> Array.length ) )
        |> fun x -> if not x then failwith "Found a bug"


        // let duplicatesMap = getDuplicates map
        // let mapExpectedNoDups = getDuplicates map'

        // mapExpectedNoDups
        // |> List.forall (fun x -> duplicatesMap |> Map.containsKey x)
        // |> fun x -> if not x then failwith "Found a bug"

        // mapExpectedNoDups
        // |> List.iter (fun ((_, _, ps) ) ->  

        //     let games = ps |> List.map (fun p -> p.Game)

        //     dupGames
        //     |> Array.filter (fun (_, xs) -> 
        //         xs |> Array.exists (fun d -> games |> List.contains d.Game)) 
        //     |> Array.iter (fun (game, polices) -> 

        //         let bestPolicyMove = 
        //             ps |> List.find (fun p ->
        //                 polices |> Array.map (fun p -> p.Move) |> Array.contains p.Move)
                
        //         let worstMoves = polices |> Array.filter (fun p -> p.Move <> bestPolicyMove.Move)

        //         map
        //         |> List.iter (fun (cardCount, worstCount, ps) -> 

        //             let isRelevant = ps |> List.exists (fun p -> p.Game = game)
        //             if isRelevant then 
        //                 ps 
        //                 |> List.find (fun p -> worstMoves |> Array.exists (fun w -> w.Move = p.Move ) )
        //                 |> ignore

        //                 if (map'.[key] |> fun (a,b,c) -> b) >= worstCount then 
        //                     failwith "Logic error on count"
        //             )
        //         )
        //     )
    )
    
let filterDuplicateMoves (data: Policy[]) =
    data 
    |> Array.groupBy (fun x -> x.GameNumber) 
    |> Array.collect (fun (_, games) -> 
        let _, _, duplicates = matchDuplicates games
        games |> Array.filter (fun x -> duplicates |> Array.contains x |> not)
    )

let filterStupidMoves (data: Policy[]) =
    data 
    |> Array.filter (fun x -> 
        MoveEncoding.intToMove x.Move
        |> MoveType.foldMove true (fun coord -> 

            MoveEncoding.getTargetCardForCoord x.Game coord 
            |> snd
            |> function 
            | Some _ -> true
            | None -> 
                let lastCardInRun = 
                    x.Game 
                    |> Game.getAllTabsWithColumn 
                    |> List.find (fun (c,_) -> c = coord.From)
                    |> snd
                    |> Tableau.getRun
                    |> List.last

                coord.Card = lastCardInRun
            )  )

let validateInstances (data: Policy[]) =
    data 
    |> Array.groupBy (fun x -> x.GameNumber) 
    |> Array.iter (fun (_, games) -> 

        toSeries games
        |> List.iter (fun series -> 

            if List.length series = 1 then 
                failwithf "Do better %A" series

            series 
            |> List.pairwise
            |> List.map (fun (p, pNext) -> MoveEncoding.intToMove p.Move, p, pNext)
            |> List.iter (fun (move, game, gameNext) -> 

                let g = 
                    GameMover.playMove move game.Game 
                    |> GameResult.fold id id (fun game _ -> game)

                if Game.getAllTabs g <> Game.getAllTabs gameNext.Game then 

                    series
                    |> List.iter (fun x -> 
                        printfn "\n%A" x.Game
                        printfn "%A" <| MoveEncoding.intToMove x.Move 
                        printfn "%A\n" x.MoveOrder
                        
                        )

                    printfn "G Mo:%d RN:%d" game.MoveOrder game.RowNumber
                    printfn "GN Mo:%d RN:%d" gameNext.MoveOrder gameNext.RowNumber

                    printfn "Logic Error:\n%A\n%A\n" game.Game move
                    printfn "Expected:\n%A\n" gameNext.Game
                    printfn "Actual:\n%A" g

                    failwith "Logic error. See logs"

                )
        ) )

// Some moves are simply a transpose - they add no value
let filterBadInstances countOfInstances (data: Policy[]) =
    data 
    |> Array.groupBy (fun x -> x.GameNumber) 
    |> Array.collect (fun (_, games) -> 
        games
        |> toSeries
        |> List.map (fun instance -> instance.Length, instance)
        |> List.sortBy snd
        |> List.truncate countOfInstances
        |> List.map snd
        |> List.concat
        |> List.toArray )

// Some moves are simply a transpose - they add no value
let filterTransposeMoves (data: Policy[]) =
    data 
    |> Array.groupBy (fun x -> x.GameNumber) 
    |> Array.collect (fun (_, games) -> 
        games
        |> toSeries
        |> List.map (fun series -> 

            let transposedState = 
                series
                |> List.filter (fun x -> x.Game |> Game.getAllTabs |> List.map Tableau.length |> List.exists (fun x -> x = 0))
                |> List.filter (fun p -> 
                    let move = p.Move |> MoveEncoding.intToMove
                    let gNext = p.Game |> GameMover.playMove move |> GameResult.fold id id (fun g _ -> g)
                    p |> permuteColumnsForPolicy |> Array.exists (fun p' -> p'.Game = gNext) ) 

            series 
            |> List.filter (fun x -> transposedState |> List.contains x |> not) )
        |> List.concat
        |> List.toArray )

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



// let augment gameNumber prefix file = 
//     let xs = File.ReadAllLines file

//     let rows = 
//         xs
//         |> Array.mapi sequenceDataPolicy
//         |> Async.Parallel
//         |> Async.RunSynchronously
//         |> filterDuplicateeMoves
//         |> inParallelConcat perumuteCardsForPolicy
//         |> inParallelConcat permuteColumnsForPolicy

//     printfn "Before %d, After: %d" xs.Length rows.Length

//     let targetFile = sprintf "%s/data-%d.csv" prefix gameNumber
//     if File.Exists targetFile then
//         File.Delete targetFile

//     let writer = new StreamWriter(targetFile, false, Encoding.ASCII, 65536)
//     let data = 
//         rows 
//         |> Array.map (fun (x: Policy) -> x.Game, float x.Move) 
//         |> Array.filter (fun x -> snd x <> 0.)  
//         |> Array.distinct
//         |> Array.groupBy snd
//         |> Array.map (fun (x,y) -> x, y |> Array.distinct)
        
//     let count = 
//         data 
//         |> Array.filter (fun x -> fst x <> 0.)
//         |> Array.map (fun x -> x |> snd |> Array.length)
//         |> Array.max

//     printfn "Oversample size:%d" count

//     let toOutputFormat (game, isDone) =

//         let game = 
//             game 
//             |> encodeKeyGame 26 cardEncoder 
//             |> List.truncate (26 * 11)
//             |> List.map string 
//             |> String.concat ","
        
//         sprintf "%d,%s" (int isDone) game

//     // let toOutputFormat (game, isDone) =
//     //     let game = game |> encodeKeyGame 13 cardEncoder |> List.truncate (13 * 10) |> Seq.map string |> String.concat ","
//     //     sprintf "%d,%s" (int isDone) game 

//     data 
//     |> Array.map (snd >> Array.toList)
//     |> inParallel (oversample count cutoutGame >> List.map toOutputFormat)
//     // |> inParallel (List.map toOutputFormat)

//     |> Array.iter (saveBalancedPolicyNet writer)
//     writer.Flush()
//     writer.Close()

let ports = [5100 .. 5107]
let brainsMover = BrainsMoverClient(ports) :> IBrainsMover

let emptyAction = List.replicate 1171 0.

let actionLookup (policy:Policy) = 
    async {
        // let! actions = brainsMover.GetBestMove policy.Game
        return { policy with PredValue = emptyAction}
    }

// let valueLookup (policy:Policy, nextState: Policy) = 
//     async {
//         let! value = brainsMover.GetValue nextState.Game
//         return { policy with Value = value}
//     }

let groupPolicyByGameInstance (policies: Policy[]) = 
    policies
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

let applyToPolicyGameInstances f policies = 
    policies
        |> groupPolicyByGameInstance
        |> List.map f
        |> List.concat
        |> List.toArray


let updateWithValue op (gameInstances: Policy list) = 
    gameInstances
    |> List.sortBy (fun x -> x.MoveOrder)
    |> List.pairwise
    |> List.map op
    |> Async.Parallel
    |> Async.RunSynchronously
    |> Array.toList

let filterAmbigousMoves targetCard coord game = 

    let nextCard = 
        game
        |> Game.getAllTabsWithColumn 
        |> List.find (fun (col,_) -> col = coord.To)
        |> snd
        |> Tableau.firstCard
        |> Option.map (CardModule.increment 1)

    let movesWithCard = 
        game 
        |> GameMover.validMoves
        |> List.filter (MoveType.foldMove false (fun c -> c.Card = targetCard))
        |> List.length

    let movesForNextCard = 
        game
        |> Game.getAllTabs
        |> List.choose Tableau.firstCard
        |> List.filter (fun x -> nextCard |> Option.exists (fun nc -> x = nc))
        |> List.length

    movesWithCard = 1 && movesForNextCard = 1

let targetCards = [Card(10, S); Card(11, S); Card(12, S); Card(13, S) ]

let isTargetMove game = function
    | Stock -> false
    | Flip _ -> false
    | Move c -> 
        if targetCards |> List.contains c.Card then 
            filterAmbigousMoves c.Card c game
        else false

let augment gameNumber prefix file = 
    async {
        printfn "Balancing %d" gameNumber
        let s = Stopwatch()
        s.Restart()

        let! xs = File.ReadAllLinesAsync file |> Async.AwaitTask

        let rows = 
            xs
            |> Array.mapi sequenceDataPolicy
            |> Async.Parallel
            |> Async.RunSynchronously
            |> Array.filter isValidPolicy
            |> filterDuplicateMoves
            |> fun rows -> validateInstances rows; rows
            // |> filterTransposeMoves
            // |> fun xs -> validateDuplicate xs; xs
            // |> filterStupidMoves
            // |> inParallelConcat permuteColumnsForPolicy
            |> Array.map sortColumnsForPolicy
            // |> inParallelConcat perumuteSaferCardsForPolicy

        // if rows.Length <> (filterTransposeMoves rows |> Array.length) then 
        //     failwithf "filterTransposeMoves is wrong %d %d" rows.Length (filterTransposeMoves rows |> Array.length)

        printfn "%3d: Before %d, After: %d E:%A" gameNumber xs.Length rows.Length s.Elapsed

        let targetFile = sprintf "%s/data-%d.csv" prefix gameNumber
        if File.Exists targetFile then
            File.Delete targetFile

        let writer = new StreamWriter(targetFile, false, Encoding.ASCII, 65536)

        let toOutputOneHostGameActionVector (policy:Policy) =

            let game = 
                policy.Game 
                |> encodeOneHotGame 13 1
                // |> List.truncate (26 * 11)
                |> List.map string 
                |> String.concat ","

            let move = policy.Move |> MoveEncoding.intToMove |> MoveEncoding.oneHotKeyed policy.Game
            sprintf "%d,%s" move game

        let toOutputFormatWithThreeVector (policy:Policy) =

            let game = 
                policy.Game 
                |> encodeOneHotGame 13 1
                // |> List.truncate (26 * 11)
                |> List.map string 
                |> String.concat ","

            let move = policy.Move |> MoveEncoding.intToMove
            let f, t, count = 
                match move with 
                | Stock -> 0,0,0
                | Flip c -> Coord.toInt c, 0, 0
                | Move c -> 
                    let card = cardEncoder.Encode c.Card
                    Coord.toInt c.From, Coord.toInt c.To, card

            sprintf "%d,%d,%d,%s" f t count game

        // let toOutputFormatOneHotActions (policy:Policy) =

        //     let game = 
        //         policy.Game 
        //         |> encodeKeyGame 26 cardEncoder 
        //         |> List.truncate (26 * 11)
        //         |> List.map string 
        //         |> String.concat ","

        //     sprintf "%d,%s" policy.Move game

        let toOutputFormatOneHotActions (policy:Policy) =

            let game = 
                policy.Game 
                |> encodeOneHotGame 13 1
                |> List.map string 
                |> String.concat ","

            let move = policy.Move |> MoveEncoding.intToMove 

            let moveOneHot = 
                move
                |> MoveEncoding.oneHotKeyed policy.Game
                |> OneHot.oneHotString MoveEncoding.oneHotViaCountCount

            // let size = 
            //     policy.Game
            //     |> GameMover.playMove move
            //     |> GameResult.fold (fun _ -> 0) (fun _ -> 0) (fun g _ -> 
            //         move |> MoveType.foldMove 0 (fun coord -> 
            //             g
            //             |> Game.getAllTabsWithColumn
            //             |> List.find (fun (col,_) -> col = coord.To)
            //             |> snd
            //             |> Tableau.getRun
            //             |> List.length )
            //         )
            //     |> OneHot.oneHotString 12

            let moves = 
                policy.Game 
                |> GameMover.validMoves
                // |> List.filter (MoveType.foldMove true (fun c -> targetCards |> List.contains c.Card))
                // |> List.filter (MoveType.foldMove true (fun _ -> false))
                |> List.map (MoveEncoding.oneHotKeyed policy.Game >> OneHot.toOneHot MoveEncoding.oneHotViaCountCount)
                |> OneHot.logicalOr

            sprintf "%s,%s,%s" moveOneHot moves game

        let toOutputFormatCardsInColumn (policy:Policy) =

            policy.Game 
            |> Game.getAllTabs
            |> List.filter (fun tab -> Tableau.length tab >= 1 )
            |> List.map (fun tab -> 
                let game = 
                    tab 
                    |> Tableau.getVisible
                    |> List.truncate 12
                    |> fun xs -> 
                        if xs.Length < 12 then 
                            let padding = List.replicate (12 - xs.Length) None
                            let r = (xs |> List.map Some) @ padding
                            if r.Length <> 12 then failwithf "%A" r
                            else r  
                        else xs |> List.map Some
                    |> List.map (CardEncoding.toOptionOneHot 1 >> List.map string)
                    |> List.concat
                    |> String.concat ","

                let card = 
                    tab
                    |> Tableau.getRun
                    |> List.truncate 12
                    |> List.last
                    |> CardEncoding.toOneHot 1
                    |> List.map string
                    |> String.concat ","            
            
                sprintf "%s,%s" card game )
            |> String.concat "\n"


        let toOutputOneHotAllMoves (policy:Policy) =

            let game =  MoveEncoding.oneHotAllMoves policy.Game
           
            let move = policy.Move |> MoveEncoding.intToMove
            let f, t, count = 
                match move with 
                | Stock -> 0,0,0
                | Flip c -> Coord.toInt c, 0, 0
                | Move c -> 
                    let card = cardEncoder.Encode c.Card
                    Coord.toInt c.From, Coord.toInt c.To, card

            sprintf "%d,%d,%d,%s" f t count game

        rows
        // |> Array.filter (fun x -> x.Move |> MoveEncoding.intToMove |> MoveType.foldMove true (fun c -> targetCards |> List.contains c.Card))
        // |> Array.filter (fun x -> x.Move |> MoveEncoding.intToMove |> MoveType.foldMove true (fun _ -> false))
        |> Array.groupBy (fun (x: Policy) -> 
            let move = x.Move |> MoveEncoding.intToMove 
            let card = move |> MoveType.foldMove -1 (fun c -> c.Card |> CardModule.getValue) 
            let c = move |> MoveEncoding.getTargetCardForMove x.Game |> Option.map fst 
            (c, card))
        |> Array.collect (fun (c, xs) -> 
            match fst c with 
            | None -> xs
            | Some _ -> 

                let magicNumber = 200

                let ys = 
                    xs 
                    |> Array.map (fun (x: Policy) -> 
                        x, x.Move 
                        |> MoveEncoding.intToMove 
                        |> MoveEncoding.getTargetCardForMove x.Game 
                        |> Option.get
                        |> snd 
                        |> Option.isNone)

                let countOfEmtpyColumns = ys |> Array.filter snd |> Array.length

                ys
                |> Array.collect(fun (x,isEmpty) -> 
                    if not isEmpty then [|x|] 
                    else 
                        x
                        |> Array.replicate (magicNumber / countOfEmtpyColumns) 
                        |> Array.truncate 1200 // Don't replicate more than this.
                        |> Array.mapi (fun i  x -> if i = 0 then x else {x with Game = cutoutGame x.Game}) 
                    )
            )
        |> Array.groupBy (fun x -> x.GameNumber)
        |> Array.map (snd >> Array.toList)
        |> Array.map (List.map toOutputFormatOneHotActions)
        |> Array.iter (saveBalancedPolicyNet writer)
        writer.Flush()
        writer.Close()
    }

let groupValueByGameInstance (values: Value[]) = 
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
        |> groupValueByGameInstance
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

    let xs = File.ReadAllLines file

    let rows = 
        xs
        |> Array.mapi sequenceDataValue
        |> updateMoveCount

    let targetFile = sprintf "%s/data-value-%d.csv" prefix gameNumber
    if File.Exists targetFile then
        File.Delete targetFile

    let toOutputFormat (game, isDone, reward) =

        let gameString = 
            game 
            |> encodeKeyGame 26 cardEncoder 
            |> List.truncate (26 * 11)
            |> List.map string 
            |> String.concat ","

        sprintf "%f,%s" reward gameString

    printfn "%d: Before %d, After: %d" gameNumber xs.Length rows.Length
    let writer = new StreamWriter(targetFile, false, Encoding.ASCII, 65536)

    rows 
    |> Array.map (fun x -> x.Game, (if x.MoveCount = x.MoveOrder then 1. else 0.), x.Reward) 
    |> Array.toList
    |> List.map (List.singleton >> List.map toOutputFormat)
    |> List.toArray
    |> Array.iter (saveBalancedPolicyNet writer)

    writer.Flush()
    writer.Close()
       