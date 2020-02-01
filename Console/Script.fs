module Script

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


type StateAction = {
    Game: string
    Move: string
    Reward: int
    MoveOrder: int
    GameNumber: int
    RowNumber: int
}

// let format (game) = 
//     let tableau = game |> Array.take (96 * 10)
//     let stock = game |> Array.skip (96 * 10)

//     let emptyPadding = Array.replicate (96 * (95 - 10)) "0" // 95 as the last row is the stock. 10 rows are the tableaus. 96 is the length of each tableau

//     let hasStock = 
//         stock 
//         |> Array.chunkBySize 10
//         |> Array.map (fun xs -> if xs |> Array.forall (fun c -> c <> "0") then "15" else "0")

//     let lastRow = Array.append hasStock (Array.replicate (96 - hasStock.Length) "0") |> String.concat ","

//     let tableau = 
//         let tableau = tableau |> Array.map (fun x -> if x = "1" then "0" else x)
//         Array.append tableau emptyPadding |> String.concat ","

//     sprintf "%s,%s" tableau lastRow

let format (game:string) = 
    let game = game.Split ","
    let tableau = game |> Array.take (96 * 10) |> String.concat ","
    let stock = game |> Array.skip (96 * 10) |> Array.truncate 10 |> String.concat ","
    sprintf "%s,%s" tableau stock

let sequenceData rowNumber (x: string) = 
    let row = x.Split ","
    let game = row |> Array.skip 1 |> Array.take (row.Length - 1171 - 2) |> String.concat ","
    let move = row |> Array.skip (row.Length - 1171 - 2) |> Array.take 1171 |> String.concat ","
    let outcome = 
        match row |> Array.rev |> Array.tail |> Array.head with 
        | "true" ->  1
        | "false" ->  0
        | _ -> failwith "Software error for reading files!"
    let moveOrder = Array.head row |> Int32.Parse
    let gameNumber = Array.last row |> Int32.Parse

    // format game, move, outcome, gameNumber, moveOrder
    {
        Game = format game
        Move = move
        Reward = outcome
        MoveOrder = moveOrder
        GameNumber = gameNumber
        RowNumber = rowNumber
    }   

let toMap data = 
    let inline toMap kvps =
        kvps
        |> Seq.map (|KeyValue|)
        |> Map.ofSeq

    let d = Dictionary<string, _>() 

    data 
    |> Array.iter (fun (g,m) -> 
        match d.TryGetValue m with 
        | true, gs -> d.[m] <- g :: gs
        | false, _ -> d.[m] <- [g]  )

    toMap d

let trimCommonMoves maxCount map = 
    map  
    |> Map.map (fun m gs -> 
            gs
            |> Array.sortByDescending (fun x -> x.RowNumber)
            |> Array.truncate maxCount

    )


let oversample minCount max map = 

    let rec oversample targetCount gs = 
        if List.length gs < targetCount then 
            let gs = List.replicate ( targetCount + 1 / gs.Length ) gs |> List.concat 
            oversample targetCount gs
        else
            List.truncate targetCount gs

    let maxCount =      
        match max with 
        | Some x -> x
        | None -> 
            if Map.count map = 0 then 
                0 
            else 
                map 
                |> Map.toList 
                |> List.map (fun (_, gs) -> gs |> List.distinct |> List.length)
                |> List.max

    printfn "Max: %d" maxCount

    map 
    // |> Map.filter (fun k (gs) -> List.length gs >= minCount)
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

let loadData file = 
    File.ReadAllLines file 
    |> Array.mapi sequenceData

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


let filterDuplicateGamesStateAction data = 

    let movesForGame = 
        data 
        |> Array.groupBy (fun stateAction -> stateAction.GameNumber)
        |> Map.ofArray
        |> Map.map (fun k v -> 
            match v |> Array.forall (fun x -> x.Reward = 1) with 
            | true -> v.Length
            | false -> Int32.MaxValue )// assume game was a loss and attribute max moves )

    let gameToGameNumber = 
        data 
        |> Array.map (fun stateAction -> stateAction.Game, stateAction.Move, stateAction.GameNumber)
        |> Array.groupBy (fun (g,m, _) -> g)
        |> Map.ofArray
        // |> Map.filter (fun k v -> v |> Array.map (fun (g,m,gn) -> m) |> Array.distinct |> Array.length > 1) // The games that have duplicate moves
        // |> Map.map (fun k v -> v |> Array.map (fun (g,m,gn) -> gn))
        |> Map.map (fun k v -> 
            v 
            |> Array.map (fun (_,_,gn) -> gn) 
            |> Array.distinct 
            |> Array.minBy (fun gn -> 
                match movesForGame.TryFind gn with 
                | Some moves -> moves
                | None -> Int32.MaxValue) )

    data 
    |> Array.groupBy (fun stateAction -> stateAction.GameNumber)
    |> Map.ofArray
    |> Map.filter (fun currentGameNumber stateActionPairs -> 
        stateActionPairs 
        |> Array.forall (fun stateAction -> 
            match gameToGameNumber |> Map.tryFind stateAction.Game with
            | None -> failwithf "Didint' find game: %A" stateAction.RowNumber
            | Some bestGameNumber ->  currentGameNumber = bestGameNumber
        ) )       
    |> Map.toArray
    |> Array.collect snd

type ResultType = 
    | Lost 
    | SingleWin
    | WonAndLost of winningRowNumbers: int array

let filterDuplicateGamesStateRewards data = 

    let gameNumbersToWin = 
        data 
        |> Array.groupBy (fun stateAction -> stateAction.GameNumber)
        |> Map.ofArray
        |> Map.map (fun k v ->  
            // if v |> Array.forall (fun x -> x.Reward = 1.) |> not then 
            //     printfn "%A" (v |> Array.map (fun x -> x.Reward, x.RowNumber) |> Array.toList)

            let rec splitWhile acc games : StateAction list list = 
                match games with 
                | [] -> acc
                | game::_ -> 
                    let n = games |> List.takeWhile (fun x -> x.Reward = game.Reward)
                    let tail = games |> List.skipWhile (fun x -> x.Reward = game.Reward)
                    splitWhile (n :: acc) tail


            let games = 
                v
                |> Array.sortBy (fun x -> x.RowNumber)
                |> Array.toList
                |> splitWhile []

            if games.Length = 1 then 
                if v |> Array.forall (fun x -> x.Reward = 1) then 
                    SingleWin
                else Lost
            else                 
                if games |> List.exists (fun gs -> gs |> List.forall (fun x -> x.Reward = 1)) then 
                    v |> Array.filter (fun x -> x.Reward = 1) |> Array.map (fun x -> x.RowNumber) |> WonAndLost  
                else Lost )


    let validRowNumbers = 
        data 
        |> Array.map (fun stateAction -> stateAction.Game, stateAction.Reward, stateAction.GameNumber, stateAction.RowNumber)
        |> Array.groupBy (fun (g,r, _, _) -> g)
        |> Map.ofArray
        |> Map.map (fun g v ->
            v 
            |> Array.map (fun (g,r,gn, rn) -> (g,r,gn, rn), gameNumbersToWin |> Map.find gn)  // Use the reward for the gameNumber since it is consistent (raw data is not)
            |> Array.distinctBy snd )
        |> Map.filter (fun g v -> v |> Array.length > 1 )
        |> Map.map (fun k v -> 


            // We have a list of games that have conflicts. 
            // If the gameState is misssing then there is no conflict and the game is valid
            // If the gameState is found, then we must return the gameState that is the best 
            // Any game that is a loss must not be returned for the input the gameState. 

            v
            |> Array.collect (fun ((g,r,gn, rn),isWin) -> 
                match isWin with 
                    | Lost -> [|rn|]
                    | SingleWin -> [|rn|]
                    | WonAndLost games -> games
                ) )
        |> Map.toArray
        |> Array.collect snd
        |> Set.ofArray


    data 
    |> Array.filter (fun x -> validRowNumbers |> Set.contains x.RowNumber)
    // |> Array.groupBy (fun stateAction -> stateAction.GameNumber)
    // |> Map.ofArray
    // |> Map.filter (fun currentGameNumber stateActionPairs -> 
    //     stateActionPairs 
    //     |> Array.forall (fun stateAction -> 
    //         match gameIsValid |> Map.tryFind stateAction.Game with
    //         | None -> true
    //         | Some winningGames -> winningGames |> Array.contains currentGameNumber
    //     ) )   
    // // |> Map.filter (fun currentGameNumber stateActionPairs -> 
    // //     stateActionPairs |> Array.forall (fun x -> consistentData |> Map.find x.GameNumber)
    // //     )    
    // |> Map.toArray
    // |> Array.collect snd    


let filterDuplicateGamesStateMoves data = 

        let getMoveOrder (a,b,c) = c

    // let gameNumbersToWin = 
        data 
        // |> Array.toList
        |> Array.filter (fun x -> x.Reward = 1)
        // |> List.distinctBy (fun x -> x.Game, x.Move, x.GameNumber, x.MoveOrder)
        // |> Array.groupBy (fun stateAction -> stateAction.GameNumber)
        // |> Map.ofArray
        // |> Map.map (fun k v ->  
            // if v |> Array.forall (fun x -> x.Reward = 1.) |> not then 
            //     printfn "%A" (v |> Array.map (fun x -> x.Reward, x.RowNumber) |> Array.toList)

            // let rec splitWhile acc games : StateAction list list = 
            //     match games with 
            //     | [] -> acc
            //     | game::_ -> 
            //         let n = games |> List.takeWhile (fun x -> x.Reward = game.Reward)
            //         let tail = games |> List.skipWhile (fun x -> x.Reward = game.Reward)
            //         splitWhile (n :: acc) tail


            // let games = 
            //     v
            //     |> Array.sortBy (fun x -> x.RowNumber)
            //     |> Array.toList
            //     |> splitWhile []

            // if games.Length = 1 then 
            //     if v |> Array.forall (fun x -> x.Reward = 1) then 
            //         v |> Array.map (fun x -> x.Game, x.Move) |> Array.toList
            //     else []
            // else                 
            //     if games |> List.exists (fun gs -> gs |> List.forall (fun x -> x.Reward = 1)) then 

            // v 
            // |> Array.filter (fun x -> x.Reward = 1) 
        |> Array.sortBy (fun x -> x.RowNumber, x.MoveOrder) 
        // |> List.map (fun x -> x.Game, x.Move, x.MoveOrder)
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
        |> List.collect (fun xs -> 
            let l = xs.Length
            xs |> List.map (fun x -> x, l))
        // |> List.distinctBy (fun (g,m,l) -> g)
        |> List.groupBy (fun (x,_) -> x.Game)
        |> List.map (fun (k,v) ->  v |> List.minBy (fun (_,x) -> x)  |> fun (x,_) -> x ) 
        |> List.toArray
        // |> Array.distinct


                // else [] 
                
                // )
        // |> Map.toList
        // |> List.collect snd
        // |> Set.ofList

    // data 
    // |> Array.filter (fun x -> gameNumbersToWin |> Set.contains (x.Game, x.Move))
    
let readAndFormat file = 
    let data = loadData file

    // data |> Array.iter (fun x -> printfn "%a" x.RowNumber, x.MoveOrder, x.GameNumber)
        
    data
        // |> Array.rev |> Array.truncate 41000 // A balance of more training data vs high quality training data 
        |> filterDuplicateGamesStateMoves
        // |> Array.filter (fun stateAction -> stateAction.Reward > 0.) 
        // |> Array.map (fun stateAction -> stateAction.Game, stateAction) 
        |> Array.groupBy (fun x -> x.Move)
        |> Map.ofArray
        |> trimCommonMoves 1000
        |> Map.map (fun k v -> v |> Array.toList |> List.map (fun x -> x.Game))
        |> oversample 1 None 
        |> savePolicy

    // data 
    // |> filterDuplicateGamesStateRewards
    // // |> Array.distinctBy (fun x -> x.Game, x.Reward)
    // |> Array.map (fun x -> x.Game, x.Reward)
    // |> Array.distinctBy (fun (g,_) -> g)
    // |> saveValueNet

let run () = 
    readAndFormat "/Users/willsam100/Desktop/spider-game-with-failure.csv"