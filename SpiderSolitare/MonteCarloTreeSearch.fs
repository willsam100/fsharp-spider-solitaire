module SpiderSolitare.MonteCarloTreeSearch

open SpiderSolitare.Representation
open System
open SpiderSolitare.Game
open Microsoft.ML
open System.IO
open System.Collections.Generic
// open StackExchange.Profiling
// open ExtCore.Collections
// open FSharp.Collections.ParallelSeq



let rand = Random()
let cardEncoder = CardEncoderKeyed()

type IBransMover = 
    abstract member GetBestMove: Game -> (MoveType * float) list
    abstract member GetValue: Game -> float

module Array = 
    let swap (a: _[]) x y =
        let tmp = a.[x]
        a.[x] <- a.[y]
        a.[y] <- tmp

    // shuffle an array (in-place)
    let shuffle a =
        Array.iteri (fun i _ -> swap a i (rand.Next(i, Array.length a))) a

[<CLIMutable>]
[<Struct>]
type Node = {
    mutable T: float
    mutable N: int
    mutable Children: Node list
    Game: Game
    Move: MoveType option 
}

type MutableNode(game:Game, prob: float, reward: float, move: MoveType option, depth: int, parent, terminalValue) = 
    // member val N = 0. with get,set
    // member val T = 0. with get, set
    member val Game = game
    member val GameHashCode = game.GetHashCode()
    member val Prob = prob
    member val Reward = reward
    member val Move = move
    member val Depth = depth
    member val Children: MutableNode list = [] with get,set
    member val Parent: MutableNode option = parent with get, set
    member val TerminalValue: float option = terminalValue

let getMetrics (gameToMetrics: IDictionary<int, float * float>) (node: MutableNode) = 
    match gameToMetrics.TryGetValue(node.GameHashCode) with 
    | true, x -> x
    | false, _ -> 0., 0.    


let ucb1 total n nRoot = 
    let n = n + 1.
    total + (1. * Math.Sqrt(Math.Log(nRoot) / n) ) 

let nextNode nRoot (nodes: (MutableNode * float * float) list) = 
    let items = 
        nodes 
        |> List.map (fun (x, t, n) -> 
            x,t,n, ucb1 t (float n) nRoot )
        |> List.sortByDescending (fun (_,_,_,x) -> x)

    items |> List.map (fun (x,t,n,_) -> x,t,n)

let nextNodesUbcOneAlpha0 gameToMetrics (node: MutableNode) nCurrentMove  nAllMoves = 

    let (t,n) = getMetrics gameToMetrics node
    let p = node.Prob
    (t / n) + (1. * p * Math.Sqrt nAllMoves / (1. + nCurrentMove))

let nextMove gameToMetrics (nodes: (MutableNode * float * float) list) =
    let nodes = nodes |> List.map (fun (x,y,z) -> x,z)
    let sumVisit = nodes |> List.sumBy snd

    nodes 
    |> List.map (fun (node, n) -> node, n, nextNodesUbcOneAlpha0 gameToMetrics node n sumVisit)
    |> List.sortByDescending (fun (_, _, ucb1) -> ucb1)
    |> List.map (fun (x, y, _) -> x,0.,y)


let reward game = 

    let scoreTab cards = 
        let rec scorer score cards = 
            match cards with 
            | [] -> score 
            | [_] -> score
            | x::y::cards -> 
                if CardModule.getValue y - 1 = CardModule.getValue x && CardModule.getSuit x = CardModule.getSuit y then 
                    scorer (score + 2) (y :: cards)
                else scorer score (y:: cards)

        scorer 0 cards

    let x = game |> Game.Game.getAllTabs |> List.sumBy (Tableau.getVisible >> scoreTab)

    let suitCompleted = 
        match game.Spades with 
        | Zero -> 0
        | One -> 50
        | Two -> 100
        | Three -> 150
        | Four -> 200
        | Five -> 250
        | Six -> 300
        | Seven -> 350
        | Eight -> 400

    float (x + suitCompleted) * 0.0025


let encodeGame game = 
    game |> encodeKeyGame cardEncoder |> Seq.map string |> String.concat ","


let getRandomMove game = 
    let allMoves = GameMover.validMoves game |> List.toArray
    Array.shuffle allMoves

    // let isPlayedGame move = 
    //     match Game.GameMover.playMove move game with 
    //     | Game.Continue (g,_) -> pastGames |> Set.contains g    
    //     | _ -> false

    let rec getMove moves = 
        match moves with 
        | [] ->
            None
        | move::moves -> 
            // match isPlayedGame move with 
            // | true -> getMove moves
            // | false -> 
                Some move

    allMoves |> List.ofArray |> getMove

let rec rolloutRandom depth count game = 
    let gamma = 0.9

    match getRandomMove game with 
    | None -> 
        if reward game |> int = 1 then 1. else 0.
    | Some move -> 
        match Game.GameMover.playMove move game with 
        | Game.Continue (game,s) ->  
            if count <= 0 then 
                reward game * Math.Pow(gamma, depth)
            else       
                // let pastGames = Set.add game pastGames
                rolloutRandom (depth + 1.) (count - 1) game
        | Game.Lost _ ->
            0.
        | Game.Won _ -> 
            // printfn "Won"
            1.      

// let getParents node = 
//     let rec loop acc (n: MutableNode) = 
//         match n.Parent with 
//         | None -> Set.add n.GameHashCode acc
//         | Some x -> loop (Set.add n.GameHashCode acc) x

//     loop Set.empty node

let expandNode getMoves (pastGames: IDictionary<int, Set<_>>) (node: MutableNode) = 

    // let game = decodeKeyedGame cardEncoder (node.Game.Split (',') |> Array.map Int32.Parse |> Array.toList)
    // let moves = GameMover.validMoves node.Game

    let moves = getMoves node.Game
    // let moves = brainsMover.GetBestMove node.Game

    // let parents = Set.union pastGames (getParents node)
    let parents = pastGames.[node.GameHashCode] // If this fails then it's a bug. The value should always be here. 
        
    moves 
    |> List.choose (fun (move, prob) ->
 
        match Game.GameMover.playMove move node.Game with 
        | Game.Continue (g,_) -> 
            let gHashCode = g.GetHashCode()
            if parents |> Set.contains gHashCode then 
                // printfn "Dropping circular states (%d) %d %d %A!" moves.Length parents.Count (g.GetHashCode()) move
                None
            else 
                let parentValues =  
                    match pastGames.TryGetValue gHashCode with
                    | true, existingParents -> Set.union existingParents parents  // There can be many ways to get to the same state. We must take both paths into account. 
                    | false, _ -> parents

                pastGames.[gHashCode] <- Set.add node.GameHashCode parentValues

                (g, MutableNode(g, prob, reward g, Some move, node.Depth + 1, Some node, None)) |> Some
        | Game.Lost g ->
            (g, MutableNode(g, prob, reward g, Some move, node.Depth + 1, Some node, Some 0.)) |> Some

        | Game.Won g -> 
            (g, MutableNode(g, prob, reward g, Some move, node.Depth + 1, Some node, Some 1.)) |> Some)

let updateNode node value = 
    { node with N = node.N + 1; T = node.T + value }

let setMetrics (gameToMetrics:IDictionary<int, float * float>) (node: MutableNode) value = 
    let hc = node.GameHashCode
    match gameToMetrics.TryGetValue hc with 
    | true, (t,n) -> 
        let metrics = (t + value, n + 1.)
        gameToMetrics.[hc] <- metrics
    | false,_ -> 
        let metrics = (value, 1.)
        gameToMetrics.[hc] <- metrics

// let rec updateChildren nodes (node, value) =     
//     let node = setMetrics node value
//     match nodes with 
//     | [] -> node
//     | (n, t)::xs -> updateChildren xs (n, value)

let getDepth node = 
    let rec loop depth (n: MutableNode) = 
        match n.Parent with 
        | None -> depth 
        | Some x -> loop (depth + 1) x

    loop 0 node |> float


let rec setParentsValues (gameToMetrics: IDictionary<int, float * float>) (node: MutableNode, value) =     
    setMetrics gameToMetrics node value
    match node.Parent with 
    | None -> ()
    | Some p -> setParentsValues gameToMetrics (p, value)


    
let rec iteration expandNode rollout pastGames gameToMetrics nRoot (node: MutableNode) = 

    let metrics = 
        node.Children 
        |> List.map (fun x -> 
            let (t,n) = getMetrics gameToMetrics x 
            x,t,n)

    match nextMove gameToMetrics metrics with 
    | [] -> 
        
        match node.TerminalValue with 
        | Some value -> 
            setParentsValues gameToMetrics (node, value)

        | None ->             
            let leaves = expandNode pastGames node
            match leaves with 
            | [] -> 
                let value = 0. // This is considered a terminal state (the game is trying to repeat a state). Consider playing this move a loss as it would only cause a loop. 
                setParentsValues gameToMetrics (node, value)
            | _ -> 
                // let pastGames = Set.union (leaves |> List.map fst |> Set.ofList) pastGames
                node.Children <- leaves |> List.map snd
                iteration expandNode rollout pastGames gameToMetrics nRoot node

    | (node,t,n)::tail -> 
        if n = 0. then 
            let value = rollout (getDepth node) 200 node.Game
            setParentsValues gameToMetrics (node, value)

        else 
            iteration expandNode rollout pastGames gameToMetrics nRoot node


let search log (brainsMover: IBransMover) pastGames gameToMetrics totalCount (root: MutableNode) =  
    let rec reSearch count = 
        if count <= 0 then 
            // Console.SetCursorPosition (0, Console.CursorTop)
            // let space = (List.replicate (80) " " |> String.concat "")
            // printfn "%s    " space
            ()
        else 
            let (t,n) = getMetrics gameToMetrics root
            // if count % 100 = 0 then 
            if log then 
                Console.SetCursorPosition (0, Console.CursorTop)
                let progress = 
                    let count = totalCount - count |> float
                    let progress = count / float totalCount
                    progress * 100.
                sprintf "%.2f%% P: %.2f%% T: %.10f  N: %.0f TC: %d" progress ((t / n) * 100.) t n totalCount |> Console.Write


            // let expand = expandNode brainsMover.GetBestMove

            let getMoves game = 
                let moves = GameMover.validMoves game 
                let bestMoves = 
                    brainsMover.GetBestMove game 
                    // |> List.filter (fun (g,p) -> p > 0.01)
                    |> List.filter (fun (move,_) -> moves |> List.contains move)
                    // |> List.truncate (float moves.Length * 0.75 |> Math.Round |> int)

                // printfn "------------------------------------------------"
                // moves |> List.map (sprintf "%A") |> String.concat "," |> printfn "%s"
                // bestMoves |> List.map (fst >> sprintf "%A") |> String.concat "," |> printfn "%s"
                // Set.intersect (bestMoves |> List.map fst |> Set.ofList ) (Set.ofList moves) |> Set.count |> printfn "%d"
                // printfn ""

                // if bestMoves.Length <= 2 && moves.Length > 2 then 
                //     moves |> List.map (fun x -> x, 1.0)
                // else 
                //     bestMoves

                if moves.Length <> bestMoves.Length then    
                    Set.difference (Set.ofList moves) (bestMoves |> List.map fst |> Set.ofList) |> printfn "%A"
                    printfn "We mave be missing a move in the neural network"

                // let evenProb = 1.0 / float moves.Length
                // moves |> List.map (fun x -> x, evenProb)
                bestMoves

            let expand = expandNode getMoves
            let rollout depth _ game = Math.Pow(0.9, depth) *  brainsMover.GetValue game  // recent rewards are better for the optimal policy
            // let rollout depth x game = rolloutRandom depth x game

            iteration expand rollout pastGames gameToMetrics n root
            reSearch (count - 1)

    reSearch totalCount


