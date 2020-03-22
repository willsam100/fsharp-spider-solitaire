module SpiderSolitare.MonteCarloTreeSearch

open SpiderSolitare.Representation
open System
open System
open System
open SpiderSolitare.Game
open Microsoft.ML
open SpiderSolitare.Game
open SpiderSolitare.Game
open SpiderSolitare.Game
open SpiderSolitare.Game
open System.IO
open System.Collections.Generic
open System.Collections.Generic
open System.Diagnostics

let rand = Random()
let cardEncoder = CardEncoderKeyed()

let encodeMove (m: int16[]) = 
    let move = int16 1
    m |> Array.findIndex (fun x -> x = move)

let decodeMove int = 
    let x = Array.replicate 1171 0
    x.[int] <- 1
    x
    
//let timed name f = 
//    let s = Stopwatch()
//    s.Restart()
//    let x = f ()
//    printfn "Timed %s: %A" name s.Elapsed
//    x

type IBransMover = 
    abstract member GetBestMove: Game -> (MoveType * float) list
    abstract member GetValue: Game -> float
    abstract member GetMoves: Game -> MoveType list
    abstract member Flush: unit -> unit

module Array = 
    let swap (a: _[]) x y =
        let tmp = a.[x]
        a.[x] <- a.[y]
        a.[y] <- tmp

    // shuffle an array (in-place)
    let shuffle a =
        Array.iteri (fun i _ -> swap a i (rand.Next(i, Array.length a))) a


type MutableNode<'a ,'b when 'a :equality>(game:'a, prob: float, reward: float, move: 'b option, depth: int, parent, terminalValue, siblingCount) = 
    // member val N = 0. with get,set
    // member val T = 0. with get, set
    member val Game = game
    member val GameHashCode = game.GetHashCode()
    member val Prob = prob
    member val Reward = reward with get, set
    member val Move = move
    member val SiblingCount: int = siblingCount
    member val Depth = depth
    member val Children: MutableNode<'a, 'b> list = [] with get,set
    member val Parent: MutableNode<'a, 'b> option = parent with get, set
    member val TerminalValue: bool option = terminalValue with get, set

let inline getMetrics (gameToMetrics: IDictionary<int, float * float>) (node: MutableNode<'a, 'b>) = 
    match gameToMetrics.TryGetValue(node.GameHashCode) with 
    | true, x -> x
    | false, _ -> 0., 0.    

let nextMove parentVisitCount (nodes: (MutableNode<'a, 'b> * float * float * float) list) =

    nodes 
    |> List.map (fun (node, t, n, p) ->
        if n = 0. then 
            node, n, Double.MaxValue
        else 
            node, n, (2. * p * Math.Sqrt(Math.Log parentVisitCount ) / n ) + (t / n) 
        )
    |> List.sortByDescending (fun (_, _, ucb1) -> ucb1)
    |> List.map (fun (x, y, _) -> x,y)

let winningNodeReward = 10.

let reward game =
    
    if game |> Game.Game.getAllTabs |> List.sumBy (Tableau.getVisible >> List.length) = 0 then
        winningNodeReward
    else
        let scoreTab cards = 
            let rec scorer depth score cards = 
                match cards with 
                | [] when depth = 0 -> 0.01
                | [] -> score
                | [_] when depth = 0 -> 0.
                | [_]  -> score
                | x::y::cards -> 
                    if CardModule.getValue y - 1 = CardModule.getValue x && CardModule.getSuit x = CardModule.getSuit y then 
                        scorer (depth + 1) (score * 2.) (y :: cards)
                    else score

            (scorer 0 1.8 cards) / 10.

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

        (float suitCompleted + x) * 0.0025


let encodeGame game = 
    game |> encodeKeyGame cardEncoder |> Seq.map string |> String.concat ","


let getRandomMove pastGames game = 
    let allMoves = GameMover.validMoves game |> List.toArray
    Array.shuffle allMoves

    let isPlayedGame move = 
        match Game.GameMover.playMove move game with 
        | Game.Continue (g,_) -> pastGames |> Set.contains (g.GetHashCode())
        | _ -> false

    let rec getMove moves = 
        match moves with 
        | [] ->
            None
        | move::moves -> 
            match isPlayedGame move with 
            | true -> getMove moves
            | false -> Some move

    allMoves |> List.ofArray |> getMove
   

let rec rolloutRandom pastGames (siblingCount: int) depth count game = 
    let gamma = 0.9

    match getRandomMove pastGames game with 
    | None -> reward game
    | Some move -> 
        match Game.GameMover.playMove move game with 
        | Game.Continue (game,_) ->  
            if count <= 0 then 
                let r = reward game * Math.Pow(gamma, depth)
                r / float siblingCount
            else       
                let pastGames = Set.add (game.GetHashCode()) pastGames
                rolloutRandom pastGames siblingCount (depth + 1.) (count - 1) game
        | Game.Lost _ -> 0.
        | Game.Won g -> reward g 

// let getParents node = 
//     let rec loop acc (n: MutableNode) = 
//         match n.Parent with 
//         | None -> Set.add n.GameHashCode acc
//         | Some x -> loop (Set.add n.GameHashCode acc) x

//     loop Set.empty node

type ExpandOperation<'a, 'b when 'a : equality> =
    | AddLeaf of MutableNode<'a, 'b>
    | NoAdd // Leaf is in the tree on the shortest path
    | Remove of int * int list * int list
    
type ExOp<'a, 'b when 'a : equality> =
    | AddLeaf of MutableNode<'a, 'b>
    | LinkSeenNode of (int * int list * int list)
    
let createMutableNode (parentNode: MutableNode<'a, 'b>) move probabiltiy g siblingCount terminalValue  =
    MutableNode(g, probabiltiy, 0., Some move, parentNode.Depth + 1, Some parentNode, terminalValue, siblingCount)

let expandNode getMoves (pastGames: IDictionary<int, int Set>) (node: MutableNode<'a, 'b>) = 
    let moves = getMoves node.Game
    let parents = pastGames.[node.GameHashCode]
    
    if (node.Children |> List.length > 0) || node.TerminalValue |> Option.isSome then
        printfn "N:%d" node.GameHashCode
        printfn "R:%f" node.Reward
        printfn "T:%A" node.TerminalValue
        failwith "Tree is invalid - we should not be expanding."
    
    let shouldAddNodeToTreee (nextGame: MutableNode<'a, 'b>) =
        if parents |> Set.contains nextGame.GameHashCode then
            None
        else
            match pastGames.TryGetValue nextGame.GameHashCode with
            | false, _ -> Some nextGame
            | true, existingParents ->
                if Set.count parents + 1 < Set.count existingParents then
                    Some nextGame
                else 
                    None

    let updatePathToParents g =
        let gHashCode = g.GetHashCode()
        if parents |> Set.contains gHashCode |> not then
            match pastGames.TryGetValue gHashCode with
            | true, existingParents ->
                
                if existingParents |> Set.contains gHashCode then
                    failwith "You have circle shit!"
                
                if Set.count parents + 1 < Set.count existingParents then
                    pastGames.[gHashCode] <- Set.add node.GameHashCode parents
                else 
                    pastGames.[gHashCode] <- existingParents
            
            | false, _ ->
                pastGames.[gHashCode] <- Set.add node.GameHashCode parents
            
    moves 
    |> List.choose (fun (g, terminalValue, move, prob) -> 
        let expandOperation =
            terminalValue
            |> createMutableNode node move prob g moves.Length
            |> shouldAddNodeToTreee
            
        updatePathToParents g 
        expandOperation ) 
    

let setMetrics (gameToMetrics:IDictionary<int, float * float>) (gameHashCode: int) value = 
    match gameToMetrics.TryGetValue gameHashCode with 
    | true, (t,n) -> gameToMetrics.[gameHashCode] <- (t + value, n + 1.)
    | false,_ -> gameToMetrics.[gameHashCode] <- (value, 1.)
    
let rec setParentsValues (gameToMetrics: IDictionary<int, float * float>) (node: MutableNode<'a,'b>) value =
    setMetrics gameToMetrics node.GameHashCode value
    match node.Parent with
    | Some p -> setParentsValues gameToMetrics p value
    | None -> ()

let printTree (gameToMetrics: IDictionary<int, float * float>) (node:MutableNode<'a, 'b>) =
    let rec loop depth (node: MutableNode<'a, 'b>) =
        let s = List.replicate depth "    " |> String.concat ""
        let (t,n) = getMetrics gameToMetrics node
        printfn "%s%d (%0.4f, %d) isTerm:%A R:%f CC:%d" s node.GameHashCode t (int n) node.TerminalValue node.Reward node.Children.Length
        if depth < 75 then 
            node.Children
            |> List.sortByDescending (fun x -> x.Children.Length)
            |> List.iter (loop (depth + 1)) 

    loop 0 node
    
let printTreeWithDepth maxDepth (gameToMetrics: IDictionary<int, float * float>) (node:MutableNode<'a, 'b>) =
    let rec loop depth (node: MutableNode<'a, 'b>) =
        let s = List.replicate depth "    " |> String.concat ""
        let (t,n) = getMetrics gameToMetrics node
        printfn "%s%d (%0.4f, %d) isTerm:%A R:%f CC:%d" s node.GameHashCode t (int n) node.TerminalValue node.Reward node.Children.Length
        if depth < maxDepth then 
            node.Children
            |> List.sortByDescending (fun x -> getMetrics gameToMetrics x |> snd)
            |> List.iter (loop (depth + 1)) 

    loop 0 node

let iteration (trialCount: int) depth expandNode rollout (pastGames: IDictionary<int, int Set>) gameToMetrics (nRoot: MutableNode<'a, 'b>) =
    
    let rec loop depth (node: MutableNode<'a,'b>) =

        let metrics = 
            node.Children 
            |> List.map (fun x -> 
                let (t,n) = getMetrics gameToMetrics x 
                x, t, n, x.Prob)
            
        let parentVisitCount =
            getMetrics gameToMetrics node |> snd
        
        match nextMove parentVisitCount metrics with 
        | [] ->
            
            match node.TerminalValue with 
            | Some _ ->
                let (_,n) = getMetrics gameToMetrics node
                setParentsValues gameToMetrics node node.Reward
                
                let (_,n') = getMetrics gameToMetrics node
                if n' = n then
                    let nodes = pastGames.[node.GameHashCode] |> Set.add node.GameHashCode 
                    printTree gameToMetrics nRoot
                    printfn "%A" <| Set.toList nodes
                    printfn "%f" node.Reward
                    failwithf "Why are we not updating"

            | None ->
                let leaves = expandNode node
                match leaves with 
                | [] ->
                    // This is considered a terminal state (the game is trying to repeat a state). C
                    // Consider playing this move a loss as it would only cause a loop.
                    let value = 0.
    //                    let nodes = pastGames.[node.GameHashCode] |> Set.add node.GameHashCode
                    setParentsValues gameToMetrics node value

                | _ ->
                    node.Children <- leaves
                    loop (depth + 1.) node
                    
        | (nextNode: MutableNode<'a,'b>,n)::_ ->            
            if n = 0. then
                nextNode.Reward <- rollout (node.Game, node.GameHashCode, 0, 50)
                
                let numberOfParentUpdates = 
                    if nextNode.Reward = winningNodeReward then 1000 else 1
                
                // Once we have found the winning node we must manipulate the higher layers to reflect that we have found the winning node.
                // It would be better if we could skip out of the loop, and just returns the moves walking back up the tree.     
                for _ in [1 .. numberOfParentUpdates] do
                    setParentsValues gameToMetrics nextNode nextNode.Reward    
            
                let (_,n) = getMetrics gameToMetrics nextNode
                if n = 0. then
                    let nodes = pastGames.[nextNode.GameHashCode] |> Set.add nextNode.GameHashCode
                    printTree gameToMetrics nRoot
                    printfn "%A" (nodes)
                    failwithf "We did not update the node: %d" nextNode.GameHashCode
                
            else
                loop (depth + 1.) nextNode
                
    loop depth nRoot

let getDepth node = 
    let rec loop depth (n: MutableNode<'a, 'b>) = 
        match n.Parent with 
        | None -> depth 
        | Some x -> loop (depth + 1) x

    loop 0 node |> float
    
type Searcher(log, brainsMover: IBransMover, pastGames: IDictionary<int, int Set>, gameToMetrics) =
    let mutable winningNode = None
    
    let log count totalCount root =
        let (t,n) = getMetrics gameToMetrics root
        if count <= 0 then 
            if log then 
                // Console.SetCursorPosition (0, Console.CursorTop)
                let progress = 
                    let count = totalCount - count |> float
                    let progress = count / float totalCount
                    progress * 100.
                List.replicate 100 " " |> String.concat "" |> Console.WriteLine
                sprintf "%.2f%% P: %.2f%% T: %.10f  N: %.0f TC: %d" progress ((t / n) * 100.) t n totalCount |> Console.WriteLine
                
        else 
            // if count % 100 = 0 then 
            if log then 
                Console.SetCursorPosition (0, Console.CursorTop)
                let progress = 
                    let count = totalCount - count |> float
                    let progress = count / float totalCount
                    progress * 100.
                sprintf "%.2f%% P: %.2f%% T: %.10f  N: %.0f TC: %d" progress ((t / n) * 100.) t n totalCount |> Console.Write
    
    
    member this.SearchRandom(totalCount, (root: MutableNode<Game, MoveType>)) =
        
        let getMovesRandom game = 
            let moves = GameMover.validMoves game
            
            moves |> List.map (fun x ->
                
                let g, isTerminal = 
                    match GameMover.playMove x game with 
                    | Game.Continue (g,_) -> g, None
                    | Game.Lost g -> g, Some false
                    | Game.Won g -> g, Some true
                
                g, isTerminal, x, float (1/List.length moves))
                    
        let rolloutRandom (game, gameHashCode, siblingCount: int, depth) =
            let pastGames = pastGames.[gameHashCode]
            rolloutRandom pastGames siblingCount (Set.count pastGames |> float) depth game
            
        let expandRandom node =
            expandNode getMovesRandom pastGames node |> List.distinctBy (fun x -> reward x.Game)
        
        let rec reSearch count = 
            log count totalCount root
            if count > 0 then 
                iteration (totalCount - count) 0. expandRandom rolloutRandom pastGames gameToMetrics root
                reSearch (count - 1)
        reSearch totalCount
        
    member this.SearchWithNN(totalCount, (root: MutableNode<Game, MoveType>)) =
        
        let rollout (game: Game, gameHashCode: int, siblingCount: int, depthSearch) = 
            let depth = pastGames.[gameHashCode].Count |> float
            let cardCount = game |> Game.Game.getAllTabs |> List.sumBy (Tableau.getVisible >> List.length)

            match Game.isComplete game with 
            | true ->
                if cardCount = 0 then
//                    winningNode <- Some gameHashCode
                    winningNodeReward
                else 0.
                    
            | false ->

                let decay = 0.9 //if cardCount > 26 then 0.9 else 0.2 
                let b = brainsMover.GetValue game
                let score = reward game
                let reward =
                        Math.Pow(decay, depth) * b // recent rewards are better for the optimal policy
                            + (2. * (Math.Pow(decay, depth) * score))
                    
                reward // float (Math.Max(siblingCount, 1))
        
        let expand (node: MutableNode<Game, MoveType>) =
            let getMoves game = 
                let moves = GameMover.validMoves game
                let bestMoves = 
                    brainsMover.GetBestMove game
                    |> List.filter (fun (move,_) -> moves |> List.contains move)
                    
                if moves.Length <> bestMoves.Length then    
                    printfn "We have been missing a move in the neural network"
                    Set.difference (Set.ofList moves) (bestMoves |> List.map fst |> Set.ofList) |> Set.toList |> printfn "%A"

                bestMoves
                |> List.map (fun (x,y) ->
                    let g, isTerminal = 
                        match GameMover.playMove x game with 
                        | Game.Continue (g,_) -> g, None
                        | Game.Lost g -> g, Some false
                        | Game.Won g -> g, Some true
                    
                    g, isTerminal, x, y)
            
            expandNode getMoves pastGames node
            |> List.distinctBy (fun x -> reward x.Game)
        
        let rec reSearch count = 
            log count totalCount root
            if count > 0 then
//                printTreeWithDepth 1 gameToMetrics root
                iteration (totalCount - count) 0. expand rollout pastGames gameToMetrics root
                
                match winningNode with
                | None ->
                    reSearch (count - 1)
                | Some gameHashCode ->
                    true
            else false

    //    let (t,n) = getMetrics gameToMetrics root
    //    // if log then 
    //    if t > 100. && n > 100. then 
    //        () 
    //    else 
    //        if t / n < 0.01 && n > 2000. then 
    //            reSearch (totalCount / 4) 
    //        else 
        reSearch totalCount
    
