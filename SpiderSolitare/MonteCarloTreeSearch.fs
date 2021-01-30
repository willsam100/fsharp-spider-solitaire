module SpiderSolitare.MonteCarloTreeSearch

open SpiderSolitare.Representation
open System
open SpiderSolitare.Game
open Microsoft.ML
open System.IO
open System.Collections.Generic
open System.Diagnostics
open SpiderSolitare
open SpiderSolitare.Representation
open SpiderSolitare.Representation
let rand = Random()
    
//let timed name f = 
//    let s = Stopwatch()
//    s.Restart()
//    let x = f ()
//    printfn "Timed %s: %A" name s.Elapsed
//    x

type CnnView = {
    ProbsOne: float list
    ProbsOneH: int 
    ProbsOneC: int

    ProbsTwo: float list
    ProbsTwoH: int 
    ProbsTwoC: int

    ProbsThree: float list
    ProbsThreeH: int 
    ProbsThreeC: int

    // ProbsFour: float list
    // ProbsFourH: int 
    // ProbsFourC: int

    // ProbsFive: float list
    // ProbsFiveH: int 
    // ProbsFiveC: int
}

type MnistView = {
    ProbsOne: float list
    ProbsOneH: int 
    ProbsOneW: int
    ProbsOneC: int

    ProbsTwo: float list
    ProbsTwoH: int 
    ProbsTwoC: int

    ProbsThree: float list
    ProbsThreeH: int 
    ProbsThreeC: int

    ProbsFour: float list
    ProbsFourH: int 
    ProbsFourC: int

    ProbsFive: float list
    ProbsFiveH: int 
    ProbsFiveC: int
}

type IBrainsMover = 
    // abstract member GetBestMove: string -> (float list * float list * float list) Async
    abstract member GetBestMove: Game -> float list Async
    abstract member GetValue: Game -> float Async
    // abstract member GetColumnOfLongestRun: Game -> (Column * float) list
    // abstract member GetCnnView: Game -> CnnView
    // abstract member GetMoves: Game -> MoveType list
    abstract member Flush: unit -> unit
    // abstract member GetMnist: string -> MnistView-

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
    member val TerminalValue: bool option = terminalValue

let inline getMetrics (gameToMetrics: IDictionary<int, float * float>) (node: MutableNode<'a, 'b>) = 
    match gameToMetrics.TryGetValue(node.GameHashCode) with 
    | true, x -> x
    | false, _ -> -10., 0.    

let nextMove parentVisitCount (nodes: (MutableNode<'a, 'b> * float * float * float) list) =

    nodes 
    |> List.map (fun (node, t, n, p) ->
        if n = 0. then 
            node, n, Double.MaxValue
        elif p >= 0.7 then
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
    elif game.Spades = Five then 
        winningNodeReward
    else
        let scoreTab cards = 
            let rec scorer depth score (cards: Card list) = 
                match cards with 
                | [] when depth = 0 -> 0.5
                | [] -> score
                | [_] when depth = 0 -> 0.
                | [_]  -> score
                | x::y::cards -> 
                    if CardModule.getValue y - 1 = CardModule.getValue x && CardModule.getSuit x = CardModule.getSuit y then 
                        scorer (depth + 1) (score * 1.3) (y :: cards)
                    else score

            (scorer 0 2. cards) / 10.

        let x = game |> Game.Game.getAllTabs |> List.sumBy (Tableau.getVisible >> scoreTab)
        
        let suitCompleted =
            match game.Spades with 
            | Zero -> 0
            | One -> 50
            | Two -> 100
            | Three -> 150
            | Four -> 200
            | Five -> 400
            | Six -> 300
            | Seven -> 350
            | Eight -> 400

        (float suitCompleted + x) * 0.0025


let encodeGame size game = 
    game |> encodeKeyGame size cardEncoder |> Seq.take (13 * 10) |> Seq.map string |> String.concat ","

let encodeGameWithStock size stock game = 
    game |> encodeKeyGame size cardEncoder |> Seq.take stock |> Seq.map string |> String.concat ","


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
   

let rec rolloutRandom pastGames depth count game = 
    let gamma = 0.9

    match getRandomMove pastGames game with 
    | None -> reward game
    | Some move -> 
        match Game.GameMover.playMove move game with 
        | Game.Continue (game,_) ->  
            if count <= 0 then 
                reward game * Math.Pow(gamma, depth)
            else       
                let pastGames = Set.add (game.GetHashCode()) pastGames
                rolloutRandom pastGames (depth + 1.) (count - 1) game
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
    
let createMutableNode (parentNode: MutableNode<'a, 'b>) move probability g siblingCount terminalValue  =
    let reward = 
        match terminalValue with 
        | Some x -> if x then winningNodeReward else -1.
        | None -> -10.

    MutableNode(g, probability, reward, Some move, parentNode.Depth + 1, Some parentNode, terminalValue, siblingCount)

let expandNode getMoves (pastGames: IDictionary<int, int Set>) (node: MutableNode<'a, 'b>) = 
    let moves = getMoves node.Game
    let parents = pastGames.[node.GameHashCode]
    
    if (node.Children |> List.length > 0) || node.TerminalValue |> Option.isSome then
        printfn "N:%d" node.GameHashCode
        printfn "R:%f" node.Reward
        printfn "T:%A" node.TerminalValue
        failwith "Tree is invalid - we should not be expanding."
    
    let shouldAddNodeToTree (nextGame: MutableNode<'a, 'b>) =
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
            |> shouldAddNodeToTree
            
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

let rec findBestScore getMetrics (root: MutableNode<Game, 'b>) = 
    if root.Children.IsEmpty then
        root.Reward
    else 
        root.Children 
        |> List.maxBy (getMetrics >> snd)
        |> findBestScore getMetrics
    
let printTreeWithDepth maxDepth (gameToMetrics: IDictionary<int, float * float>) (node:MutableNode<'a, 'b>) =
    let rec loop depth (node: MutableNode<'a, 'b>) =
        let s = List.replicate depth "  " |> String.concat ""
        let (t,n) = getMetrics gameToMetrics node
        printfn "%s%d (%0.4f, %d) isTerm:%A R:%f CC:%d" s node.GameHashCode t (int n) node.TerminalValue node.Reward node.Children.Length
        if depth < maxDepth then 
            node.Children
            |> List.sortByDescending (fun x -> getMetrics gameToMetrics x |> snd)
            |> List.iter (loop (depth + 1)) 

    loop 0 node

let printTreeWithDepthAndWidth maxWidth maxDepth (gameToMetrics: IDictionary<int, float * float>) (node:MutableNode<'a, 'b>) =
    let rec loop depth (node: MutableNode<'a, 'b>) =
        let s = List.replicate depth "|  " |> String.concat ""
        let (t,n) = getMetrics gameToMetrics node
        printfn "%s%d (%0.4f, %d) isTerm:%A R:%f ChildrenCount:%d Prob:%f (d:%d) %A" s node.GameHashCode t (int n) node.TerminalValue node.Reward node.Children.Length node.Prob depth node.Move
        if depth < maxDepth then 
            node.Children
            // |> List.sortByDescending (fun x -> getMetrics gameToMetrics x |> snd)
            |> List.sortByDescending (fun x -> x.Prob)
            |> List.truncate maxWidth
            |> List.iter (loop (depth + 1)) 

    loop 0 node

let getMoves (node: MutableNode<'a, 'b>) = 
    let rec getMoves moves (node: MutableNode<'a, 'b>) = 
        match node.Parent with
        | Some x -> getMoves (x:: moves) x
        | None -> List.tail moves
    getMoves [node] node


let iteration log logger getMaxReward maxDepth depth expandNode rollout gameToMetrics (nRoot: MutableNode<'a, 'b>) =
    
    let rec loop depth (node: MutableNode<'a,'b>) =
        logger nRoot

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
                if node.Reward = winningNodeReward then 
                    getMoves node, 0.
                else
                    if log then printfn "\nEnd of the line..."
                    node.Reward <- -0.99
                    setParentsValues gameToMetrics node node.Reward
                    [], node.Reward

            | None ->
                let leaves = expandNode node
                match leaves with 
                | [] ->
                    if node.Reward = winningNodeReward then 
                        printfn "Error (or maybe). This is happening."
                        printfn "R:%A T:%A" node.Reward node.TerminalValue
                        printfn "%A" node.Game
                        getMoves node, 0.
                    else 
                        // This is considered a terminal state (the game is trying to repeat a state). C
                        // Consider playing this move a loss as it would only cause a loop.
                        let value = -0.99
                        setParentsValues gameToMetrics node value
                        if log then printfn "\nEnd of the line (2)..."
                        [], -0.99

                | _ ->
                    node.Children <- leaves

                    node.Children 
                    |> List.filter (fun x -> x.Reward = -10.)
                    |> List.map (fun (nextNode: MutableNode<'a, 'b>) -> 
                        async {
                            let! reward = rollout (nextNode.Game, nextNode.GameHashCode, 50)
                            nextNode.Reward <- reward
                        })
                    |> Async.Parallel
                    |> Async.Ignore
                    |> Async.RunSynchronously

                    node.Children 
                    |> List.iter (fun (nextNode: MutableNode<'a, 'b>) ->
                        setParentsValues gameToMetrics nextNode nextNode.Reward   
                    )

                    let maxNode = node.Children |> List.maxBy (fun x -> x.Reward)
                    let maxProb = node.Children |> List.maxBy (fun x -> x.Prob)

                    if depth > maxDepth then
                        if log then printfn "\nHit Max Depth (%.0f): %f" depth maxNode.Reward
                        [], maxNode.Reward
                    elif maxNode.Reward = winningNodeReward then
                        if log then printfn "\nFound gold: %f"maxNode.Reward
                        getMoves maxNode, 0.
                    // elif maxProb.Prob < 0.5 then
                    //     if log then printfn "\nNot confident: %f"maxNode.Prob
                    //     printTreeWithDepthAndWidth 2 120 gameToMetrics nRoot

                    //     let parent = maxProb.Parent |> Option.get
                    //     printfn "%A" parent.Game

                    //     printfn "%A" (Option.get maxProb.Move)
                    //     printfn "%A" maxProb.Game
                    //     maxProb.Game  |> MoveEncoding.sortGame |> snd |> printfn "%A" 

                    //     [], maxProb.Reward
                    else 
                        loop (depth + 1.) node
                    
        | (nextNode: MutableNode<'a,'b>,n)::_ ->            
            if n = 0. then
                nextNode.Reward <- rollout (nextNode.Game, nextNode.GameHashCode, 50) |> Async.RunSynchronously
                if log then printfn "\nExplored the unexplored: %f" nextNode.Reward

                if nextNode.Reward = winningNodeReward then 
                    getMoves nextNode, 0.
                else [], nextNode.Reward
                
            else
                loop (depth + 1.) nextNode
                
    loop depth nRoot

let getDepth node = 
    let rec loop depth (n: MutableNode<'a, 'b>) = 
        match n.Parent with 
        | None -> depth 
        | Some x -> loop (depth + 1) x

    loop 0 node |> float


let logger log gameToMetrics getTimeRemaining root maxReward maxDepth =
    let (t,n) = getMetrics gameToMetrics root
    if log then 
        let tm: TimeSpan = getTimeRemaining()
        Console.SetCursorPosition (0, Console.CursorTop)
        List.replicate 150 " " |> String.concat "" |> Console.Write
        Console.SetCursorPosition (0, Console.CursorTop)
        let progress = tm.ToString("hh\:mm\:ss")
        let s = sprintf "%s P: %.2f%% T: %.2f MaxReward: %.4f  N: %.0f MD:%.0f" progress ((t / n) * 100.) t maxReward n maxDepth

        if tm.Seconds = 0 && tm.Milliseconds < 1 then     
                // let progress = 
                    // let count = totalCount - count |> float
                    // let progress = count / float totalCount
                    // progress * 100.
            s |> Console.WriteLine
                
        else s |> Console.Write

type ISearcher = 
    abstract member Search: root:MutableNode<Game, MoveType> -> MutableNode<Game, MoveType> list
    abstract member GetProgress: unit ->  float list
    abstract member BrainServer: IBrainsMover option
    abstract member Init: MutableNode<Game,MoveType> -> unit 
    abstract member GetMetrics: MutableNode<'a, 'b> -> float * float
    abstract member PrintTree: maxDepth:int -> root:MutableNode<Game, MoveType> -> unit
    abstract member ResetMaxReward: unit -> unit
    abstract member MaxTime: TimeSpan
    abstract member Copy: unit -> ISearcher

// type SearcherRandomly(log) = 
//     let pastGames = Dictionary<_, _>()
//     let gameToMetrics = Dictionary<_, _>()
//     let logger = logger log gameToMetrics

//     interface ISearcher with 
//         member this.MaxTime = TimeSpan.Zero
//         member this.ResetMaxReward() = ()

//         member this.Copy() = 
//             SearcherRandomly(log) :> ISearcher

//         member this.Init root = 
//             pastGames.[root.GameHashCode] <-Set.empty

//         member this.GetProgress() = []
//         member ths.BrainServer = None

//         member this.GetMetrics(node: MutableNode<'a, 'b>) = 
//             getMetrics gameToMetrics node

//         member this.PrintTree maxDepth rootNode = 
//             printTreeWithDepthAndWidth 3 maxDepth gameToMetrics rootNode

//         member this.Search maxLookAhead root = 
//             let getMovesRandom game = 
//                 let moves = GameMover.validMoves game
                
//                 moves |> List.map (fun x ->
                    
//                     let g, isTerminal = 
//                         match GameMover.playMove x game with 
//                         | Game.Continue (g,_) -> g, None
//                         | Game.Lost g -> g, Some false
//                         | Game.Won g -> g, Some true
                    
//                     g, isTerminal, x, Math.Max(reward game * 2., 1.0)) 
                        
//             let rolloutRandom (game, gameHashCode, depth) =
//                 async {
//                     do! Async.SwitchToThreadPool()
//                     let pastGames = pastGames.[gameHashCode]
//                     return rolloutRandom pastGames (Set.count pastGames |> float) depth game
//                 }
                
//             let expandRandom node =
//                 expandNode getMovesRandom pastGames node //|> List.distinctBy (fun x -> reward x.Game)
            
//             let rec reSearch count = 
//                 let maxDepth = 100.
//                 logger (fun () -> TimeSpan.FromHours 1.) root -1. maxDepth
//                 if count > 0 then 
//                     let moves = iteration log (fun root -> logger (fun () -> TimeSpan.FromHours 1.) root -1. maxDepth) (fun () -> 1.) maxDepth 0. expandRandom rolloutRandom gameToMetrics root |> ignore
//                     reSearch (count - 1)
//                 else failwithf "TOOD: make this return the list of moves"
//             reSearch totalCount

type SearcherWithNeuralNetwork(brainsMover: IBrainsMover, log, maxTime) =
    let mutable winningNode = None
    let mutable progress: float list = []
    let pastGames = Dictionary<_, _>()
    let gameToMetrics = Dictionary<_, _>()
    let logger = logger log gameToMetrics
    let mutable maxReward = -10.
    let watch = Stopwatch()
    let maxLookAhead = 250.

    let toPolicyStructure (game:Game) = 
        let transform left right =        
            let game = transformGame game left right
            game, (left, right)

        applyTransform transform
        |> List.sortBy (fun (x,_) -> x |> Game.getAllTabs |> List.map Tableau.getVisible |> List.concat)
        |> List.take 1
        |> List.map (fun (game, (l, r)) -> l,r,game)
        |> List.head

    interface ISearcher with 
        member this.Copy() = 
            SearcherWithNeuralNetwork(brainsMover, log, maxTime) :> ISearcher

        member this.Init root = 
            pastGames.[root.GameHashCode] <- Set.empty

        member this.MaxTime = maxTime

        member this.ResetMaxReward() = 
            maxReward <- -10.

        member this.GetProgress() = []
        member ths.BrainServer = None

        member this.GetMetrics(node: MutableNode<'a, 'b>) = 
            getMetrics gameToMetrics node

        member this.PrintTree maxDepth rootNode = 
            printTreeWithDepth maxDepth gameToMetrics rootNode

        member this.Search root = 
            watch.Restart()
            maxReward <- -10.

            let rollout (game: Game, gameHashCode: int, depthSearch) = 
                // let depth = pastGames.[gameHashCode].Count |> float
                // let cardCount = game |> Game.Game.getAllTabs |> List.sumBy Tableau.length
                async {
                    match Game.isComplete game with 
                    | true -> return winningNodeReward
                    | false -> return! brainsMover.GetValue game
                }

            let rolloutRandom (game, gameHashCode, depth) =
                let pastGames = pastGames.[gameHashCode]
                rolloutRandom pastGames (Set.count pastGames |> float) depth game
            
            let expand (node: MutableNode<Game, MoveType>) =
                let getMoves game = 
                    let moves = GameMover.validMoves game

                    let mapping, gPolicy = MoveEncoding.sortGame game

                    let bestMoves = 
                        brainsMover.GetBestMove gPolicy
                        |> Async.RunSynchronously
                        |> List.mapi (fun i x -> i,x)
                        |> List.choose (fun (index,score) -> 
                            MoveEncoding.oneHotToMove game index
                            |> Option.map (MoveEncoding.reverseMove mapping)
                            |> Option.map (fun move -> move, score))
                        // |> fun xs -> (Stock, 0.4) :: xs
                        |> List.sortByDescending snd
                        |> List.filter (fun (move,_) -> moves |> List.contains move)

                    if moves.Length > bestMoves.Length then
                        progress <- float (moves.Length - bestMoves.Length) / float moves.Length :: progress

                    // bestMoves  |> List.iter (fun (m,p) -> printfn "%A, %.4f" m p)

                    let bestMoves = 
                        List.fold (fun bestMoves validMove -> 
                            if bestMoves |> List.map fst |> List.contains validMove then 
                                bestMoves
                            else 
                                (validMove, 0.001) :: bestMoves
                        ) bestMoves moves

                    bestMoves
                    |> List.map (fun (x,y) ->
                        let g, isTerminal = 
                            match GameMover.playMove x game with 
                            | Game.Continue (g,_) -> g, None
                            | Game.Lost g -> g, Some false
                            | Game.Won g -> g, Some true
                        
                        g, isTerminal, x, y)
                
                expandNode getMoves pastGames node
                // |> List.distinctBy (fun x -> reward x.Game, x.TerminalValue)

            let startValue = Math.Min(maxLookAhead, 10.)
            let increment = 40. // (maxLookAhead - startValue) / float totalCount

            let rec reSearch maxDepth = 
                let timeRemaining () = maxTime - watch.Elapsed
                logger timeRemaining root maxReward maxDepth                
                if watch.Elapsed < maxTime then
                    
                    let moves, reward = iteration log (fun root -> logger timeRemaining root maxReward maxDepth) (fun () -> maxReward) maxDepth 0. expand rollout gameToMetrics root

                    if reward > maxReward then 
                        maxReward <- reward

                    match moves with 
                    | [] -> 
                        let maxDepth = 
                            if maxDepth = maxLookAhead then startValue
                            else Math.Min(maxDepth + increment, maxLookAhead)
                        reSearch maxDepth
                    | moves -> 
                        printfn "\nFound WINING node!"
                        moves
                else
                    printfn "\nFailed to find winning path within time limit (%A)" maxTime
                    []

            reSearch startValue
