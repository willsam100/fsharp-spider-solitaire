module SpiderSolitare.MonteCarloTreeSearch

open SpiderSolitare.Representation
open System
open SpiderSolitare.Game
open Microsoft.ML
open System.IO
open System.Collections.Generic
open System.Diagnostics
open SpiderSolitare

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
    abstract member GetBestMove: Game -> (MoveType * float) list Async
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
    | false, _ -> 0., 0.    

let nextMove parentVisitCount (nodes: (MutableNode<'a, 'b> * float * float * float) list) =

    nodes 
    |> List.map (fun (node, t, n, p) ->
        if n = 0. then 
            node, n, Double.MaxValue
        else 
            node, n, (1. * p * Math.Sqrt(Math.Log parentVisitCount ) / n ) + (t / n) 
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

let encodeGameWtihStock size stock game = 
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
    
let createMutableNode (parentNode: MutableNode<'a, 'b>) move probabiltiy g siblingCount terminalValue  =
    let reward = 
        match terminalValue with 
        | Some x -> if x then winningNodeReward else -1.
        | None -> 0.

    MutableNode(g, probabiltiy, reward, Some move, parentNode.Depth + 1, Some parentNode, terminalValue, siblingCount)

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
        let s = List.replicate depth "  " |> String.concat ""
        let (t,n) = getMetrics gameToMetrics node
        printfn "%s%d (%0.4f, %d) isTerm:%A R:%f ChildrenCount:%d" s node.GameHashCode t (int n) node.TerminalValue node.Reward node.Children.Length 
        if depth < maxDepth then 
            node.Children
            |> List.sortByDescending (fun x -> getMetrics gameToMetrics x |> snd)
            |> List.truncate maxWidth
            |> List.iter (loop (depth + 1)) 

    loop 0 node

let rec printMoves gameToMetrics (node: MutableNode<'a, 'b>) = 
    let (t,n) = getMetrics gameToMetrics node
    printfn "%d (%0.4f, %d) isTerm:%A R:%f ChildrenCount:%d %A" node.GameHashCode t (int n) node.TerminalValue node.Reward node.Children.Length node.Move
    match node.Parent with 
    | Some x -> printMoves gameToMetrics x
    | None -> ()

let iteration logger maxDepth depth expandNode rollout gameToMetrics (nRoot: MutableNode<'a, 'b>) =
    
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
                // let (_,n) = getMetrics gameToMetrics node
                setParentsValues gameToMetrics node node.Reward

                // if node.Reward = winningNodeReward then 
                //     printfn "Terminal Value"
                //     printMoves gameToMetrics node
                node.Reward
                
                // let (_,n') = getMetrics gameToMetrics node
                // if n' = n then
                //     let nodes = pastGames.[node.GameHashCode] |> Set.add node.GameHashCode 
                //     printTree gameToMetrics nRoot
                //     printfn "%A" <| Set.toList nodes
                //     printfn "%f" node.Reward
                //     failwithf "Why are we not updating"

            | None ->
                let leaves = expandNode node
                match leaves with 
                | [] ->
                    // This is considered a terminal state (the game is trying to repeat a state). C
                    // Consider playing this move a loss as it would only cause a loop.
                    let value = 0.
    //                    let nodes = pastGames.[node.GameHashCode] |> Set.add node.GameHashCode
                    setParentsValues gameToMetrics node value
                    value

                | _ ->
                    node.Children <- leaves

                    node.Children 
                    |> List.filter (fun x -> x.Reward = 0.)
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
                        let numberOfParentUpdates = if nextNode.Reward = winningNodeReward then 1000 else 1
                        
                        // Once we have found the winning node we must manipulate the higher layers to reflect that we have found the winning node.
                        // It would be better if we could skip out of the loop, and just returns the moves walking back up the tree.     
                        for i in [1 .. numberOfParentUpdates] do
                            setParentsValues gameToMetrics nextNode nextNode.Reward   
                    )

                    let maxNode = node.Children |> List.maxBy (fun x -> x.Reward)

                    if depth > maxDepth || maxNode.Reward = winningNodeReward then 
                        // if maxNode.Reward = winningNodeReward then 
                        //     printfn "Expanded value"
                        //     printMoves gameToMetrics maxNode

                        maxNode.Reward
                    else 
                        loop (depth + 1.) node
                    
        | (nextNode: MutableNode<'a,'b>,n)::_ ->            
            if n = 0. then
                nextNode.Reward <- rollout (nextNode.Game, nextNode.GameHashCode, 50) |> Async.RunSynchronously

                // if depth % 3. = 0. then 
                //     nextNode.Reward <- rollout (nextNode.Game, nextNode.GameHashCode, 0, 50)
                // else nextNode.Reward <- 0.

                let numberOfParentUpdates = 
                    if nextNode.Reward = winningNodeReward then 1000 else
                    1
                
                // Once we have found the winning node we must manipulate the higher layers to reflect that we have found the winning node.
                // It would be better if we could skip out of the loop, and just returns the moves walking back up the tree.     
                for _ in [1 .. numberOfParentUpdates] do
                    setParentsValues gameToMetrics nextNode nextNode.Reward    
            
                // let (_,n) = getMetrics gameToMetrics nextNode
                // if n = 0. then
                //     let nodes = pastGames.[nextNode.GameHashCode] |> Set.add nextNode.GameHashCode
                //     printTree gameToMetrics nRoot
                //     printfn "%A" (nodes)
                //     failwithf "We did not update the node: %d" nextNode.GameHashCode

                // if node.Reward = winningNodeReward then 
                //     printfn "Single value"
                //     printMoves gameToMetrics node

                node.Reward
                
            else
                loop (depth + 1.) nextNode
                
    loop depth nRoot

let getDepth node = 
    let rec loop depth (n: MutableNode<'a, 'b>) = 
        match n.Parent with 
        | None -> depth 
        | Some x -> loop (depth + 1) x

    loop 0 node |> float


let logger log gameToMetrics count totalCount root maxReward maxDepth =
    let (t,n) = getMetrics gameToMetrics root
    if log then 
        if count <= 0 then     
                let progress = 
                    let count = totalCount - count |> float
                    let progress = count / float totalCount
                    progress * 100.
                Console.SetCursorPosition (0, Console.CursorTop)
                List.replicate 100 " " |> String.concat "" |> Console.Write
                Console.SetCursorPosition (0, Console.CursorTop)
                sprintf "%.2f%% P: %.2f%% T: %.2f MaxReward: %.4f  N: %.0f TC: %d MD:%.0f" progress ((t / n) * 100.) t maxReward n totalCount maxDepth |> Console.WriteLine
                
        else 
            // if count % 100 = 0 then 
                Console.SetCursorPosition (0, Console.CursorTop)
                let progress = 
                    let count = totalCount - count |> float
                    let progress = count / float totalCount
                    progress * 100.
                sprintf "%.2f%% P: %.2f%% T: %.2f MaxReward: %.4f  N: %.0f TC: %d MD:%.0f" progress ((t / n) * 100.) t maxReward n totalCount maxDepth |> Console.Write

type ISearcher = 
    abstract member Search:  totalCount:int -> maxLookAhead:float -> root:MutableNode<Game, MoveType> -> bool
    abstract member GetProgress: unit ->  float list
    abstract member BrainServer: IBrainsMover option
    abstract member Init: MutableNode<Game,MoveType> -> unit 
    abstract member GetMetrics: MutableNode<'a, 'b> -> float * float
    abstract member PrintTree: maxDepth:int -> root:MutableNode<Game, MoveType> -> unit
    abstract member ResetMaxReward: unit -> unit
    abstract member MaxTime: TimeSpan

type SearcherRandomer(log) = 
    let pastGames = Dictionary<_, _>()
    let gameToMetrics = new Dictionary<_, _>()
    let logger = logger log gameToMetrics

    interface ISearcher with 
        member this.MaxTime = TimeSpan.Zero
        member this.ResetMaxReward() = ()

        member this.Init root = 
            pastGames.Clear()
            pastGames.[root.GameHashCode] <-Set.empty
            gameToMetrics.Clear()

        member this.GetProgress() = []
        member ths.BrainServer = None

        member this.GetMetrics(node: MutableNode<'a, 'b>) = 
            getMetrics gameToMetrics node

        member this.PrintTree maxDepth rootNode = 
            printTreeWithDepthAndWidth 3 maxDepth gameToMetrics rootNode

        member this.Search totalCount maxLookAhead root = 
            let getMovesRandom game = 
                let moves = GameMover.validMoves game
                
                moves |> List.map (fun x ->
                    
                    let g, isTerminal = 
                        match GameMover.playMove x game with 
                        | Game.Continue (g,_) -> g, None
                        | Game.Lost g -> g, Some false
                        | Game.Won g -> g, Some true
                    
                    g, isTerminal, x, Math.Max(reward game * 2., 1.0)) 
                        
            let rolloutRandom (game, gameHashCode, depth) =
                async {
                    do! Async.SwitchToThreadPool()
                    let pastGames = pastGames.[gameHashCode]
                    return rolloutRandom pastGames (Set.count pastGames |> float) depth game
                }
                
            let expandRandom node =
                expandNode getMovesRandom pastGames node //|> List.distinctBy (fun x -> reward x.Game)
            
            let rec reSearch count = 
                let maxDepth = 100.
                logger count totalCount root -1. maxDepth
                if count > 0 then 
                    iteration (fun root -> logger count totalCount root -1. maxDepth) maxDepth 0. expandRandom rolloutRandom gameToMetrics root |> ignore
                    reSearch (count - 1)
            reSearch totalCount
            false

type SearcherWithNeuralNetwork(brainsMover: IBrainsMover, log, maxTime) =
    let mutable winningNode = None
    let mutable progress: float list = []
    let pastGames = Dictionary<_, _>()
    let gameToMetrics = new Dictionary<_, _>()
    let logger = logger log gameToMetrics
    let mutable maxReward = -10.
    let watch = Stopwatch()

    interface ISearcher with 

        member this.Init root = 
            pastGames.Clear()
            pastGames.[root.GameHashCode] <- Set.empty
            winningNode <- None
            progress <- []
            gameToMetrics.Clear()
            maxReward <- -10.
            watch.Restart()

        member this.MaxTime = maxTime

        member this.ResetMaxReward() = 
            maxReward <- 0.

        member this.GetProgress() = []
        member ths.BrainServer = None

        member this.GetMetrics(node: MutableNode<'a, 'b>) = 
            getMetrics gameToMetrics node

        member this.PrintTree maxDepth rootNode = 
            printTreeWithDepth maxDepth gameToMetrics rootNode

        member this.Search totalCount maxLookAhead root = 

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
                    let bestMoves = 
                        brainsMover.GetBestMove game
                        |> Async.RunSynchronously
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
            let increment = (maxLookAhead - startValue) / float totalCount

            if maxReward = 0. then 
                maxReward <- findBestScore (getMetrics gameToMetrics) root

            let rec reSearch maxDepth count = 
                logger count totalCount root maxReward maxDepth                
                if count > 0 && watch.Elapsed < maxTime then
    //                printTreeWithDepth 1 gameToMetrics root
                    let reward = iteration (fun root -> logger count totalCount root maxReward maxDepth) maxDepth 0. expand rollout gameToMetrics root

                    if reward > maxReward then 
                        maxReward <- reward
                    
                    match maxReward = winningNodeReward with
                    | false ->
                        let maxDepth = 
                            if maxDepth = maxLookAhead then  startValue
                            else Math.Min(maxDepth + increment, maxLookAhead)
                        reSearch maxDepth (count - 1)
                        
                    | true ->
                        printfn "Found WINING node!"
                        true
                else false

            reSearch startValue totalCount
