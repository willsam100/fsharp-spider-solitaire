module SpiderSolitare.MonteCarloTreeSearch

open SpiderSolitare.Representation
open System
open System
open SpiderSolitare.Game
open Microsoft.ML
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
    
let timed name f = 
    let s = Stopwatch()
    s.Restart()
    let x = f ()
    printfn "Timed %s: %A" name s.Elapsed
    x

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


type MutableNode(game:Game, prob: float, reward: float, move: MoveType option, depth: int, parent, terminalValue) = 
    // member val N = 0. with get,set
    // member val T = 0. with get, set
    member val Game = game
    member val GameHashCode = game.GetHashCode()
    member val Prob = prob
    member val Reward = reward with get, set
    member val Move = move
    member val Depth = depth
    member val Children: MutableNode list = [] with get,set
    member val Parent: MutableNode option = parent with get, set
    member val TerminalValue: bool option = terminalValue with get, set

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
    |> List.map (fun (x, y, _) -> x,y)


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
    | None -> 
        if reward game |> int = 1 then 1. else 0.
    | Some move -> 
        match Game.GameMover.playMove move game with 
        | Game.Continue (game,s) ->  
            if count <= 0 then 
                reward game * Math.Pow(gamma, depth)
            else       
                let pastGames = Set.add (game.GetHashCode()) pastGames
                rolloutRandom pastGames (depth + 1.) (count - 1) game
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

type ExpandOperation =
    | AddLeaf of MutableNode
    | NoAdd // Leaf is in the tree on the shortest path
    | Remove of int * int list * int list
    
type ExOp =
    | AddLeaf of MutableNode
    | LinkSeenNode of (int * int list * int list)
    
let createMutableNode (parentNode: MutableNode) move probabiltiy g terminalValue =
    MutableNode(g, probabiltiy, 0., Some move, parentNode.Depth + 1, Some parentNode, terminalValue)

let expandNode getMoves (pastGames: IDictionary<int, int Set>) (node: MutableNode) = 
    let moves = getMoves node.Game
    let parents = pastGames.[node.GameHashCode]
    
    if (node.Children |> List.length > 0) || node.TerminalValue |> Option.isSome then
        printfn "N:%d" node.GameHashCode
        printfn "R:%f" node.Reward
        printfn "T:%A" node.TerminalValue
        failwith "Tree is invalid - we should not be expanding."
    
    let shouldAddNodeToTreee (nextGame: MutableNode) =
        match pastGames.TryGetValue nextGame.GameHashCode with
        | false, _ -> Some nextGame
        | true, existingParents ->
            
            if parents |> Set.contains nextGame.GameHashCode || existingParents |> Set.contains nextGame.GameHashCode then
//                printfn "Resetting to tree!"
//                printfn "%d" nextGame.GameHashCode
//                nextGame.TerminalValue <- Some false
                None
            else Some nextGame
                
//                let newLength = parents.Count + 1
//                if existingParents.Count <= newLength then
//                    NoAdd
//                else
//            let parents = parents |> seq |> Seq.toList // Take a copy of the parents
//            let shortestPath = node.GameHashCode :: parents // the parent of the new node needs to be added.
//            LinkSeenNode (nextGame.GameHashCode, shortestPath, existingParents |> seq |> Seq.toList)

    let updatePathToParents g = 
        let gHashCode = g.GetHashCode()
        match pastGames.TryGetValue gHashCode with
        | true, existingParents ->
            
            if parents |> Set.contains gHashCode || existingParents |> Set.contains gHashCode then
//                printfn "Skipping duplicate in Tree."
                ()
            else 
//            // We must add one to account for the parent node.
//            // The existingParents has already had this applied
//            let newLength = parents.Count + 1
//            if existingParents.Count > newLength then
//                let parents = parents |> seq |> HashSet
//                parents.Add node.GameHashCode |> ignore
//            let eParents = existingParents |> seq |> Seq.toList
//            let p = parents |> seq |> Seq.toList
                pastGames.[gHashCode] <- Set.union existingParents parents |> Set.add node.GameHashCode //HashSet(eParents @ p)

        | false, _ ->
//            pastGames.[gHashCode] <- HashSet(parents)
//            pastGames.[gHashCode].Add node.GameHashCode |> ignore
            pastGames.[gHashCode] <- Set.add node.GameHashCode parents
            
    moves 
    |> List.choose (fun (move, prob) ->       
        let g, terminalValue = 
            match Game.GameMover.playMove move node.Game with 
            | Game.Continue (g,_) -> g, None
            | Game.Lost g -> g, Some false
            | Game.Won g -> g, Some true

        let expandOperation =
            terminalValue
            |> createMutableNode node move prob g
            |> shouldAddNodeToTreee
            
        updatePathToParents g 
        expandOperation
    )

let setMetrics (gameToMetrics:IDictionary<int, float * float>) (hc: int) value = 
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


let setParentsValues (gameToMetrics: IDictionary<int, float * float>) (nodes, value) =  
    nodes |> Set.iter (fun p -> setMetrics gameToMetrics p value)

let printTree (node:MutableNode) =
    let rec loop depth (node: MutableNode) =
        let s = List.replicate depth "__" |> String.concat ""
        printfn "%s%d" s node.GameHashCode
        node.Children |> List.iter (loop (depth + 1)) 

    loop 0 node
    
let asserter (node:MutableNode) =
    let rec loop (node: MutableNode) = 
        match node.Children with 
        | [] -> ()
        | children -> 
        
            let g = node.Game
            let validMoves = GameMover.validMoves g
            
            let nextStates = 
                validMoves
                |> List.map (fun x ->
                    match GameMover.playMove x g with
                    | Continue (g', _) -> x, g'
                    | Lost g' -> x,g'
                    | Won g' -> x, g' )
                
            children
            |> List.forall (fun x ->
                nextStates |> List.contains (x.Move.Value, x.Game)
                )
            |> (fun x ->
                if not x then
                    failwith "We have issues"
            )
            
            children |> List.iter loop
    loop node
    
    
let backWalk (node: MutableNode) =
    let rec loop acc (node: MutableNode) =
        match node.Parent with
        | None -> acc
        | Some x -> loop (x :: acc) x
    loop [] node
    
    
let inline toString xs =
    xs |> List.map string |> String.concat "," |> printfn "%s"
    
let consistent (pastGames: IDictionary<int, HashSet<_>>) (node: MutableNode) =
//    printfn "Exploring: %d" node.GameHashCode
    let rec loop (node: MutableNode) =
        let parents = pastGames.[node.GameHashCode] |> seq |> Set.ofSeq
        let treeParents = backWalk node |> List.map (fun x -> x.GameHashCode) |> Set.ofList
        
        let diff = Set.difference parents treeParents
        if diff.Count <> 0 then
            parents |> Set.toList |> toString
            treeParents |> Set.toList |> toString
            diff |> Set.toList |> List.iter (printfn "Diff: %d")
            failwith "F**&^K"
            
        node.Children |> List.iter loop
    loop node
    
let updateLongestPath gHashCode path (nRoot: MutableNode) =

    let filterNode (node: MutableNode) =
        node.Children <- node.Children |> List.filter (fun x -> x.GameHashCode <> gHashCode)
        
    let findNode (node: MutableNode) =
        node.Children |> List.tryFind (fun x -> x.GameHashCode = gHashCode)
    
    let rec loop path (node: MutableNode) =
        let n = node.Children |> List.tryFind (fun x -> path |> List.contains x.GameHashCode)
        match n, path with
        | None, nodes when List.length nodes = 1 ->
            match node.Parent |> Option.bind findNode with
            | Some nodeBeingMoved ->
                filterNode node
                nodeBeingMoved
            | None ->
                match findNode node with
                | Some nodeBeingMoved ->
                    filterNode node
                    nodeBeingMoved
                | None ->
                    printTree nRoot
                    failwith "Why end here?"       
                
        | None, _ ->
            match findNode node with
            | Some nodeBeingMoved ->
                filterNode node
                nodeBeingMoved
            | None ->
                printTree nRoot
                failwith "Why here?"
    
        | Some node, _ -> 
            let nodes = path |> List.filter (fun x -> x <> node.GameHashCode)
            loop nodes node

    if path |> List.contains nRoot.GameHashCode then
        let path = path |> List.filter (fun x -> x <> nRoot.GameHashCode)
        loop path nRoot
    else
        failwith "Why stop Here?"
        
let findNode gHashCode path (nRoot: MutableNode) =
        
    let findNode (node: MutableNode) =
        node.Children |> List.tryFind (fun x -> x.GameHashCode = gHashCode)
    
    let rec loop path (node: MutableNode) =
        let n = node.Children |> List.tryFind (fun x -> path |> List.contains x.GameHashCode)
        match n, path with
        | None, nodes when List.length nodes = 1 ->
            match node.Parent |> Option.bind findNode with
            | Some nodeBeingMoved ->
                nodeBeingMoved
            | None ->
                match findNode node with
                | Some nodeBeingMoved ->
                    nodeBeingMoved
                | None ->
                    printTree nRoot
                    failwith "Why end here?"       
                
        | None, _ ->
            match findNode node with
            | Some nodeBeingMoved ->
                nodeBeingMoved
            | None ->
                printTree nRoot
                failwith "Why here?"
    
        | Some node, _ -> 
            let nodes = path |> List.filter (fun x -> x <> node.GameHashCode)
            loop nodes node

    if path |> List.contains nRoot.GameHashCode then
        let path = path |> List.filter (fun x -> x <> nRoot.GameHashCode)
        loop path nRoot
    else
        printfn "%d" gHashCode
        printTree nRoot
        failwith "Why stop Here?"

let rec updateShortestPath shortest (nodeToAdd: MutableNode) (node: MutableNode) =
//    printfn "Add:%d" nodeToAdd.GameHashCode
//    printfn "%A" shortest
//    printfn "node:%d" node.GameHashCode
    
    let addChild (node: MutableNode) =
        node.Children <- nodeToAdd :: node.Children |> List.distinctBy (fun x -> x.GameHashCode)
    
    match node.Children with
    | [] -> addChild node
    | _ -> 
        match node.Children |> List.tryFind (fun x -> shortest |> List.contains x.GameHashCode) with
        | None -> addChild node
        | Some node ->
            updateShortestPath shortest nodeToAdd node
        
    
//let appendNodes nRoot leaves longPathRemoveOp =
//
//    printfn "Duplicate count: %d" (List.length longPathRemoveOp)
//    
//    longPathRemoveOp
//    |> List.iter (fun (gHashCode, shortest, longest) ->
//        
//        printfn "DupGCode: %d" gHashCode
//        printfn "Sh:%A" shortest
//        printfn "Ln:%A" longest
//
//        let nodeToAdd = updateLongestPath gHashCode longest nRoot
//        updateShortestPath shortest nodeToAdd nRoot
//    )
//    
//    let duplicateGameCodes = longPathRemoveOp |> List.map (fun (g: int,_,_) -> g)
//    
//    leaves |> List.filter (fun (x: MutableNode) ->
//            duplicateGameCodes |> List.contains x.GameHashCode |> not 
//        )
    

let rec iteration expandNode rollout (pastGames: IDictionary<int, int Set>) gameToMetrics nRoot (node: MutableNode) = 
    
    let metrics = 
        node.Children 
        |> List.map (fun x -> 
            let (t,n) = getMetrics gameToMetrics x 
            x,t,n)
    
    match nextMove gameToMetrics metrics with 
    | [] -> 
        
        match node.TerminalValue with 
        | Some result ->
            let (t,n) = getMetrics gameToMetrics node
            
            let nodes = pastGames.[node.GameHashCode] |> Set.add node.GameHashCode 
            timed "\nTERMINAL:" <| fun () -> setParentsValues gameToMetrics (nodes, node.Reward)
//            printfn "RESULT: %b - %d" result node.GameHashCode
            
            let (t',n') = getMetrics gameToMetrics node
            
            printfn ""
            printfn "%f - %f" n n'
            printfn "%f - %f" t t'
            if n' = n then
                printTree nRoot
                printfn "%A" <| Set.toList nodes
                printfn "%f" node.Reward
                failwithf "Why are we not updating"

        | None ->
//            printTree nRoot
//            printfn "Expanding.. node:%d" node.GameHashCode
            let leaves = timed "expand-non-term" <| fun () -> expandNode pastGames node
            
//            printfn "Expanding.. count:%d" <| List.length leaves
            
//            let leaves =
//                expandOps
//                |> List.fold (fun acc x ->
//                    match x with
//                    | AddLeaf x -> x :: acc
//                    | _ -> acc ) []
                
//            let duplicates, leaves = timed "expand-non-term" <| fun () -> expandNode pastGames node
            match leaves with 
            | [] ->
                // This is considered a terminal state (the game is trying to repeat a state). C
                // Consider playing this move a loss as it would only cause a loop.
                let value = 0.
                let nodes = pastGames.[node.GameHashCode] |> Set.add node.GameHashCode
                timed "updateMetrics0" <| fun () ->  setParentsValues gameToMetrics (nodes, value)
                
//                failwithf "This is odd - I'm not sure how we got here: %d" node.GameHashCode
                // printfn "Set parents for terminal 0" 
            | _ ->
                
//                expandOps
//                |> List.fold (fun acc x ->
//                    match x with    
//                    | LinkSeenNode (hash, newP, exisitngP) -> (hash, newP, exisitngP) :: acc
//                    | _ -> acc ) []
//                |> List.iter (fun (hash, newP, exisitngP) ->
//                    printfn "Finding node: %d" hash
////                    printfn "NewP: %A" newP
////                    printfn "EP: %A" exisitngP
//                    let linkNode = findNode hash exisitngP nRoot
//                    printfn "Found node:%d" linkNode.GameHashCode
//                    printTree nRoot
//                    if linkNode.GameHashCode = 1025371856 then
//                        printfn "%A" exisitngP
//                        printfn "%A" newP
//                        printfn "%A" (linkNode.Children |> List.map (fun (x: MutableNode) -> x.GameHashCode))
//                        failwith "Stopping here!"
//                    node.Children <- node.Children |> List.filter (fun x -> newP |> List.contains x.GameHashCode)
//                    
//                    updateShortestPath newP linkNode nRoot
//                    printfn "node is linked"
//                )
//
////                let leaves = appendNodes nRoot leaves longPathRemoveOp
//                printfn "appending leaves: %d" leaves.Length
                node.Children <- leaves
                
//                if List.isEmpty node.Children then
//                    printfn "Starting from the top"
//                    iteration expandNode rollout pastGames gameToMetrics nRoot nRoot
//                else 
                iteration expandNode rollout pastGames gameToMetrics nRoot node

    | (node,n)::_ -> 
        if n = 0. then  
            let value = rollout (node, 50)
            let nodes = pastGames.[node.GameHashCode] |> Set.add node.GameHashCode
            setParentsValues gameToMetrics (nodes, value)
            let (t,n) = getMetrics gameToMetrics node
            if n = 0. then
                printTree nRoot
                printfn "%A" (nodes)
                failwithf "We did not update the node: %d" node.GameHashCode
            
        else
            iteration expandNode rollout pastGames gameToMetrics nRoot node

let getDepth node = 
    let rec loop depth (n: MutableNode) = 
        match n.Parent with 
        | None -> depth 
        | Some x -> loop (depth + 1) x

    loop 0 node |> float

let search log (brainsMover: IBransMover) (pastGames: IDictionary<int, int Set>) gameToMetrics totalCount (root: MutableNode) =  
    
    let rec reSearch count = 
        // printfn "research"
        let (t,n) = getMetrics gameToMetrics root
//        printfn "research:%d" count
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


            // let expand = expandNode brainsMover.GetBestMove

            let getMoves game = 
                let moves = GameMover.validMoves game 
                let bestMoves = 
                    brainsMover.GetBestMove game 
                    |> List.filter (fun (move,_) -> moves |> List.contains move)

                if moves.Length <> bestMoves.Length then    
                    Set.difference (Set.ofList moves) (bestMoves |> List.map fst |> Set.ofList) |> printfn "%A"
                    printfn "We have been missing a move in the neural network"

                bestMoves

            let expand = expandNode getMoves
            let rollout (node: MutableNode, depthSearch) = 
                let depth = pastGames.[node.GameHashCode].Count |> float

//                let depthLimit =    
//                    let depthLimit = 100
//                    match game.Spades with 
//                    | One -> depthLimit * 2
//                    | Two -> depthLimit * 3
//                    | Three -> depthLimit * 4
//                    | _ -> depthLimit * 5

                // if depth > float depthLimit then 0.
                // else 

                match Game.isComplete node.Game with 
                | true ->
                        if GameMover.validMoves node.Game |> List.isEmpty then
                            0.
                        else 
                            let r = Math.Pow(0.9, depth) * 1.
                            node.Reward <- r
                            r
                        
                | false -> 
                    let r = Math.Pow(0.9, depth) * brainsMover.GetValue node.Game  // recent rewards are better for the optimal policy
                    node.Reward <- r
                    r

            // let rollout (node, depth) game = 
            //     let pastGames = pastGames.[game.GetHashCode()]
            //     rolloutRandom pastGames (Set.count pastGames |> float) depth game


            timed "iteration" <| fun () -> iteration expand rollout pastGames gameToMetrics root root
            reSearch (count - 1)

    let (t,n) = getMetrics gameToMetrics root
    // if log then 
    if t / n > 0.80 && n > 100. then 
        () 
    else 
        if t > 100. && n > 2000. then 
            () 
        else 
            reSearch totalCount



