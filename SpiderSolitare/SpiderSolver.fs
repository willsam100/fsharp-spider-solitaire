namespace SpiderSolitare.Solver
open SpiderSolitare.Game
open System
open System.Diagnostics
open System.Threading.Tasks
open Logary
open FSharp.Collections.ParallelSeq
open SpiderSolitare.Operations

[<AutoOpen>]
module Console = 

    let printfnWithColor color (text:string) =
        let orig = Console.ForegroundColor
        Console.ForegroundColor <- color
        Console.WriteLine text
        Console.ForegroundColor <- orig

type Tree<'LeafData,'INodeData> =
    | LeafNode of 'LeafData
    | InternalNode of 'INodeData * Tree<'LeafData,'INodeData> list

module Tree =
    let rec cata fLeaf fNode (tree:Tree<'LeafData,'INodeData>) :'r = 
        let recurse = cata fLeaf fNode  
        match tree with
        | LeafNode leafInfo -> 
            fLeaf leafInfo 
        | InternalNode (nodeInfo,subtrees) -> 
            if List.isEmpty subtrees then [] 
            else subtrees |> List.map recurse
            |> fNode nodeInfo

    let rec fold fLeaf fNode acc (tree:Tree<'LeafData,'INodeData>) :'r = 
        let recurse = fold fLeaf fNode  
        match tree with
        | LeafNode leafInfo -> 
            fLeaf acc leafInfo 
        | InternalNode (nodeInfo,subtrees) -> 
            let localAccum = fNode acc nodeInfo
            if List.isEmpty subtrees then localAccum else
                let finalAccum = Seq.fold recurse localAccum subtrees
                finalAccum 
        
    let rec map fLeaf fNode (tree:Tree<'LeafData,'INodeData>) = 
        let recurse = map fLeaf fNode  
        match tree with
        | LeafNode leafInfo -> 
            let newLeafInfo = fLeaf leafInfo
            LeafNode newLeafInfo 
        | InternalNode (nodeInfo,subtrees) -> 
            let newNodeInfo = fNode nodeInfo
            let newSubtrees = 
                if List.isEmpty subtrees 
                then [] 
                else subtrees |> List.map recurse 
            InternalNode (newNodeInfo, newSubtrees)

    let rec foldFast fLeaf fNode (acc: 'r) (tree:Tree<'LeafData,'INodeData>): 'r = 
        let recurse = foldFast fLeaf fNode  
        match tree with
        | LeafNode leafInfo -> 
            fLeaf acc leafInfo
        | InternalNode (nodeInfo,subtrees) -> 
            let (localAccum, searchCompleted) = fNode acc nodeInfo
            if searchCompleted then localAccum else
                let finalAccum = subtrees |> List.fold recurse localAccum 
                finalAccum 

    let rec foldFastWithChildren fLeaf (fNode: 'r -> 'INodeData -> Tree<'LeafData,'INodeData> list -> ('r * bool)) acc (tree:Tree<'LeafData,'INodeData>): 'r = 
        let recurse = foldFastWithChildren fLeaf fNode  
        match tree with
        | LeafNode leafInfo -> 
            fLeaf acc leafInfo
        | InternalNode (nodeInfo,subtrees) -> 
            let (localAccum, searchCompleted) = fNode acc nodeInfo subtrees
            if searchCompleted then localAccum 
            else
                subtrees |> List.fold recurse localAccum 

type InternalGame = {
    MoveType: MoveType Option
    Heuristic: int
    Game: Game
    Moves: MoveType list
}

type MovePlayedGame = {
    Heuristic: int
    Move: MoveType
    Game: Game
    Moves: MoveType list
}

type SpiderTree = Tree<MovePlayedGame,InternalGame>
type SpiderDepthTree = Tree<(int * MovePlayedGame), (int * InternalGame)>
module SpiderTree = 

    let playeMovedToInternalGame playedMove = 
       {MoveType = Some playedMove.Move; Game = playedMove.Game; Moves = playedMove.Moves; Heuristic = playedMove.Heuristic}


    let createTree game moves h : SpiderTree = 
        InternalNode ({MoveType = None; Game = game; Moves = moves; Heuristic = h}, [])

    let appendLeaf node trees playedMove = 
        InternalNode (node, (LeafNode playedMove) :: trees)

    let appendTree node trees tree = 
        InternalNode (node, tree :: trees)

    let replaceTrees node trees = 
        InternalNode (node, trees)

    let appendTreeAtLeaf leaf tree = 
        InternalNode (playeMovedToInternalGame leaf, [tree])

    let createNodeFromLeaf leaf playedMove: SpiderTree = 
        InternalNode (playeMovedToInternalGame leaf, [(LeafNode playedMove)])

    let getTrees = function
    | LeafNode _ -> []
    | InternalNode (node, trees) -> trees

    let getRoot f g = function 
    | LeafNode leaf -> f leaf
    | InternalNode (node:InternalGame, _) -> g node

    let getGame = 
        getRoot (fun leaf -> leaf.Game) (fun node -> node.Game)

    let getRootMove = 
        getRoot (fun leaf -> Some leaf.Move) (fun node -> node.MoveType)

    let getHeuristic = 
        getRoot (fun leaf -> leaf.Heuristic) (fun node -> node.Heuristic)

    let depth tree = 
        let fLeaf a = 1

        let fNode a leaves = 
            if List.isEmpty leaves then 1 else 
            let leaves = List.toArray leaves
            leaves |> Array.max |> (fun x -> x + 1)

        Tree.cata fLeaf fNode tree

    let toList tree = 
        let fLeaf acc (leaf : MovePlayedGame) = leaf.Game :: acc
        let fNode acc (node: InternalGame) = node.Game :: acc 
        Tree.fold fLeaf fNode [] tree

    let addDepthDescending (tree: SpiderTree): SpiderDepthTree = 

        let fLeaf leaf = 
            LeafNode (0, leaf)

        let fNode node leaves = 
            let level = 
                if List.isEmpty leaves 
                then 0 
                else leaves |> List.map (depth) |> List.max 
            InternalNode ((level + 1, node), leaves)

        Tree.cata fLeaf fNode tree

    let depthAscending tree =
        let max = depth tree
        let fLeaf (depth, leaf) = (max - depth, leaf)      
        let fNode (depth, node) = (max - depth, node)
        Tree.map fLeaf fNode tree

    let findBestScore tree = 
        let fLeaf max leaf = Math.Max (max, leaf.Heuristic)
        let fNode max (node: InternalGame) = max
        Tree.fold fLeaf fNode Core.int.MinValue tree

    let totalCount (tree: SpiderTree) = 
        let fLeaf total _ = total + 1 
        let fNode total _ = total + 1

        Tree.fold fLeaf fNode 0 tree

    let canAddWithHeuristic playedMove targetGame tree = 

        let fLeaf acc leaf = 
            leaf.Heuristic > playedMove.Heuristic |> not

        let fNode acc (node: InternalGame) = 
            if acc = false then false, true 
            elif node.Heuristic > playedMove.Heuristic then false, true
            else acc, false

        Tree.foldFast fLeaf fNode true tree

    let containsGame game tree = 

        let fLeaf acc leaf = 
            leaf.Game = game

        let fNode acc (node: InternalGame) = 
            if acc then acc, true 
            else node.Game = game, false

        Tree.foldFast fLeaf fNode false tree

    let addMovePlayedNonGreedy targetGame playedMove tree = 
        if containsGame playedMove.Game tree || 
            not <| canAddWithHeuristic playedMove targetGame tree then tree else 

        let fLeaf leaf: SpiderTree = 
            if (leaf.Game = targetGame) then createNodeFromLeaf leaf playedMove
            else LeafNode leaf

        let fNode (node: InternalGame) trees = 
            if node.Game = targetGame then 
                appendLeaf node trees playedMove
            else InternalNode (node, trees)

        Tree.cata fLeaf fNode tree 

    let addMovePlayedGreedy targetGame playedMove tree = 
        //if containsGame playedMove.Game tree then tree else 

        let fLeaf leaf: SpiderTree = 
            if (leaf.Game = targetGame) then createNodeFromLeaf leaf playedMove
            else LeafNode leaf

        let fNode (node: InternalGame) trees = 
            if node.Game = targetGame then 
                appendLeaf node trees playedMove
            else InternalNode (node, trees)

        Tree.cata fLeaf fNode tree 

    let addTreeAtGame newTree tree = 

        let targetGame = getGame newTree
        let trees = getTrees newTree

        let fLeaf leaf: SpiderTree = 
            if (leaf.Game = targetGame) 
            then replaceTrees (playeMovedToInternalGame leaf) trees
            else LeafNode leaf

        let fNode (node: InternalGame) tr = 
            if node.Game = targetGame 
            then replaceTrees node trees
            else InternalNode (node, tr)

        Tree.cata fLeaf fNode tree 

    let increaseBaseHeuristic amountToIncrease tree = 

        let fLeaf (leaf: MovePlayedGame) = 
            {leaf with Heuristic = leaf.Heuristic + amountToIncrease}

        let fNode (node: InternalGame) = 
            {node with Heuristic = node.Heuristic + amountToIncrease}

        Tree.map fLeaf fNode tree 

    let getNode game tree = 

        let fLeaf acc (leaf: MovePlayedGame) = 
            acc

        let fNode acc (node: InternalGame) trees = 
            match acc with 
            | Some x -> Some x, true
            | None -> 
                if node.Game = game then 
                    Some <| InternalNode (node,trees), true
                else 
                    acc, false

        Tree.foldFastWithChildren fLeaf fNode None tree 


    let toString tree = 
        let prefix depth = String.replicate depth "  "

        let printGame header prefix game = 
            header @ App.toStringGame game |> List.map (sprintf "%s%s" prefix) |> String.concat "\n"

        let fLeaf acc (depth, leaf) = 
            let header =  [
                "Leaf"
                App.printMove leaf.Move
                string leaf.Heuristic  ]
            acc + (printGame header (prefix depth) leaf.Game) + "\n"

        let fNode acc (depth, node: InternalGame) =
            let prefix = prefix depth 
            let header = [
                "Node..."
                string node.Heuristic
                sprintf "%A" <| Option.map App.printMove node.MoveType ]
            acc + (printGame header prefix node.Game + "\n")

        Tree.fold fLeaf fNode "" (tree |> addDepthDescending |> depthAscending)

    let printTree tree = 

        printfn "Printing tree.."
        tree |> toString |> printfn "%s"
        printfn "Printing complete"


type Heuristic = Game -> int
type Depth = int
type SearchData = {
   Heuristics: (Heuristic * Depth) list
   TimeLimit: Stopwatch
}

//type GameMoves = {
//    Game: Game
//    Moves: MoveType list 
//}

module SearchData = 

    let getHeuristic searchData = 
        searchData.Heuristics |> List.head |> fst

    let getDepth searchData = 
        searchData.Heuristics |> List.head |> snd

    let startTimer searchData = 
        {searchData with TimeLimit = Stopwatch.StartNew()}

    let exceededBottom searchData = 
        searchData |> getDepth <= 1 || List.length searchData.Heuristics = 0
        //timeLimit.ElapsedMilliseconds > 8L * 1000L

    let atBottom searchData = 
        searchData |> getDepth = 1 && List.length searchData.Heuristics > 1

    let nextHeuristic searchData = 
        {searchData with Heuristics = List.tail searchData.Heuristics}

    let reduceFirstHeuristicDepth searchData =  
        let h = getHeuristic searchData
        let depth = getDepth searchData 
        let tail = List.tail searchData.Heuristics
        {searchData with Heuristics = (h, depth - 1) :: tail}

module Search = 
    type Result = GameLost | GameWon 
    type Status = Result of Result | MaxHeuristic of int



    let updateStates (knownStates) gameResults = 
        let folder acc game = 
            match game with 
            | Won -> acc
            | Lost game -> game :: acc
            | Continue (game, _) -> game :: acc

        gameResults |> List.fold folder [] |> Set.ofList |> Set.union knownStates 

    let filterGameResults knownGames gameResult = 
        match gameResult with 
        | Won -> false
        | Lost game -> false
        | Continue (game, _) -> Set.contains game knownGames |> not

    let rec playGamesWithMoves f knownStates = function 
    | [] -> (GameLost, knownStates)
    | (game, moves)::xs -> 
        match f knownStates game moves with 
        | GameWon, newStates -> (GameWon, Set.union newStates knownStates)
        | GameLost, newStates -> playGamesWithMoves f (Set.union newStates knownStates) xs


    let playMoves playMove heuristic knownStates moves game gameWon evaulateAndContinue tree memoGames =

        let wonGame = function | Won -> true | _ -> false

        let (|HasWon|_|) gameAndMoves =
            let games = gameAndMoves |> Seq.map snd
            match Seq.isEmpty games |> not &&  games |> Seq.map wonGame |> Seq.reduce (||) with 
            | true -> gameAndMoves |> Seq.tryFind (fun (m,g) ->  wonGame g) |> Option.map fst
            | false -> None

        let (|GamesAndMoves|_|) heuristic gameAndMoves =
            let games = gameAndMoves |> Seq.map snd
            match Seq.isEmpty games || games |> Seq.map wonGame |> Seq.reduce (||) |> not with 
            | true -> 
                gameAndMoves 
                |> GameOperations.toGameAndMoves 
                |> List.map (fun (move,(game,moves)) -> {Move = move; Game = game; Moves = moves; Heuristic = heuristic game})
                |> Some
            | false -> None

        let games = moves |> List.map (fun x -> x,(playMove game x))
        match games with 
        | HasWon move -> gameWon move
        | GamesAndMoves heuristic gamesAndMoves -> 
            let gamesAndMoves = gamesAndMoves |> List.filter (fun x -> Set.contains x.Game knownStates |> not)
            let knownStates = updateStates knownStates (List.map snd games)
            if Seq.isEmpty gamesAndMoves 
            then (tree, memoGames)
            else evaulateAndContinue (knownStates, gamesAndMoves)
        | _ -> (tree, memoGames)

    let addGamesToTree addPlayedMove games targetGame tree = 
        let addMove = addPlayedMove targetGame
        games |> List.fold (fun tree playedMove -> addMove playedMove tree) tree

    let shouldChangeHeuristic searchData game tree = 
        if SearchData.atBottom searchData then false else
        
        let trees = 
            SpiderTree.getNode game tree 
            |> Option.defaultValue (SpiderTree.createTree game [] 0)
            |> SpiderTree.getTrees 

        if List.isEmpty trees then false else
        let currentHeuristic = SearchData.getHeuristic searchData game

        trees
        |> List.map SpiderTree.getHeuristic
        |> List.forall (fun x -> x < currentHeuristic)


    let playNextHeuristic recurseWithHeuristic heuristic game moves tree = 
        let bestScore = SpiderTree.findBestScore tree
        let (nextHeuristicTree, memoGames) = recurseWithHeuristic (SpiderTree.createTree game moves (heuristic game)) game moves 
        //let nextHeuristicTree = nextHeuristicTree |> SpiderTree.increaseBaseHeuristic bestScore
        SpiderTree.addTreeAtGame nextHeuristicTree tree, memoGames

        // shouldChangeHeuristic heuristics heuristic
    let playNextMoves shouldChange recurseWithHeuristic heuristic game moves tree memoGames = 
        if shouldChange game tree then 
            playNextHeuristic recurseWithHeuristic heuristic game moves tree
         else tree, memoGames



    let rec rollout playMove searchData knownStates memoGames tree game moves =
        if SearchData.exceededBottom searchData || List.isEmpty moves then (tree, memoGames) else 
            let knownStates = tree |> SpiderTree.toList |> Set.ofList |> Set.union knownStates

            let handlePlayedMove tree (knownStates, gamesAndMoves) =

                let tree = tree |> addGamesToTree SpiderTree.addMovePlayedNonGreedy gamesAndMoves game
                let heuristicDepth = List.length searchData.Heuristics
                let memoGames = 
                    if Map.containsKey (game, heuristicDepth) memoGames then memoGames else 
                    Map.add (game, heuristicDepth) (tree, knownStates, gamesAndMoves) memoGames
                if SearchData.atBottom searchData then 
                    let recurse = rollout playMove (SearchData.nextHeuristic searchData) Set.empty memoGames
                    playNextHeuristic recurse (SearchData.getHeuristic searchData) game moves tree
                else 
                    let (tree, memoGames) = 
                        gamesAndMoves 
                        |> List.sortByDescending (fun x -> x.Heuristic) 
                        |> List.fold (fun (tree, memo) playedMove -> 
                            let recurse = rollout playMove (SearchData.reduceFirstHeuristicDepth searchData) knownStates memoGames
                            recurse tree playedMove.Game playedMove.Moves) (tree, memoGames)

                    //let memoGames = Map.add (game, List.length heuristics) (tree, knownStates, moves) memoGames
                    let recurse = rollout playMove (SearchData.nextHeuristic searchData) Set.empty memoGames
                    (playNextMoves (shouldChangeHeuristic searchData) recurse (SearchData.getHeuristic searchData) game moves tree) memoGames

            let heuristicKey = List.length searchData.Heuristics
            if Map.containsKey (game, heuristicKey) memoGames && false then 
                let (tree, knownStates, moves) = Map.find (game, heuristicKey) memoGames
                handlePlayedMove tree (knownStates, moves)
            else 
                playMoves playMove (SearchData.getHeuristic searchData) knownStates moves game (fun move -> tree, memoGames) (handlePlayedMove tree) tree memoGames
        
    let getBestMove tree = 
        tree |> SpiderTree.getTrees |> function
        | [] -> None
        | xs -> 
            xs
            |> List.map (fun x -> x, SpiderTree.findBestScore x)
            |> List.sortByDescending snd 
            |> List.tryHead
            |> Option.map fst 
            |> Option.bind SpiderTree.getRootMove

    let loop searchData gameResult memo = 
        let rec play knownStates currentH gameResult depth memo movesPlayed = 
            match gameResult with 
            | Lost game -> (GameLost, knownStates, None)
            | Won -> (GameLost, knownStates, None)
            | Continue (game, moves) -> 

                if Set.contains game knownStates 
                then (GameLost, knownStates, None)
                else 

                let knownStates = Set.add game knownStates 

                Game.toString game |> printfn "%s"
                printfn "Current Score: %d" currentH
                if depth = 0 then (GameLost, knownStates, None) else

                let tree = SpiderTree.createTree game moves currentH

                let searchData = SearchData.startTimer searchData
                let playMove = F.flip GameMover.playMove
                let runRollout = rollout playMove searchData knownStates memo tree 

                let tree, memo = runRollout game moves
                let bestMove =  getBestMove tree
                let h = SpiderTree.findBestScore tree
                   
                //SpiderTree.printTree tree

                printfn ""
                printfn "Max depth: %d" <| SpiderTree.depth tree
                printfn "Total Count: %d" <| SpiderTree.totalCount tree
                printfn "BestMove: %A" <| Option.map App.printMove bestMove
                printfn "BestScore: %d" h
                knownStates |> Set.count |> printfn "Unique game count: %d"
                printfn ""

                match bestMove with 
                | None -> (GameLost, knownStates, Some tree)
                | Some move ->
                    let h = SearchData.getHeuristic searchData
                    play knownStates (h game) (GameMover.playMove move game) (depth - 1) memo movesPlayed

        play Set.empty Core.int.MinValue gameResult Core.int.MaxValue memo Set.empty

    let getGamesAndMoves gameResult = 
        match gameResult with 
        | Lost game -> None
        | Won -> None
        | Continue x -> Some (x)

    type T = {
        G:Game
        G2:Game
        Moves:MoveType list
        Tree:SpiderTree 
        Tree2:SpiderTree 
        Tree3:SpiderTree 
        Tree4:SpiderTree 
        Tree5:SpiderTree 
    }

    let setup () = 
        let g = Game.emptyGame
        let g2 = {Game.emptyGame with Diamonds = One}
        let g3 = {Game.emptyGame with Diamonds = Two}
        let g4 = {Game.emptyGame with Diamonds = Two; Clubs = One}
        let g5 = {Game.emptyGame with Diamonds = Two; Clubs = Two}
        let moves = [Stock]
        let tree = SpiderTree.createTree g moves 0
        let tree2 = SpiderTree.addMovePlayedGreedy g {Move = Stock; Game = g2; Heuristic = 1; Moves = moves} tree
        let tree3 = SpiderTree.addMovePlayedGreedy g2 {Move = Stock; Game = g3; Heuristic = 1; Moves = moves} tree2
        let tree4 = SpiderTree.addMovePlayedGreedy g {Move = Stock; Game = g4; Heuristic = 1; Moves = moves} tree3
        let tree5 = SpiderTree.addMovePlayedGreedy g4 {Move = Stock; Game = g5; Heuristic = 1; Moves = moves} tree4

        { G = g; G2 = g2; Moves = moves; Tree = tree; Tree2 = tree2; Tree3 = tree3; Tree4 = tree4; Tree5 = tree5; }




module ScrathPad =
   
    let rec multistageNestedRollout inDeadEndOrWin argMax maxH result getA playedActions (hs: ((Game -> int) list)) (ns: int list) (s:Game): (int * MoveType list) = 
        let recurse = multistageNestedRollout inDeadEndOrWin argMax maxH result getA
        if inDeadEndOrWin s then List.head hs s, playedActions else 
            if List.head ns = -1 then 
                List.head hs s, playedActions
            else
                let recurse' = recurse [] hs (((List.head ns) - 1) :: (List.tail ns))
                let (maxHeuristicVal, a) = s |> getA |> maxH (fun a -> result s a |> recurse' |> fst) 
                let a = 
                    if (List.head hs s) > maxHeuristicVal && List.length hs > 1 then 
                        let recurse = recurse [] (List.tail hs) (List.tail ns)
                        s |> getA |> argMax (fun a -> result s a |> recurse |> fst) 
                    else a
                let playedActions = a :: playedActions
                result s a |> recurse playedActions hs ns


    type SearchResult = SearchWon | SearchLost | Searched of float

    let playheuristic hs s = 
        List.head hs s

    let levelNMinusOne ns = 
        (List.head ns) - 1 :: (List.tail ns)

    let hitRockBottom ns = 
        List.head ns = -1


    type Heurisitc = int
    type level = int
    type Message = 
        | Add of (Game * Heurisitc * level * SearchResult)
        | Get of (Game * Heurisitc * level * (SearchResult * Game) Option AsyncReplyChannel)
        | Size of int AsyncReplyChannel
        | Clear 

    module GameStateCache = 
        type Message = 
            | Add of (GameResult * MoveType)
            | Get of (MoveType * GameResult Option AsyncReplyChannel)
            | Size of int AsyncReplyChannel
            | Clear 

    let gameToMoveCache = 
        MailboxProcessor.Start(fun inbox ->
            let rec loop cache = 
                async {
                    let! msg = inbox.Receive()
                    match msg with 
                    | GameStateCache.Message.Add (g, m) -> 
                        let cache = Map.add m g cache
                        return! loop cache
                    | GameStateCache.Message.Get (m,rc) -> 
                        Map.tryFind (m) cache |> rc.Reply
                        return! loop cache
                    | GameStateCache.Message.Size rc -> 
                        Map.count cache |> rc.Reply
                        return! loop cache 
                    | GameStateCache.Message.Clear -> 
                        return! loop Map.empty }
            loop Map.empty)

    let gameCache = 
        MailboxProcessor.Start(fun inbox ->
            let rec loop cache = 
                async {
                    let! msg = inbox.Receive()
                    match msg with 
                    | Add (g,h,l,z) -> 
                        let cache = Map.add (g,h,l) (z, g) cache
                        return! loop cache
                    | Get (g,h,l,rc) -> 
                        Map.tryFind (g,h,l) cache |> rc.Reply
                        return! loop cache
                    | Size rc -> 
                        Map.count cache |> rc.Reply
                        return! loop cache 
                    | Clear -> 
                        return! loop Map.empty }
            loop Map.empty)

    module GlobalCache = 
        type GlobalCacheMessage = 
            | Add of (Heurisitc * level * Game)
            | Get of (Heurisitc * level * Game Set AsyncReplyChannel)
            | Size of int AsyncReplyChannel
            | Clear 

    let globalCache = 
        MailboxProcessor.Start(fun inbox ->
            let rec loop cache = 
                async {
                    let! msg = inbox.Receive()
                    match msg with 
                    | GlobalCache.Add (h,l,s) -> 
                        let history = Map.tryFind (h, l) cache |> Option.defaultValue Set.empty
                        let cache = Map.add (h,l) (Set.add s history) cache 
                        return! loop cache
                    | GlobalCache.Get (h,l,rc) -> 
                        Map.tryFind (h, l) cache |> Option.defaultValue Set.empty |> rc.Reply
                        return! loop cache
                    | GlobalCache.Size rc -> 
                        Map.count cache |> rc.Reply
                        return! loop cache 
                    | GlobalCache.Clear -> 
                        return! loop Map.empty }
            loop Map.empty)

    let printQueue =
        MailboxProcessor.Start(fun inbox ->
           let rec loop () = 
               async {
                   let! msg = inbox.Receive()
                   printfn "%s" msg
                   return! loop () }
           loop ())

    let log s = printQueue.Post s

    let logAtRoot isAtRoot ns m = 
        if isAtRoot ns then 
            sprintf "Depth: %d" (List.head ns) |> log
            m |> log

    let getFromCache s h n = 
        gameCache.PostAndReply (fun rc -> Get (s, h, n, rc))

    let addToCache s h n sr = 
        //if n > 0 then 
        (s,h,n,sr) |> Add |> gameCache.Post 

    let addToGlobalCache h n s =
        GlobalCache.Add (h,n,s) |> globalCache.Post 

    let getFromGlobalCache h n = 
        globalCache.PostAndReply (fun rc -> GlobalCache.Get (h,n,rc))

    let clear () = gameCache.Post Clear
    let clearGlobal () = globalCache.Post GlobalCache.Clear

    let isDeadEnd s a = 
        if List.isEmpty a then true else false

    let hasTimerExpired max n (timer: Stopwatch) =
        if n < max && timer.Elapsed > TimeSpan.FromSeconds 1. then true else false

    type T<'a> = {Result: 'a}

    let taskLong (f:unit -> 'T) = 
        { Result = f () }
        //let t = new Task<'T>(new Func<'T>(f), TaskCreationOptions.LongRunning)
        //t.Start()
        //t

    let maxH f actions = 

        let actions = List.toArray actions
        let nextActions = Array.Parallel.map (fun a -> a, f a) actions |> Array.toList

        //let handleNextActions nextActions = 
        let wonGames = nextActions |> List.filter (fun (a,h) -> h |> function | SearchWon -> true | _ -> false)
        let lostGames = nextActions |> List.filter (fun (a,h) -> h |> function | SearchLost -> true | _ -> false)
        let searchGames = 
            nextActions 
            |> List.fold (fun acc (a,h) -> 
                match h with 
                | SearchWon -> acc
                | SearchLost -> acc
                | Searched h -> (a, h) :: acc ) []
            |> List.sortByDescending (fun (a,h) -> h)
            |> List.map (fun (a,h) -> a, Searched h)

        wonGames @ searchGames @ lostGames

        //Task.Factory.ContinueWhenAll (nextActions, fun (tasks: Task<_>[]) -> 
            //tasks|> Array.map (fun x -> x.Result) |> Array.zip actions |> Array.toList |> handleNextActions )

    
    let playGameResult playedActions whenContinue s = 
        match s with
        | Won -> (SearchWon, playedActions)
        | Lost s -> (SearchLost, s)
        | Continue (s, ms) -> whenContinue s ms

    

    

    let rec multistageNestedRolloutFast 
                (hasTimeExpired: int -> Stopwatch -> bool) 
                isDeadEnd 
                getFromCache 
                addToCache
                filterLocalLoop
                maxH 
                moveOrdering
                result 
                isAtRoot
                (timer: Stopwatch) 
                playedActions 
            (hs: ((Game -> float) list)) (ns: int list) (s:GameResult) =
            let recurse = multistageNestedRolloutFast hasTimeExpired isDeadEnd getFromCache addToCache filterLocalLoop maxH moveOrdering result isAtRoot

            let filterPlayedMoves s ms = ms
                //if (List.length hs = 1 && List.head ns = 0) then 
                //    let playedMoves = getFromGlobalCache (List.length hs) (List.head ns) s
                //    ms |> List.filter (fun x -> Set.contains x playedMoves |> not)
                //else ms

            let isInLoop h n s = 
                getFromGlobalCache h n |> Set.contains s

            let logAtRoot = logAtRoot isAtRoot ns

            let rec innerRec s ms = 

                "\n----" |> logAtRoot
                Game.toString s |> logAtRoot
                ms |> App.printMoves |> String.concat ", " |> logAtRoot
                "----\n" |> logAtRoot

                let h = (fun () -> playheuristic hs s) |> taskLong |> (fun t -> t.Result)
                let ms = taskLong (fun () -> filterLocalLoop s ms |> filterPlayedMoves s |> moveOrdering s) |> (fun t -> t.Result)

                if List.isEmpty ms then
                    "At and in a loop :(" |> logAtRoot
                    addToCache s (List.length hs) (List.head ns) SearchLost 
                    (SearchLost, s)
                elif isInLoop (List.length hs) (List.head ns) s then 

                    "At and in a loop :(" |> logAtRoot
                    addToCache s (List.length hs) (List.head ns) SearchLost 
                    (SearchLost, s)
                elif isAtRoot ns |> not && hasTimeExpired (List.length hs) timer then 
                    let hApplied = taskLong (fun () -> playheuristic hs s |> Searched) |> (fun t -> t.Result)
                    (hApplied, s)
                else
                    //addToGlobalCache (List.length hs) (List.head ns) s

                    "\nFiltered moves:" |> logAtRoot
                    ms |> App.printMoves |> String.concat ", " |> logAtRoot
                    "---\n" |> logAtRoot

                //printfn "Cache Size = %d" (gameCache.PostAndReply Size)
                //sprintf "Global Size = %d" (globalCache.PostAndReply GlobalCache.Size) |> log

                //ms |> App.printMoves |> String.concat ", " |> printfn "%s"

                //ms |> App.printMoves |> String.concat ", " |> printfn "%s"

                //if isDeadEnd s ms || hasTimeExpired (List.head ns) timer then 
                //    let hApplied = playheuristic hs s |> Searched
                //    (hApplied, playedActions)
                //else

                    let timer = if isAtRoot ns then Stopwatch.StartNew() else timer

                    let recurse' = recurse timer s hs (levelNMinusOne ns)
                    let results = ms |> maxH (fun a -> result s a |> recurse' |> fst)
                    let (a, maxVal) = List.head results

                    if isAtRoot ns then 
                        "\nSearch Results" |> log
                        Game.toString s |> log
                        results |> List.iter (fun (m, sa) -> printfn "Move: %s, SearchResult: %A" (App.printMove m) sa)
                        sprintf "Selected move: %s" (App.printMove a) |> log
                        sprintf "Search Result: %A" maxVal |> log
                        "------\n" |> log

                    let nextHeuristic () = 
                        getFromCache s (List.length hs) (List.head ns) |> function 
                        | Some (z, cachedGame) -> 
                            if z = SearchLost then (SearchLost, s) 
                            else recurse timer s (List.tail hs) (List.tail ns) (Continue (s, ms))
                        | None -> 
                            //printfn "Next heuristic"
                            recurse timer s (List.tail hs) (List.tail ns) (Continue (s, ms))

                    match maxVal with 
                    | SearchWon -> (SearchWon, s)
                    | SearchLost -> if (List.length hs = 1) then (SearchLost, s) else nextHeuristic()
                    | Searched hNext ->
                        logAtRoot "\nPlaying search result"
                        sprintf "H: %f, v: %f H function: %d, depth: %d" h hNext (List.length hs) (List.head ns) |> logAtRoot
                        "---\n" |> logAtRoot

                        if hNext < h && (List.length hs > 1) then 
                            logAtRoot "Moving to next heuristic:"
                            addToCache s (List.length hs) (List.head ns) (Searched h) 
                            nextHeuristic()

                        //elif hNext < h then (Searched hNext, playedActions) // NonGreedy search
                        else
                            logAtRoot "Moving to next heuristic:"
                            //printfn "Playing move: %s" <| App.printMove a 
                            if (List.length hs = 1 && List.head ns = 0) then addToGlobalCache 1 0 s
                            result s a |> playGameResult s innerRec

            let playContinuedGame timer s ms = 
                //taskLong <| fun () -> 
                //if isAtRoot ns then 
                    //App.printableGame s |> log
                //elif List.length hs = 1 && (List.head ns = 2) then  
                    //App.printableGame s |> log

                //let ms = taskLong (fun () -> filterLocalLoop s ms |> moveOrdering s |> filterPlayedMoves s) |> (fun t -> t.Result)

                if isDeadEnd s ms then  //hasTimeExpired (List.head ns) timer || 
                    sprintf "bottom:  isDeadEnd" |> log
                    addToCache s (List.length hs) (List.head ns) SearchLost
                    (SearchLost, s)
                elif hitRockBottom ns || hasTimeExpired (List.length hs) timer then 
                    let hApplied = taskLong (fun () -> playheuristic hs s |> Searched) |> (fun t -> t.Result)
                    (hApplied, s)
                else
                    getFromCache s (List.length hs) (List.head ns) |> function 
                    | Some (z, cachedGame) -> 
                        if z = SearchLost then (SearchLost, s)   // z = 0 is in the white paper, it's unlcear what 0 means
                        elif (List.length hs > 1) then recurse timer s (List.tail hs) (List.tail ns) (Continue (s, ms))
                        else innerRec s ms
                    | None -> 
                        innerRec s ms

            playGameResult playedActions (playContinuedGame timer) s

    let playMove s a = 
        gameToMoveCache.PostAndReply (fun rc -> GameStateCache.Message.Get (a, rc)) |> function 
        | Some sNext -> sNext
        | None -> 
            let sNext = GameMover.playMove a s

            //match sNext with 
            //| Won -> ()
            //| Lost _ -> () 
            //| Continue (g,ms) -> 

                //"\n----------" |> log
                //"Orig Game:" |> log
                //App.printableGame s |> log
                //a |> App.printMove |> log
                //"New Game:" |> log
                //App.printableGame g |> log
                //ms |> App.printMoves |> String.concat ", " |> log
                //"__________\n" |> log

            gameToMoveCache.Post <| GameStateCache.Message.Add (sNext, a)
            sNext
             
        
    let runMultistageNestedRolloutFast hasTimerExpired = 
            multistageNestedRolloutFast hasTimerExpired isDeadEnd getFromCache 
                                        addToCache (GameOperations.filterLocalLoopMoves GameMover.playMove) maxH GameOperations.moveOrdering playMove 
    let raiseToPower pow = 
        (float >> F.flip (F.curry Math.Pow) pow >> int) 

    let hiddenCards tabs = 
        (54 - (tabs |> Array.map MLTab.hiddenLength |> Array.sum)) * 2 |> float

    let sizeOfHiddenCards tabs = 
        tabs 
        |> Array.map MLTab.hiddenLength
        |> Array.map (raiseToPower 3.) 
        |> Array.map (fun x -> x * -1) 
        |> Array.sum
        |> float


    let lengthOfRuns = 
        Array.map (Tableau.getRun >> List.length)
        >> Array.map (raiseToPower 3.) 
        >> Array.sum
        >> float

    let numberOfRuns = 
        Array.map (Tableau.getRun >> List.length) 
        >> Array.filter (fun x -> x > 1) 
        >> Array.map (raiseToPower 2.) 
        >> Array.sum
        >> float

    let faceUpCards tabs = 
        tabs 
        |> Array.map (Tableau.getVisible >> List.length)
        |> Array.sum
        |> float

    let numberOfStreaks =
        Array.map (MLTab.getStreak >> List.length) 
        >> Array.filter (fun x -> x > 1) 
        >> Array.map (fun x -> x * x)
        >> Array.sum
        >> float

    let emptyTab tabs = 
        tabs 
        |> Array.filter (fun x -> Tableau.length x = 0) 
        |> Array.map (fun x -> 1000)
        |> Array.sum
        |> float

    let numberOfSuitsInTab tabs = 
        tabs 
        |> Array.map (Tableau.getVisible >> List.map Card.getSuit >> List.groupBy id >> List.length) 
        |> Array.map (raiseToPower 2.) 
        |> Array.map (fun x -> x * -1) 
        |> Array.sum
        |> float

    let randomScore tabs = 
        tabs
        |> Array.mapi (fun i t -> t |> Tableau.getVisible |> List.map (fun x -> (Card.getValue x) * i) |> List.sum)
        |> Array.sum
        |> (fun x -> x |> float |> Math.Log)

    let scoreSuit = function 
    | Zero -> 0
    | One -> (13 * 13) * 40
    | Two -> (13 * 13) * 50
    | Three -> (13 * 13) * 60
    | Four -> (13 * 13) * 70

    let suitCompletion game = 
        [game.Hearts; game.Spades; game.Clubs; game.Diamonds] |> List.map scoreSuit |> List.sum |> float

    let numberOfCards =
        Array.map (Tableau.getVisible >> List.length) >> Array.map (fun x -> x * 1) >> Array.sum >> float

    let combine scorers game = 
        scorers |> List.map (fun f -> game |> Game.getAllTabs |> List.toArray |> f) |> List.sum


    let stageFour = 
        combine [sizeOfHiddenCards; randomScore] 

    let stageOne = 
        combine [numberOfStreaks; lengthOfRuns; numberOfRuns; hiddenCards; randomScore] 

    let stageTwo game = 
        combine [numberOfCards; lengthOfRuns; hiddenCards; randomScore; emptyTab; numberOfSuitsInTab] game

    let stageThree game = 
        suitCompletion game + combine [numberOfSuitsInTab; emptyTab; lengthOfRuns; hiddenCards; randomScore; ] game 

    let playGame () = 
        let rand = Random(1)

        let values = [3;1]

        let isAtRoot ns = 
            List.length ns = List.length values //&& List.head ns = List.head values

        let r, s = runMultistageNestedRolloutFast (hasTimerExpired (List.head values)) isAtRoot (Stopwatch.StartNew()) Game.emptyGame [stageFour; stageThree] values (GameMover.startGame (Card.deck Card.One) rand)
        Game.toString s |> printfn "%s"
        printfn "%A" r

        //let (result, states, tree) = loop searchData (GameMover.startGame rand) Map.empty
        //tree |> Option.iter SpiderTree.printTree


//module Node = 

    //let untriedActions node = 
    //    ()

    //let simulateAction node = 
        //node

    let performAction node = 
        node

    let rolloutActions node rolloutAction depth horizon = 
        node, depth

    let createNode problem state = 
        ()

    // This must walk up the tree (ie from the leaf) and update statistics 
    let update discounting node = 
        node // root

type ActInfo<'a> = {
    Action: 'a 
    Reward: int
    Visits: int
}

module ActInfo = 

    let create action reward visits = 
        {
            Action = action
            Reward = reward
            Visits = visits
        }


module UCT = 
    //// find a node with untried actions by recursing through the children
    //let findNode node action nodeActions selectAction children depth horizon  = 

    let findNode children selectAction simulateAction untriedActions horizon = 
        let rec findNodeRecurse node depth = 
            if node |> untriedActions |> List.isEmpty || node |> children |> List.isEmpty |> not || depth > horizon then 
                (node, selectAction node, depth)
            else 
                findNodeRecurse (simulateAction node) (depth + 1)
        findNodeRecurse

    let expandNode expandAction isGoal performAction untriedActions  horizon node action depth = 
        if untriedActions node |> List.isEmpty && depth <= horizon && not <| isGoal node then
            let action = expandAction node  // use heuristics to pick one of the actions to try
            let node = performAction node
            (node, action, depth + 1)
        else (node, action, depth)


    
    let mcts untriedActions triedActions findNode expandNode rolloutActions update createNode rootState problem budget horizon rolloutAction selectBest root = 

        let rec recurse iterations root = 
            if iterations |> budget |> not then root
            else 
                let node = root
                let depth = 1
                printfn "  step (1): selecting node"    

                let (node, action, depth) = findNode untriedActions horizon node depth

                printfn "  selected node with the state " //  + str(node.state))
                printfn"  step (2): expanding node on depth %d" depth 

                let (node, action, depth) = expandNode untriedActions horizon node action depth 

                printfn "  step (3): performing rollout"
                let (node, depth) = rolloutActions node rolloutAction depth horizon

                printfn "  ended up in the state " //  + str(node.state))
                let root = update node
                recurse (iterations + 1) root

        let root = createNode problem rootState |> recurse 0

        let actions = triedActions root |> List.map (fun (action, (reward, visits)) -> ActInfo.create action reward visits)

        selectBest actions

[<AutoOpen>]
module GameState = 
    type Message = 
        | SetDelta of float 
        | GetDelta of AsyncReplyChannel<float>
        | GetUtility of (GameResult * AsyncReplyChannel<float>)
        | SetUtility of (GameResult * float)
        | GetAllUvalues of AsyncReplyChannel<Map<GameResult, float>>
        | SetLookUpTable of GameResult * MoveType * GameResult
        | GetLookUpTable of GameResult * MoveType * GameResult option AsyncReplyChannel
        | SetBandit of GameResult * MoveType
        | GetBandit of GameResult * MoveType * int AsyncReplyChannel

    let gameState =
        MailboxProcessor.Start(fun inbox ->
            let rec loop (delta, uValues, lookUpTable, bandit) = 
                async {
                    let! msg = inbox.Receive()
                    match msg with 
                    | SetDelta x -> 
                        return! loop (x, uValues,lookUpTable, bandit)
                    | GetDelta rc -> 
                        rc.Reply delta
                        return! loop (delta, uValues,lookUpTable, bandit)
                    | GetUtility (g, rc) -> 
                        Map.tryFind g uValues |> Option.defaultValue 0. |> rc.Reply
                        return! loop (delta, uValues,lookUpTable, bandit)
                    | SetUtility (g, value) -> 
                        return! loop (delta, Map.add g value uValues, lookUpTable, bandit)
                    | GetAllUvalues rc -> 
                        rc.Reply uValues
                        return! loop (delta, uValues, lookUpTable, bandit)
                    | SetLookUpTable (g,m,result) -> 
                        return! loop (delta, uValues, Map.add (g,m) result lookUpTable, bandit)
                    | GetLookUpTable (g,m,rc) -> 
                        Map.tryFind (g,m) lookUpTable |> rc.Reply
                        return! loop (delta, uValues, lookUpTable, bandit)
                    | SetBandit (g,m) -> 
                        let playedCount = Map.tryFind (g,m) bandit |> Option.defaultValue 0
                        return! loop (delta, uValues, lookUpTable, Map.add (g,m) (playedCount + 1) bandit)
                    | GetBandit (g,m,rc) -> 
                        Map.tryFind (g,m) bandit |> Option.defaultValue 0 |> rc.Reply
                        return! loop (delta, uValues, lookUpTable, bandit)

                }
            loop (0.1, Map.empty, Map.empty, Map.empty) )


module Bellman = 

    let gamma = 0.9
    let epsilon = 0.01 
    let errorBandit = 0.01

    let getNextState (g,m) = 
        gameState.PostAndReply (fun rc -> GetLookUpTable (g,m,rc)) |> function 
        | Some x -> x
        | None -> 
            match g with 
            | Won -> g
            | Lost _ -> g
            | Continue (game,_) -> 
                let s' = GameOperations.playMoveToMinima m game
                gameState.Post <| SetLookUpTable (g,m,s')
                s'

    let getUtility state = 
        gameState.PostAndReply (fun rc -> GetUtility (state, rc))

    let updateUtitlity (state, utiltiy) = 
        gameState.Post <| SetUtility (state, utiltiy)

    let getBandit (g,m) = 
        gameState.PostAndReply (fun rc -> GetBandit (g,m,rc))

    let setBandit (g,m) = 
        gameState.Post <| SetBandit (g,m)


    type NextMove = Learning | Playing

    let getNextBestUtilities random (state, history) = 
        match state with 
        | Won -> [None, state, getUtility state]
        | Lost s -> [None, state, getUtility state]
        | Continue (s, moves) -> 

            let states' = 
                moves
                |> (GameOperations.filterLocalLoopMoves GameMover.playMove s)
                |> GameOperations.moveOrdering s
                |> List.map (fun a ->  
                    let s' = getNextState (state, a)
                    Some a, s', getUtility s')
                |> List.filter (fun (_,s',_) -> Set.contains s' history |> not)
                |> (fun xs -> 
                    if List.isEmpty xs then [(None, Lost s, -100.)]
                    else xs )

            match random with 
            | Learning -> 
                if rand.NextDouble() < errorBandit && List.contains Stock moves then
                    let stock = getNextState (state, Stock)
                    [Some Stock, stock, GameOperations.getReward stock]
                else 
                    states' 
            | Playing -> states'

    let getNextBestUtility random (state, history) = 
        (state, history) |> getNextBestUtilities random |> List.maxBy (fun (_,_,u) -> u)     

    let valueIteration game = 

        let computeUtility (state, history) = 
            let a, s', utility = getNextBestUtility Learning (state,history)
            let reward = GameOperations.getReward state
            let discountedFutureUtility = gamma * utility
            a, s', reward + discountedFutureUtility

        let getDelta () = 
            gameState.PostAndReply GetDelta

        let setDelta f = 
            gameState.Post <| SetDelta f

        let numberOfStatesExplored () =
            let gamesStates = gameState.PostAndReply GetAllUvalues
            Map.count gamesStates |> float

        [1.;] |> List.iter (fun tDuration -> 
            while (getDelta () > epsilon * (1. - gamma) / gamma) do
                setDelta 0.

                let rec runValueUpdate (t: Stopwatch) (state, history) = 
                    match state with 
                    | Won -> ()
                    | Lost _ -> ()
                    | Continue (game, _) -> 

                        if game.Spades = Two then ()

                        let currentUtility = getUtility state
                        let a, s', bestUtilityOfNextState = computeUtility (state, history)
                        let history = Set.add s' history

                        updateUtitlity (state, bestUtilityOfNextState)
                        let difference = Math.Abs (bestUtilityOfNextState - currentUtility)
                        if (difference > getDelta ()) then 
                            setDelta difference

                        if (t.Elapsed.TotalMinutes < tDuration) then runValueUpdate t (s', history)

                runValueUpdate (Stopwatch.StartNew()) (game, Set.empty)
                getDelta () |> sprintf "%f" |> printfnWithColor ConsoleColor.DarkYellow
            printfn "Converged for: %f" tDuration )

    let game = 
        let rand = new System.Random(0)
        (GameMover.startGame (Card.deck Card.One) rand)


    let playGame () = 
        let uValuesForGame = gameState.PostAndReply GetAllUvalues

        let rec play n g u = 
            match g with 
            | Won -> printfn "Game Won"
            | Lost _ -> printfn "Game Lost"
            | Continue (s,ms) -> 
                printfn "%s" <| Game.toString s
                let (_, s',u') = getNextBestUtility Playing (g, Set.empty)
                if (n > 0 && Map.containsKey s' uValuesForGame && g <> s') then play (n - 1) s' u'
                elif (g = s') 
                    then play (n - 1) (GameOperations.playMoveToMinima Stock s ) u'

        play 10000000 game 0.


module PolicyIteration = 

    let gamma = 0.9
    let epsilon = 0.1
    ////let mutable delta = 1.
    ////let mutable uValuesForGame = Map.empty

    type Message = 
        | SetDelta of float 
        | GetDelta of AsyncReplyChannel<float>
        | GetQuality of (GameResult * MoveType * AsyncReplyChannel<float>)
        | SetQuality of (GameResult * MoveType * float)
        | GetPolicy of AsyncReplyChannel<Map<GameResult * MoveType, float>>



    //    // 
   
    //let gameState =
    //    MailboxProcessor.Start(fun inbox ->
    //        let rec loop (delta, uValues) = 
    //            async {
    //                let! msg = inbox.Receive()
    //                match msg with 
    //                | SetDelta x -> 
    //                    return! loop (x, uValues)
    //                | GetDelta rc -> 
    //                    rc.Reply delta
    //                    return! loop (delta, uValues)
    //                | GetUtility (g, rc) -> 
    //                    Map.tryFind g uValues |> Option.defaultValue 0. |> rc.Reply
    //                    return! loop (delta, uValues)
    //                | SetUtility (g, value) -> 
    //                    return! loop (delta, Map.add g value uValues)
    //                | GetAllUvalues rc -> 
    //                    rc.Reply uValues
    //                    return! loop (delta, uValues)

    //            }
    //        loop (0.1, Map.empty) )


    //let getUtility state = 
    //    gameState.PostAndReply (fun rc -> GetUtility (state, rc))
    //    //match Map.tryFind state uValuesForGame with 
    //    //| Some x -> x
    //    //| None -> 0.

    //let updateUtitlity (state, utiltiy) = 
    //    gameState.Post <| SetUtility (state, utiltiy)
    //    //uValuesForGame <- Map.add state utiltiy uValuesForGame

    //let getNextBestUtility state = 
    //    match state with 
    //    | Won -> None, state, getUtility state
    //    | Lost s -> None, state, getUtility state
    //    | Continue (state, moves) -> 
    //        moves
    //        |> (ScrathPad.filterLocalLoopMoves GameMover.playMove state)
    //        |> ScrathPad.moveOrdering state
    //        |> List.map (fun a ->  
    //            let s' = GameMover.playMove a state
    //            Some a, s', getUtility s')
    //        |> List.maxBy (fun (_,_,u) -> u) 

    //let valueIteration game = 

    //    let getReward state = 
    //        match state with 
    //        | Won -> 1000.
    //        | Lost _ -> -1000.
    //        | Continue (state, moves) -> 
    //            let suitCompletion = 
    //                let scoreSuit = function 
    //                    | Zero -> 0
    //                    | One -> (13 * 13) * 40
    //                    | Two -> (13 * 13) * 50
    //                    | Three -> (13 * 13) * 60
    //                    | Four -> 1000
    //                [state.Hearts; state.Spades; state.Clubs; state.Diamonds] 
                    
    //                |> List.map scoreSuit |> List.sum

    //            let distanceToEmptyColumn = 
    //                let shortestColumn = 
    //                    state |> Game.getAllTabs  |> List.map Tableau.length |> List.map (fun x -> x + 1) |> List.min
    //                10 / shortestColumn

    //            let lengthOfLogestRun = 
    //                state |> GameMover.getRuns  |> List.map (snd >> List.length) |> List.max

    //            let runToLengthValue = 
    //                let lengthOfRuns = 
    //                    state |> Game.getAllTabs  |> List.map Tableau.getRun |> List.map (List.length)
    //                let lengthOfVisible = 
    //                    state |> Game.getAllTabs  |> List.map Tableau.getVisible |> List.map (List.length)

    //                List.zip lengthOfRuns lengthOfVisible
    //                |> List.map (fun (r,v) -> if (v = 0) then 0 else r/v)
    //                |> List.sum

    //            //let lengthOfRuns = 
    //                //state 
    //                //|> Game.getAllTabs 
    //                //
    //                //|> List.map Tableau.getRun 
    //                //|> List.map (List.length)
    //                //|> List.filter (fun x -> x = 0)
    //                //|> List.map (fun x -> x/10)
    //                //|> List.sum

    //            suitCompletion + distanceToEmptyColumn + lengthOfLogestRun + runToLengthValue  |> float

    //    let computeUtility state = 
    //        let a, s', utility = getNextBestUtility state 
    //        let reward = getReward state
    //        let discountedFutureUtility = gamma * utility
    //        a, s', reward + discountedFutureUtility

    //    let getDelta () = 
    //        gameState.PostAndReply GetDelta

    //    let setDelta f = 
    //        gameState.Post <| SetDelta f

    //    while (getDelta () > epsilon * (1. - gamma) / gamma) do
    //        setDelta 0.

    //        let rec runValueUpdate (t: Stopwatch) state = 

    //            match state with 
    //            | Won -> () 
    //            | Lost _ -> ()
    //            | Continue (game, _) -> 

    //                let completed = 
    //                    [game.Hearts; game.Spades; game.Clubs; game.Diamonds]
    //                    |> List.exists (fun x -> x = One || x = Two)

    //                if completed then () else

    //                let currentUtility = getUtility state
    //                let a, s', bestUtilityOfNextState = computeUtility state
    //                updateUtitlity (state, bestUtilityOfNextState)
    //                let difference = Math.Abs (bestUtilityOfNextState - currentUtility)
    //                if (difference > getDelta ()) then 
    //                    setDelta difference

    //                if (t.Elapsed.TotalSeconds < 10.) then runValueUpdate t s'

    //        runValueUpdate (Stopwatch.StartNew()) game 
    //        printfn "%f" <| getDelta ()


    //let game = 
    //    let rand = new System.Random(0)
    //    (GameMover.startGame (Card.deck Card.One) rand)


    //let playGame () = 
        //let uValuesForGame = gameState.PostAndReply GetAllUvalues

        //let rec play n g u = 
        //    printfn "%s" <| Game.toString g
        //    let (a,s',u') = getNextBestUtility g
        //    if (u' > u && Map.containsKey s' uValuesForGame) then play (n - 1) s' u'

        //play 10000000 game 0.