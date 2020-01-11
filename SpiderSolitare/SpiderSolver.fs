namespace SpiderSolitare.Solver
open SpiderSolitare.Game
open System
open System.Diagnostics
open System.Threading.Tasks
open Logary
open FSharp.Collections.ParallelSeq
open SpiderSolitare.Operations

// [<AutoOpen>]
// module Console = 

//     let printfnWithColor color (text:string) =
//         let orig = Console.ForegroundColor
//         Console.ForegroundColor <- color
//         Console.WriteLine text
//         Console.ForegroundColor <- orig

// type Tree<'LeafData,'INodeData> =
//     | LeafNode of 'LeafData
//     | InternalNode of 'INodeData * Tree<'LeafData,'INodeData> list

// module Tree =
//     let rec cata fLeaf fNode (tree:Tree<'LeafData,'INodeData>) :'r = 
//         let recurse = cata fLeaf fNode  
//         match tree with
//         | LeafNode leafInfo -> 
//             fLeaf leafInfo 
//         | InternalNode (nodeInfo,subtrees) -> 
//             if List.isEmpty subtrees then [] 
//             else subtrees |> List.map recurse
//             |> fNode nodeInfo

//     let rec fold fLeaf fNode acc (tree:Tree<'LeafData,'INodeData>) :'r = 
//         let recurse = fold fLeaf fNode  
//         match tree with
//         | LeafNode leafInfo -> 
//             fLeaf acc leafInfo 
//         | InternalNode (nodeInfo,subtrees) -> 
//             let localAccum = fNode acc nodeInfo
//             if List.isEmpty subtrees then localAccum else
//                 let finalAccum = Seq.fold recurse localAccum subtrees
//                 finalAccum 
        
//     let rec map fLeaf fNode (tree:Tree<'LeafData,'INodeData>) = 
//         let recurse = map fLeaf fNode  
//         match tree with
//         | LeafNode leafInfo -> 
//             let newLeafInfo = fLeaf leafInfo
//             LeafNode newLeafInfo 
//         | InternalNode (nodeInfo,subtrees) -> 
//             let newNodeInfo = fNode nodeInfo
//             let newSubtrees = 
//                 if List.isEmpty subtrees 
//                 then [] 
//                 else subtrees |> List.map recurse 
//             InternalNode (newNodeInfo, newSubtrees)

//     let rec foldFast fLeaf fNode (acc: 'r) (tree:Tree<'LeafData,'INodeData>): 'r = 
//         let recurse = foldFast fLeaf fNode  
//         match tree with
//         | LeafNode leafInfo -> 
//             fLeaf acc leafInfo
//         | InternalNode (nodeInfo,subtrees) -> 
//             let (localAccum, searchCompleted) = fNode acc nodeInfo
//             if searchCompleted then localAccum else
//                 let finalAccum = subtrees |> List.fold recurse localAccum 
//                 finalAccum 

//     let rec foldFastWithChildren fLeaf (fNode: 'r -> 'INodeData -> Tree<'LeafData,'INodeData> list -> ('r * bool)) acc (tree:Tree<'LeafData,'INodeData>): 'r = 
//         let recurse = foldFastWithChildren fLeaf fNode  
//         match tree with
//         | LeafNode leafInfo -> 
//             fLeaf acc leafInfo
//         | InternalNode (nodeInfo,subtrees) -> 
//             let (localAccum, searchCompleted) = fNode acc nodeInfo subtrees
//             if searchCompleted then localAccum 
//             else
//                 subtrees |> List.fold recurse localAccum 

// type InternalGame = {
//     MoveType: MoveType Option
//     Heuristic: int
//     Game: Game
//     Moves: MoveType list
// }

// type MovePlayedGame = {
//     Heuristic: int
//     Move: MoveType
//     Game: Game
//     Moves: MoveType list
// }

// type SpiderTree = Tree<MovePlayedGame,InternalGame>
// type SpiderDepthTree = Tree<(int * MovePlayedGame), (int * InternalGame)>
// module SpiderTree = 

//     let playeMovedToInternalGame playedMove = 
//        {MoveType = Some playedMove.Move; Game = playedMove.Game; Moves = playedMove.Moves; Heuristic = playedMove.Heuristic}


//     let createTree game moves h : SpiderTree = 
//         InternalNode ({MoveType = None; Game = game; Moves = moves; Heuristic = h}, [])

//     let appendLeaf node trees playedMove = 
//         InternalNode (node, (LeafNode playedMove) :: trees)

//     let appendTree node trees tree = 
//         InternalNode (node, tree :: trees)

//     let replaceTrees node trees = 
//         InternalNode (node, trees)

//     let appendTreeAtLeaf leaf tree = 
//         InternalNode (playeMovedToInternalGame leaf, [tree])

//     let createNodeFromLeaf leaf playedMove: SpiderTree = 
//         InternalNode (playeMovedToInternalGame leaf, [(LeafNode playedMove)])

//     let getTrees = function
//     | LeafNode _ -> []
//     | InternalNode (node, trees) -> trees

//     let getRoot f g = function 
//     | LeafNode leaf -> f leaf
//     | InternalNode (node:InternalGame, _) -> g node

//     let getGame = 
//         getRoot (fun leaf -> leaf.Game) (fun node -> node.Game)

//     let getRootMove = 
//         getRoot (fun leaf -> Some leaf.Move) (fun node -> node.MoveType)

//     let getHeuristic = 
//         getRoot (fun leaf -> leaf.Heuristic) (fun node -> node.Heuristic)

//     let depth tree = 
//         let fLeaf a = 1

//         let fNode a leaves = 
//             if List.isEmpty leaves then 1 else 
//             let leaves = List.toArray leaves
//             leaves |> Array.max |> (fun x -> x + 1)

//         Tree.cata fLeaf fNode tree

//     let toList tree = 
//         let fLeaf acc (leaf : MovePlayedGame) = leaf.Game :: acc
//         let fNode acc (node: InternalGame) = node.Game :: acc 
//         Tree.fold fLeaf fNode [] tree

//     let addDepthDescending (tree: SpiderTree): SpiderDepthTree = 

//         let fLeaf leaf = 
//             LeafNode (0, leaf)

//         let fNode node leaves = 
//             let level = 
//                 if List.isEmpty leaves 
//                 then 0 
//                 else leaves |> List.map (depth) |> List.max 
//             InternalNode ((level + 1, node), leaves)

//         Tree.cata fLeaf fNode tree

//     let depthAscending tree =
//         let max = depth tree
//         let fLeaf (depth, leaf) = (max - depth, leaf)      
//         let fNode (depth, node) = (max - depth, node)
//         Tree.map fLeaf fNode tree

//     let findBestScore tree = 
//         let fLeaf max leaf = Math.Max (max, leaf.Heuristic)
//         let fNode max (node: InternalGame) = max
//         Tree.fold fLeaf fNode Core.int.MinValue tree

//     let totalCount (tree: SpiderTree) = 
//         let fLeaf total _ = total + 1 
//         let fNode total _ = total + 1

//         Tree.fold fLeaf fNode 0 tree

//     let canAddWithHeuristic playedMove targetGame tree = 

//         let fLeaf acc leaf = 
//             leaf.Heuristic > playedMove.Heuristic |> not

//         let fNode acc (node: InternalGame) = 
//             if acc = false then false, true 
//             elif node.Heuristic > playedMove.Heuristic then false, true
//             else acc, false

//         Tree.foldFast fLeaf fNode true tree

//     let containsGame game tree = 

//         let fLeaf acc leaf = 
//             leaf.Game = game

//         let fNode acc (node: InternalGame) = 
//             if acc then acc, true 
//             else node.Game = game, false

//         Tree.foldFast fLeaf fNode false tree

//     let addMovePlayedNonGreedy targetGame playedMove tree = 
//         if containsGame playedMove.Game tree || 
//             not <| canAddWithHeuristic playedMove targetGame tree then tree else 

//         let fLeaf leaf: SpiderTree = 
//             if (leaf.Game = targetGame) then createNodeFromLeaf leaf playedMove
//             else LeafNode leaf

//         let fNode (node: InternalGame) trees = 
//             if node.Game = targetGame then 
//                 appendLeaf node trees playedMove
//             else InternalNode (node, trees)

//         Tree.cata fLeaf fNode tree 

//     let addMovePlayedGreedy targetGame playedMove tree = 
//         //if containsGame playedMove.Game tree then tree else 

//         let fLeaf leaf: SpiderTree = 
//             if (leaf.Game = targetGame) then createNodeFromLeaf leaf playedMove
//             else LeafNode leaf

//         let fNode (node: InternalGame) trees = 
//             if node.Game = targetGame then 
//                 appendLeaf node trees playedMove
//             else InternalNode (node, trees)

//         Tree.cata fLeaf fNode tree 

//     let addTreeAtGame newTree tree = 

//         let targetGame = getGame newTree
//         let trees = getTrees newTree

//         let fLeaf leaf: SpiderTree = 
//             if (leaf.Game = targetGame) 
//             then replaceTrees (playeMovedToInternalGame leaf) trees
//             else LeafNode leaf

//         let fNode (node: InternalGame) tr = 
//             if node.Game = targetGame 
//             then replaceTrees node trees
//             else InternalNode (node, tr)

//         Tree.cata fLeaf fNode tree 

//     let increaseBaseHeuristic amountToIncrease tree = 

//         let fLeaf (leaf: MovePlayedGame) = 
//             {leaf with Heuristic = leaf.Heuristic + amountToIncrease}

//         let fNode (node: InternalGame) = 
//             {node with Heuristic = node.Heuristic + amountToIncrease}

//         Tree.map fLeaf fNode tree 

//     let getNode game tree = 

//         let fLeaf acc (leaf: MovePlayedGame) = 
//             acc

//         let fNode acc (node: InternalGame) trees = 
//             match acc with 
//             | Some x -> Some x, true
//             | None -> 
//                 if node.Game = game then 
//                     Some <| InternalNode (node,trees), true
//                 else 
//                     acc, false

//         Tree.foldFastWithChildren fLeaf fNode None tree 


//     let toString tree = 
//         let prefix depth = String.replicate depth "  "

//         let printGame header prefix game = 
//             header @ App.toStringGame game |> List.map (sprintf "%s%s" prefix) |> String.concat "\n"

//         let fLeaf acc (depth, leaf) = 
//             let header =  [
//                 "Leaf"
//                 App.printMove leaf.Move
//                 string leaf.Heuristic  ]
//             acc + (printGame header (prefix depth) leaf.Game) + "\n"

//         let fNode acc (depth, node: InternalGame) =
//             let prefix = prefix depth 
//             let header = [
//                 "Node..."
//                 string node.Heuristic
//                 sprintf "%A" <| Option.map App.printMove node.MoveType ]
//             acc + (printGame header prefix node.Game + "\n")

//         Tree.fold fLeaf fNode "" (tree |> addDepthDescending |> depthAscending)

//     let printTree tree = 

//         printfn "Printing tree.."
//         tree |> toString |> printfn "%s"
//         printfn "Printing complete"


// type Heuristic = Game -> int
// type Depth = int
// type SearchData = {
//    Heuristics: (Heuristic * Depth) list
//    TimeLimit: Stopwatch
// }

// //type GameMoves = {
// //    Game: Game
// //    Moves: MoveType list 
// //}

// module SearchData = 

//     let getHeuristic searchData = 
//         searchData.Heuristics |> List.head |> fst

//     let getDepth searchData = 
//         searchData.Heuristics |> List.head |> snd

//     let startTimer searchData = 
//         {searchData with TimeLimit = Stopwatch.StartNew()}

//     let exceededBottom searchData = 
//         searchData |> getDepth <= 1 || List.length searchData.Heuristics = 0
//         //timeLimit.ElapsedMilliseconds > 8L * 1000L

//     let atBottom searchData = 
//         searchData |> getDepth = 1 && List.length searchData.Heuristics > 1

//     let nextHeuristic searchData = 
//         {searchData with Heuristics = List.tail searchData.Heuristics}

//     let reduceFirstHeuristicDepth searchData =  
//         let h = getHeuristic searchData
//         let depth = getDepth searchData 
//         let tail = List.tail searchData.Heuristics
//         {searchData with Heuristics = (h, depth - 1) :: tail}

// module Search = 
//     type Result = GameLost | GameWon 
//     type Status = Result of Result | MaxHeuristic of int



//     let updateStates (knownStates) gameResults = 
//         let folder acc game = 
//             match game with 
//             | Won -> acc
//             | Lost game -> game :: acc
//             | Continue (game, _) -> game :: acc

//         gameResults |> List.fold folder [] |> Set.ofList |> Set.union knownStates 

//     let filterGameResults knownGames gameResult = 
//         match gameResult with 
//         | Won -> false
//         | Lost game -> false
//         | Continue (game, _) -> Set.contains game knownGames |> not

//     let rec playGamesWithMoves f knownStates = function 
//     | [] -> (GameLost, knownStates)
//     | (game, moves)::xs -> 
//         match f knownStates game moves with 
//         | GameWon, newStates -> (GameWon, Set.union newStates knownStates)
//         | GameLost, newStates -> playGamesWithMoves f (Set.union newStates knownStates) xs


//     let playMoves playMove heuristic knownStates moves game gameWon evaulateAndContinue tree memoGames =

//         let wonGame = function | Won -> true | _ -> false

//         let (|HasWon|_|) gameAndMoves =
//             let games = gameAndMoves |> Seq.map snd
//             match Seq.isEmpty games |> not &&  games |> Seq.map wonGame |> Seq.reduce (||) with 
//             | true -> gameAndMoves |> Seq.tryFind (fun (m,g) ->  wonGame g) |> Option.map fst
//             | false -> None

//         let (|GamesAndMoves|_|) heuristic gameAndMoves =
//             let games = gameAndMoves |> Seq.map snd
//             match Seq.isEmpty games || games |> Seq.map wonGame |> Seq.reduce (||) |> not with 
//             | true -> 
//                 gameAndMoves 
//                 |> GameOperations.toGameAndMoves 
//                 |> List.map (fun (move,(game,moves)) -> {Move = move; Game = game; Moves = moves; Heuristic = heuristic game})
//                 |> Some
//             | false -> None

//         let games = moves |> List.map (fun x -> x,(playMove game x))
//         match games with 
//         | HasWon move -> gameWon move
//         | GamesAndMoves heuristic gamesAndMoves -> 
//             let gamesAndMoves = gamesAndMoves |> List.filter (fun x -> Set.contains x.Game knownStates |> not)
//             let knownStates = updateStates knownStates (List.map snd games)
//             if Seq.isEmpty gamesAndMoves 
//             then (tree, memoGames)
//             else evaulateAndContinue (knownStates, gamesAndMoves)
//         | _ -> (tree, memoGames)

//     let addGamesToTree addPlayedMove games targetGame tree = 
//         let addMove = addPlayedMove targetGame
//         games |> List.fold (fun tree playedMove -> addMove playedMove tree) tree

//     let shouldChangeHeuristic searchData game tree = 
//         if SearchData.atBottom searchData then false else
        
//         let trees = 
//             SpiderTree.getNode game tree 
//             |> Option.defaultValue (SpiderTree.createTree game [] 0)
//             |> SpiderTree.getTrees 

//         if List.isEmpty trees then false else
//         let currentHeuristic = SearchData.getHeuristic searchData game

//         trees
//         |> List.map SpiderTree.getHeuristic
//         |> List.forall (fun x -> x < currentHeuristic)


//     let playNextHeuristic recurseWithHeuristic heuristic game moves tree = 
//         let bestScore = SpiderTree.findBestScore tree
//         let (nextHeuristicTree, memoGames) = recurseWithHeuristic (SpiderTree.createTree game moves (heuristic game)) game moves 
//         //let nextHeuristicTree = nextHeuristicTree |> SpiderTree.increaseBaseHeuristic bestScore
//         SpiderTree.addTreeAtGame nextHeuristicTree tree, memoGames

//         // shouldChangeHeuristic heuristics heuristic
//     let playNextMoves shouldChange recurseWithHeuristic heuristic game moves tree memoGames = 
//         if shouldChange game tree then 
//             playNextHeuristic recurseWithHeuristic heuristic game moves tree
//          else tree, memoGames



//     let rec rollout playMove searchData knownStates memoGames tree game moves =
//         if SearchData.exceededBottom searchData || List.isEmpty moves then (tree, memoGames) else 
//             let knownStates = tree |> SpiderTree.toList |> Set.ofList |> Set.union knownStates

//             let handlePlayedMove tree (knownStates, gamesAndMoves) =

//                 let tree = tree |> addGamesToTree SpiderTree.addMovePlayedNonGreedy gamesAndMoves game
//                 let heuristicDepth = List.length searchData.Heuristics
//                 let memoGames = 
//                     if Map.containsKey (game, heuristicDepth) memoGames then memoGames else 
//                     Map.add (game, heuristicDepth) (tree, knownStates, gamesAndMoves) memoGames
//                 if SearchData.atBottom searchData then 
//                     let recurse = rollout playMove (SearchData.nextHeuristic searchData) Set.empty memoGames
//                     playNextHeuristic recurse (SearchData.getHeuristic searchData) game moves tree
//                 else 
//                     let (tree, memoGames) = 
//                         gamesAndMoves 
//                         |> List.sortByDescending (fun x -> x.Heuristic) 
//                         |> List.fold (fun (tree, memo) playedMove -> 
//                             let recurse = rollout playMove (SearchData.reduceFirstHeuristicDepth searchData) knownStates memoGames
//                             recurse tree playedMove.Game playedMove.Moves) (tree, memoGames)

//                     //let memoGames = Map.add (game, List.length heuristics) (tree, knownStates, moves) memoGames
//                     let recurse = rollout playMove (SearchData.nextHeuristic searchData) Set.empty memoGames
//                     (playNextMoves (shouldChangeHeuristic searchData) recurse (SearchData.getHeuristic searchData) game moves tree) memoGames

//             let heuristicKey = List.length searchData.Heuristics
//             if Map.containsKey (game, heuristicKey) memoGames && false then 
//                 let (tree, knownStates, moves) = Map.find (game, heuristicKey) memoGames
//                 handlePlayedMove tree (knownStates, moves)
//             else 
//                 playMoves playMove (SearchData.getHeuristic searchData) knownStates moves game (fun move -> tree, memoGames) (handlePlayedMove tree) tree memoGames
        
//     let getBestMove tree = 
//         tree |> SpiderTree.getTrees |> function
//         | [] -> None
//         | xs -> 
//             xs
//             |> List.map (fun x -> x, SpiderTree.findBestScore x)
//             |> List.sortByDescending snd 
//             |> List.tryHead
//             |> Option.map fst 
//             |> Option.bind SpiderTree.getRootMove

//     let loop searchData gameResult memo = 
//         let rec play knownStates currentH gameResult depth memo movesPlayed = 
//             match gameResult with 
//             | Lost game -> (GameLost, knownStates, None)
//             | Won -> (GameLost, knownStates, None)
//             | Continue (game, moves) -> 

//                 if Set.contains game knownStates 
//                 then (GameLost, knownStates, None)
//                 else 

//                 let knownStates = Set.add game knownStates 

//                 Game.toString game |> printfn "%s"
//                 printfn "Current Score: %d" currentH
//                 if depth = 0 then (GameLost, knownStates, None) else

//                 let tree = SpiderTree.createTree game moves currentH

//                 let searchData = SearchData.startTimer searchData
//                 let playMove = F.flip GameMover.playMove
//                 let runRollout = rollout playMove searchData knownStates memo tree 

//                 let tree, memo = runRollout game moves
//                 let bestMove =  getBestMove tree
//                 let h = SpiderTree.findBestScore tree
                   
//                 //SpiderTree.printTree tree

//                 printfn ""
//                 printfn "Max depth: %d" <| SpiderTree.depth tree
//                 printfn "Total Count: %d" <| SpiderTree.totalCount tree
//                 printfn "BestMove: %A" <| Option.map App.printMove bestMove
//                 printfn "BestScore: %d" h
//                 knownStates |> Set.count |> printfn "Unique game count: %d"
//                 printfn ""

//                 match bestMove with 
//                 | None -> (GameLost, knownStates, Some tree)
//                 | Some move ->
//                     let h = SearchData.getHeuristic searchData
//                     play knownStates (h game) (GameMover.playMove move game) (depth - 1) memo movesPlayed

//         play Set.empty Core.int.MinValue gameResult Core.int.MaxValue memo Set.empty

//     let getGamesAndMoves gameResult = 
//         match gameResult with 
//         | Lost game -> None
//         | Won -> None
//         | Continue x -> Some (x)

//     type T = {
//         G:Game
//         G2:Game
//         Moves:MoveType list
//         Tree:SpiderTree 
//         Tree2:SpiderTree 
//         Tree3:SpiderTree 
//         Tree4:SpiderTree 
//         Tree5:SpiderTree 
//     }

//     let setup () = 
//         let g = Game.emptyGame
//         let g2 = {Game.emptyGame with Diamonds = One}
//         let g3 = {Game.emptyGame with Diamonds = Two}
//         let g4 = {Game.emptyGame with Diamonds = Two; Clubs = One}
//         let g5 = {Game.emptyGame with Diamonds = Two; Clubs = Two}
//         let moves = [Stock]
//         let tree = SpiderTree.createTree g moves 0
//         let tree2 = SpiderTree.addMovePlayedGreedy g {Move = Stock; Game = g2; Heuristic = 1; Moves = moves} tree
//         let tree3 = SpiderTree.addMovePlayedGreedy g2 {Move = Stock; Game = g3; Heuristic = 1; Moves = moves} tree2
//         let tree4 = SpiderTree.addMovePlayedGreedy g {Move = Stock; Game = g4; Heuristic = 1; Moves = moves} tree3
//         let tree5 = SpiderTree.addMovePlayedGreedy g4 {Move = Stock; Game = g5; Heuristic = 1; Moves = moves} tree4

//         { G = g; G2 = g2; Moves = moves; Tree = tree; Tree2 = tree2; Tree3 = tree3; Tree4 = tree4; Tree5 = tree5; }




// module ScrathPad =
   
//     let rec multistageNestedRollout inDeadEndOrWin argMax maxH result getA playedActions (hs: ((Game -> int) list)) (ns: int list) (s:Game): (int * MoveType list) = 
//         let recurse = multistageNestedRollout inDeadEndOrWin argMax maxH result getA
//         if inDeadEndOrWin s then List.head hs s, playedActions else 
//             if List.head ns = -1 then 
//                 List.head hs s, playedActions
//             else
//                 let recurse' = recurse [] hs (((List.head ns) - 1) :: (List.tail ns))
//                 let (maxHeuristicVal, a) = s |> getA |> maxH (fun a -> result s a |> recurse' |> fst) 
//                 let a = 
//                     if (List.head hs s) > maxHeuristicVal && List.length hs > 1 then 
//                         let recurse = recurse [] (List.tail hs) (List.tail ns)
//                         s |> getA |> argMax (fun a -> result s a |> recurse |> fst) 
//                     else a
//                 let playedActions = a :: playedActions
//                 result s a |> recurse playedActions hs ns


//     type SearchResult = SearchWon | SearchLost | Searched of float

//     let playheuristic hs s = 
//         List.head hs s

//     let levelNMinusOne ns = 
//         (List.head ns) - 1 :: (List.tail ns)

//     let hitRockBottom ns = 
//         List.head ns = -1


//     type Heurisitc = int
//     type level = int
//     type Message = 
//         | Add of (Game * Heurisitc * level * SearchResult)
//         | Get of (Game * Heurisitc * level * (SearchResult * Game) Option AsyncReplyChannel)
//         | Size of int AsyncReplyChannel
//         | Clear 

//     module GameStateCache = 
//         type Message = 
//             | Add of (GameResult * MoveType)
//             | Get of (MoveType * GameResult Option AsyncReplyChannel)
//             | Size of int AsyncReplyChannel
//             | Clear 

//     let gameToMoveCache = 
//         MailboxProcessor.Start(fun inbox ->
//             let rec loop cache = 
//                 async {
//                     let! msg = inbox.Receive()
//                     match msg with 
//                     | GameStateCache.Message.Add (g, m) -> 
//                         let cache = Map.add m g cache
//                         return! loop cache
//                     | GameStateCache.Message.Get (m,rc) -> 
//                         Map.tryFind (m) cache |> rc.Reply
//                         return! loop cache
//                     | GameStateCache.Message.Size rc -> 
//                         Map.count cache |> rc.Reply
//                         return! loop cache 
//                     | GameStateCache.Message.Clear -> 
//                         return! loop Map.empty }
//             loop Map.empty)

//     let gameCache = 
//         MailboxProcessor.Start(fun inbox ->
//             let rec loop cache = 
//                 async {
//                     let! msg = inbox.Receive()
//                     match msg with 
//                     | Add (g,h,l,z) -> 
//                         let cache = Map.add (g,h,l) (z, g) cache
//                         return! loop cache
//                     | Get (g,h,l,rc) -> 
//                         Map.tryFind (g,h,l) cache |> rc.Reply
//                         return! loop cache
//                     | Size rc -> 
//                         Map.count cache |> rc.Reply
//                         return! loop cache 
//                     | Clear -> 
//                         return! loop Map.empty }
//             loop Map.empty)

//     module GlobalCache = 
//         type GlobalCacheMessage = 
//             | Add of (Heurisitc * level * Game)
//             | Get of (Heurisitc * level * Game Set AsyncReplyChannel)
//             | Size of int AsyncReplyChannel
//             | Clear 

//     let globalCache = 
//         MailboxProcessor.Start(fun inbox ->
//             let rec loop cache = 
//                 async {
//                     let! msg = inbox.Receive()
//                     match msg with 
//                     | GlobalCache.Add (h,l,s) -> 
//                         let history = Map.tryFind (h, l) cache |> Option.defaultValue Set.empty
//                         let cache = Map.add (h,l) (Set.add s history) cache 
//                         return! loop cache
//                     | GlobalCache.Get (h,l,rc) -> 
//                         Map.tryFind (h, l) cache |> Option.defaultValue Set.empty |> rc.Reply
//                         return! loop cache
//                     | GlobalCache.Size rc -> 
//                         Map.count cache |> rc.Reply
//                         return! loop cache 
//                     | GlobalCache.Clear -> 
//                         return! loop Map.empty }
//             loop Map.empty)

//     let printQueue =
//         MailboxProcessor.Start(fun inbox ->
//            let rec loop () = 
//                async {
//                    let! msg = inbox.Receive()
//                    printfn "%s" msg
//                    return! loop () }
//            loop ())

//     let log s = printQueue.Post s

//     let logAtRoot isAtRoot ns m = 
//         if isAtRoot ns then 
//             sprintf "Depth: %d" (List.head ns) |> log
//             m |> log

//     let getFromCache s h n = 
//         gameCache.PostAndReply (fun rc -> Get (s, h, n, rc))

//     let addToCache s h n sr = 
//         //if n > 0 then 
//         (s,h,n,sr) |> Add |> gameCache.Post 

//     let addToGlobalCache h n s =
//         GlobalCache.Add (h,n,s) |> globalCache.Post 

//     let getFromGlobalCache h n = 
//         globalCache.PostAndReply (fun rc -> GlobalCache.Get (h,n,rc))

//     let clear () = gameCache.Post Clear
//     let clearGlobal () = globalCache.Post GlobalCache.Clear

//     let isDeadEnd s a = 
//         if List.isEmpty a then true else false

//     let hasTimerExpired max n (timer: Stopwatch) =
//         if n < max && timer.Elapsed > TimeSpan.FromSeconds 1. then true else false

//     type T<'a> = {Result: 'a}

//     let taskLong (f:unit -> 'T) = 
//         { Result = f () }
//         //let t = new Task<'T>(new Func<'T>(f), TaskCreationOptions.LongRunning)
//         //t.Start()
//         //t

//     let maxH f actions = 

//         let actions = List.toArray actions
//         let nextActions = Array.Parallel.map (fun a -> a, f a) actions |> Array.toList

//         //let handleNextActions nextActions = 
//         let wonGames = nextActions |> List.filter (fun (a,h) -> h |> function | SearchWon -> true | _ -> false)
//         let lostGames = nextActions |> List.filter (fun (a,h) -> h |> function | SearchLost -> true | _ -> false)
//         let searchGames = 
//             nextActions 
//             |> List.fold (fun acc (a,h) -> 
//                 match h with 
//                 | SearchWon -> acc
//                 | SearchLost -> acc
//                 | Searched h -> (a, h) :: acc ) []
//             |> List.sortByDescending (fun (a,h) -> h)
//             |> List.map (fun (a,h) -> a, Searched h)

//         wonGames @ searchGames @ lostGames

//         //Task.Factory.ContinueWhenAll (nextActions, fun (tasks: Task<_>[]) -> 
//             //tasks|> Array.map (fun x -> x.Result) |> Array.zip actions |> Array.toList |> handleNextActions )

    
//     let playGameResult playedActions whenContinue s = 
//         match s with
//         | Won -> (SearchWon, playedActions)
//         | Lost s -> (SearchLost, s)
//         | Continue (s, ms) -> whenContinue s ms

    

    

//     let rec multistageNestedRolloutFast 
//                 (hasTimeExpired: int -> Stopwatch -> bool) 
//                 isDeadEnd 
//                 getFromCache 
//                 addToCache
//                 filterLocalLoop
//                 maxH 
//                 moveOrdering
//                 result 
//                 isAtRoot
//                 (timer: Stopwatch) 
//                 playedActions 
//             (hs: ((Game -> float) list)) (ns: int list) (s:GameResult) =
//             let recurse = multistageNestedRolloutFast hasTimeExpired isDeadEnd getFromCache addToCache filterLocalLoop maxH moveOrdering result isAtRoot

//             let filterPlayedMoves s ms = ms
//                 //if (List.length hs = 1 && List.head ns = 0) then 
//                 //    let playedMoves = getFromGlobalCache (List.length hs) (List.head ns) s
//                 //    ms |> List.filter (fun x -> Set.contains x playedMoves |> not)
//                 //else ms

//             let isInLoop h n s = 
//                 getFromGlobalCache h n |> Set.contains s

//             let logAtRoot = logAtRoot isAtRoot ns

//             let rec innerRec s ms = 

//                 "\n----" |> logAtRoot
//                 Game.toString s |> logAtRoot
//                 ms |> App.printMoves |> String.concat ", " |> logAtRoot
//                 "----\n" |> logAtRoot

//                 let h = (fun () -> playheuristic hs s) |> taskLong |> (fun t -> t.Result)
//                 let ms = taskLong (fun () -> filterLocalLoop s ms |> filterPlayedMoves s |> moveOrdering s) |> (fun t -> t.Result)

//                 if List.isEmpty ms then
//                     "At and in a loop :(" |> logAtRoot
//                     addToCache s (List.length hs) (List.head ns) SearchLost 
//                     (SearchLost, s)
//                 elif isInLoop (List.length hs) (List.head ns) s then 

//                     "At and in a loop :(" |> logAtRoot
//                     addToCache s (List.length hs) (List.head ns) SearchLost 
//                     (SearchLost, s)
//                 elif isAtRoot ns |> not && hasTimeExpired (List.length hs) timer then 
//                     let hApplied = taskLong (fun () -> playheuristic hs s |> Searched) |> (fun t -> t.Result)
//                     (hApplied, s)
//                 else
//                     //addToGlobalCache (List.length hs) (List.head ns) s

//                     "\nFiltered moves:" |> logAtRoot
//                     ms |> App.printMoves |> String.concat ", " |> logAtRoot
//                     "---\n" |> logAtRoot

//                 //printfn "Cache Size = %d" (gameCache.PostAndReply Size)
//                 //sprintf "Global Size = %d" (globalCache.PostAndReply GlobalCache.Size) |> log

//                 //ms |> App.printMoves |> String.concat ", " |> printfn "%s"

//                 //ms |> App.printMoves |> String.concat ", " |> printfn "%s"

//                 //if isDeadEnd s ms || hasTimeExpired (List.head ns) timer then 
//                 //    let hApplied = playheuristic hs s |> Searched
//                 //    (hApplied, playedActions)
//                 //else

//                     let timer = if isAtRoot ns then Stopwatch.StartNew() else timer

//                     let recurse' = recurse timer s hs (levelNMinusOne ns)
//                     let results = ms |> maxH (fun a -> result s a |> recurse' |> fst)
//                     let (a, maxVal) = List.head results

//                     if isAtRoot ns then 
//                         "\nSearch Results" |> log
//                         Game.toString s |> log
//                         results |> List.iter (fun (m, sa) -> printfn "Move: %s, SearchResult: %A" (App.printMove m) sa)
//                         sprintf "Selected move: %s" (App.printMove a) |> log
//                         sprintf "Search Result: %A" maxVal |> log
//                         "------\n" |> log

//                     let nextHeuristic () = 
//                         getFromCache s (List.length hs) (List.head ns) |> function 
//                         | Some (z, cachedGame) -> 
//                             if z = SearchLost then (SearchLost, s) 
//                             else recurse timer s (List.tail hs) (List.tail ns) (Continue (s, ms))
//                         | None -> 
//                             //printfn "Next heuristic"
//                             recurse timer s (List.tail hs) (List.tail ns) (Continue (s, ms))

//                     match maxVal with 
//                     | SearchWon -> (SearchWon, s)
//                     | SearchLost -> if (List.length hs = 1) then (SearchLost, s) else nextHeuristic()
//                     | Searched hNext ->
//                         logAtRoot "\nPlaying search result"
//                         sprintf "H: %f, v: %f H function: %d, depth: %d" h hNext (List.length hs) (List.head ns) |> logAtRoot
//                         "---\n" |> logAtRoot

//                         if hNext < h && (List.length hs > 1) then 
//                             logAtRoot "Moving to next heuristic:"
//                             addToCache s (List.length hs) (List.head ns) (Searched h) 
//                             nextHeuristic()

//                         //elif hNext < h then (Searched hNext, playedActions) // NonGreedy search
//                         else
//                             logAtRoot "Moving to next heuristic:"
//                             //printfn "Playing move: %s" <| App.printMove a 
//                             if (List.length hs = 1 && List.head ns = 0) then addToGlobalCache 1 0 s
//                             result s a |> playGameResult s innerRec

//             let playContinuedGame timer s ms = 
//                 //taskLong <| fun () -> 
//                 //if isAtRoot ns then 
//                     //App.printableGame s |> log
//                 //elif List.length hs = 1 && (List.head ns = 2) then  
//                     //App.printableGame s |> log

//                 //let ms = taskLong (fun () -> filterLocalLoop s ms |> moveOrdering s |> filterPlayedMoves s) |> (fun t -> t.Result)

//                 if isDeadEnd s ms then  //hasTimeExpired (List.head ns) timer || 
//                     sprintf "bottom:  isDeadEnd" |> log
//                     addToCache s (List.length hs) (List.head ns) SearchLost
//                     (SearchLost, s)
//                 elif hitRockBottom ns || hasTimeExpired (List.length hs) timer then 
//                     let hApplied = taskLong (fun () -> playheuristic hs s |> Searched) |> (fun t -> t.Result)
//                     (hApplied, s)
//                 else
//                     getFromCache s (List.length hs) (List.head ns) |> function 
//                     | Some (z, cachedGame) -> 
//                         if z = SearchLost then (SearchLost, s)   // z = 0 is in the white paper, it's unlcear what 0 means
//                         elif (List.length hs > 1) then recurse timer s (List.tail hs) (List.tail ns) (Continue (s, ms))
//                         else innerRec s ms
//                     | None -> 
//                         innerRec s ms

//             playGameResult playedActions (playContinuedGame timer) s

//     let playMove s a = 
//         gameToMoveCache.PostAndReply (fun rc -> GameStateCache.Message.Get (a, rc)) |> function 
//         | Some sNext -> sNext
//         | None -> 
//             let sNext = GameMover.playMove a s

//             //match sNext with 
//             //| Won -> ()
//             //| Lost _ -> () 
//             //| Continue (g,ms) -> 

//                 //"\n----------" |> log
//                 //"Orig Game:" |> log
//                 //App.printableGame s |> log
//                 //a |> App.printMove |> log
//                 //"New Game:" |> log
//                 //App.printableGame g |> log
//                 //ms |> App.printMoves |> String.concat ", " |> log
//                 //"__________\n" |> log

//             gameToMoveCache.Post <| GameStateCache.Message.Add (sNext, a)
//             sNext
             
        
//     let runMultistageNestedRolloutFast hasTimerExpired = 
//             multistageNestedRolloutFast hasTimerExpired isDeadEnd getFromCache 
//                                         addToCache (GameOperations.filterLocalLoopMoves GameMover.playMove) maxH GameOperations.moveOrdering playMove 
//     let raiseToPower pow = 
//         (float >> F.flip (F.curry Math.Pow) pow >> int) 

//     let hiddenCards tabs = 
//         (54 - (tabs |> Array.map MLTab.hiddenLength |> Array.sum)) * 2 |> float

//     let sizeOfHiddenCards tabs = 
//         tabs 
//         |> Array.map MLTab.hiddenLength
//         |> Array.map (raiseToPower 3.) 
//         |> Array.map (fun x -> x * -1) 
//         |> Array.sum
//         |> float


//     let lengthOfRuns = 
//         Array.map (Tableau.getRun >> List.length)
//         >> Array.map (raiseToPower 3.) 
//         >> Array.sum
//         >> float

//     let numberOfRuns = 
//         Array.map (Tableau.getRun >> List.length) 
//         >> Array.filter (fun x -> x > 1) 
//         >> Array.map (raiseToPower 2.) 
//         >> Array.sum
//         >> float

//     let faceUpCards tabs = 
//         tabs 
//         |> Array.map (Tableau.getVisible >> List.length)
//         |> Array.sum
//         |> float

//     let numberOfStreaks =
//         Array.map (MLTab.getStreak >> List.length) 
//         >> Array.filter (fun x -> x > 1) 
//         >> Array.map (fun x -> x * x)
//         >> Array.sum
//         >> float

//     let emptyTab tabs = 
//         tabs 
//         |> Array.filter (fun x -> Tableau.length x = 0) 
//         |> Array.map (fun x -> 1000)
//         |> Array.sum
//         |> float

//     let numberOfSuitsInTab tabs = 
//         tabs 
//         |> Array.map (Tableau.getVisible >> List.map Card.getSuit >> List.groupBy id >> List.length) 
//         |> Array.map (raiseToPower 2.) 
//         |> Array.map (fun x -> x * -1) 
//         |> Array.sum
//         |> float

//     let randomScore tabs = 
//         tabs
//         |> Array.mapi (fun i t -> t |> Tableau.getVisible |> List.map (fun x -> (Card.getValue x) * i) |> List.sum)
//         |> Array.sum
//         |> (fun x -> x |> float |> Math.Log)

//     let scoreSuit = function 
//     | Zero -> 0
//     | One -> (13 * 13) * 40
//     | Two -> (13 * 13) * 50
//     | Three -> (13 * 13) * 60
//     | Four -> (13 * 13) * 70
//     | Five -> (13 * 13) * 70
//     | Six -> (13 * 13) * 70
//     | Seven -> (13 * 13) * 70
//     | Eight -> (13 * 13) * 70

//     let suitCompletion game = 
//         [game.Hearts; game.Spades; game.Clubs; game.Diamonds] |> List.map scoreSuit |> List.sum |> float

//     let numberOfCards =
//         Array.map (Tableau.getVisible >> List.length) >> Array.map (fun x -> x * 1) >> Array.sum >> float

//     let combine scorers game = 
//         scorers |> List.map (fun f -> game |> Game.getAllTabs |> List.toArray |> f) |> List.sum


//     let stageFour = 
//         combine [sizeOfHiddenCards; randomScore] 

//     let stageOne = 
//         combine [numberOfStreaks; lengthOfRuns; numberOfRuns; hiddenCards; randomScore] 

//     let stageTwo game = 
//         combine [numberOfCards; lengthOfRuns; hiddenCards; randomScore; emptyTab; numberOfSuitsInTab] game

//     let stageThree game = 
//         suitCompletion game + combine [numberOfSuitsInTab; emptyTab; lengthOfRuns; hiddenCards; randomScore; ] game 

//     let playGame () = 
//         let rand = Random(1)

//         let values = [3;1]

//         let isAtRoot ns = 
//             List.length ns = List.length values //&& List.head ns = List.head values

//         let r, s = runMultistageNestedRolloutFast (hasTimerExpired (List.head values)) isAtRoot (Stopwatch.StartNew()) Game.emptyGame [stageFour; stageThree] values (GameMover.startGame (Card.deck Card.One) rand)
//         Game.toString s |> printfn "%s"
//         printfn "%A" r

//         //let (result, states, tree) = loop searchData (GameMover.startGame rand) Map.empty
//         //tree |> Option.iter SpiderTree.printTree


// //module Node = 

//     //let untriedActions node = 
//     //    ()

//     //let simulateAction node = 
//         //node

//     let performAction node = 
//         node

//     let rolloutActions node rolloutAction depth horizon = 
//         node, depth

//     let createNode problem state = 
//         ()

//     // This must walk up the tree (ie from the leaf) and update statistics 
//     let update discounting node = 
//         node // root

// type ActInfo<'a> = {
//     Action: 'a 
//     Reward: int
//     Visits: int
// }

// module ActInfo = 

//     let create action reward visits = 
//         {
//             Action = action
//             Reward = reward
//             Visits = visits
//         }


// module UCT = 
//     //// find a node with untried actions by recursing through the children
//     //let findNode node action nodeActions selectAction children depth horizon  = 

//     let findNode children selectAction simulateAction untriedActions horizon = 
//         let rec findNodeRecurse node depth = 
//             if node |> untriedActions |> List.isEmpty || node |> children |> List.isEmpty |> not || depth > horizon then 
//                 (node, selectAction node, depth)
//             else 
//                 findNodeRecurse (simulateAction node) (depth + 1)
//         findNodeRecurse

//     let expandNode expandAction isGoal performAction untriedActions  horizon node action depth = 
//         if untriedActions node |> List.isEmpty && depth <= horizon && not <| isGoal node then
//             let action = expandAction node  // use heuristics to pick one of the actions to try
//             let node = performAction node
//             (node, action, depth + 1)
//         else (node, action, depth)


    
//     let mcts untriedActions triedActions findNode expandNode rolloutActions update createNode rootState problem budget horizon rolloutAction selectBest root = 

//         let rec recurse iterations root = 
//             if iterations |> budget |> not then root
//             else 
//                 let node = root
//                 let depth = 1
//                 printfn "  step (1): selecting node"    

//                 let (node, action, depth) = findNode untriedActions horizon node depth

//                 printfn "  selected node with the state " //  + str(node.state))
//                 printfn"  step (2): expanding node on depth %d" depth 

//                 let (node, action, depth) = expandNode untriedActions horizon node action depth 

//                 printfn "  step (3): performing rollout"
//                 let (node, depth) = rolloutActions node rolloutAction depth horizon

//                 printfn "  ended up in the state " //  + str(node.state))
//                 let root = update node
//                 recurse (iterations + 1) root

//         let root = createNode problem rootState |> recurse 0

//         let actions = triedActions root |> List.map (fun (action, (reward, visits)) -> ActInfo.create action reward visits)

//         selectBest actions

// [<AutoOpen>]
// module GameState = 
//     type Message = 
//         | SetDelta of float 
//         | GetDelta of AsyncReplyChannel<float>
//         | GetUtility of (GameResult * AsyncReplyChannel<float>)
//         | SetUtility of (GameResult * float)
//         | GetAllUvalues of AsyncReplyChannel<Map<GameResult, float>>
//         | SetLookUpTable of GameResult * MoveType * GameResult
//         | GetLookUpTable of GameResult * MoveType * GameResult option AsyncReplyChannel
//         | SetBandit of GameResult * MoveType
//         | GetBandit of GameResult * MoveType * int AsyncReplyChannel
//         | AddGame of GameResult
//         | GetGames of GameResult list AsyncReplyChannel

//     let gameState () =
//         MailboxProcessor.Start(fun inbox ->
//             let rec loop (delta, uValues, lookUpTable, bandit, games) = 
//                 async {
//                     let! msg = inbox.Receive()
//                     match msg with 
//                     | SetDelta x -> 
//                         return! loop (x, uValues,lookUpTable, bandit, games)
//                     | GetDelta rc -> 
//                         rc.Reply delta
//                         return! loop (delta, uValues,lookUpTable, bandit, games)
//                     | GetUtility (g, rc) -> 
//                         Map.tryFind g uValues |> Option.defaultValue 10. |> rc.Reply
//                         return! loop (delta, uValues,lookUpTable, bandit, games)
//                     | SetUtility (g, value) -> 
//                         return! loop (delta, Map.add g value uValues, lookUpTable, bandit, games)
//                     | GetAllUvalues rc -> 
//                         rc.Reply uValues
//                         return! loop (delta, uValues, lookUpTable, bandit, games)
//                     | SetLookUpTable (g,m,result) -> 
//                         return! loop (delta, uValues, Map.add (g,m) result lookUpTable, bandit, games)
//                     | GetLookUpTable (g,m,rc) -> 
//                         Map.tryFind (g,m) lookUpTable |> rc.Reply
//                         return! loop (delta, uValues, lookUpTable, bandit, games)
//                     | SetBandit (g,m) -> 
//                         let playedCount = Map.tryFind (g,m) bandit |> Option.defaultValue 0
//                         return! loop (delta, uValues, lookUpTable, Map.add (g,m) (playedCount + 1) bandit, games)
//                     | GetBandit (g,m,rc) -> 
//                         Map.tryFind (g,m) bandit |> Option.defaultValue 0 |> rc.Reply
//                         return! loop (delta, uValues, lookUpTable, bandit, games)
//                     | AddGame g -> 
//                         let spades g = GameResult.fold Zero Zero (fun x _ -> x.Spades) g
//                         let games =
//                             if Set.forall (fun x -> spades x <> spades g) games then 
//                                 g |> GameResult.fold "" "" (fun g _ -> Game.toString g) |> printfn "%s"
//                                 printfn "Another run completed"
//                                 Set.add g games
//                             else games
//                         return! loop (delta, uValues, lookUpTable, bandit, games)
//                     | GetGames rc -> 
//                         games |> Set.toList |> rc.Reply
//                         return! loop (delta, uValues, lookUpTable, bandit, games)


//                 }
//             loop (0.1, Map.empty, Map.empty, Map.empty, Set.empty) )


// module Bellman = 

//     let gamma = 0.99
//     let epsilon = 0.001 
//     //let errorBandit = 0.001

//     let globalGameState = gameState() 

//     let getNextState (g,m) = 
//         globalGameState.PostAndReply (fun rc -> GetLookUpTable (g,m,rc)) |> function 
//         | Some x -> x
//         | None -> 
//             match g with 
//             | Won -> g
//             | Lost _ -> g
//             | Continue (game,_) -> 
//                 let s' = GameOperations.playMoveToMinima m game
//                 globalGameState.Post <| SetLookUpTable (g,m,s')
//                 s'

//     let getUtility state = 
//         globalGameState.PostAndReply (fun rc -> GetUtility (state, rc))

//     let updateUtitlity (state, utiltiy) = 
//         globalGameState.Post <| SetUtility (state, utiltiy)

//     let getBandit (g,m) = 
//         globalGameState.PostAndReply (fun rc -> GetBandit (g,m,rc))

//     let setBandit (g,m) = 
//         globalGameState.Post <| SetBandit (g,m)

//     let getGames () =
//         globalGameState.PostAndReply GetGames

//     let addGame g = 
//         globalGameState.Post <| AddGame g


//     type NextMove = Learning of float | Playing

//     let getNextBestUtilities random (state, history) = 
//         match state with 
//         | Won -> [None, state, 10000.]
//         | Lost s -> [None, state, -1000.]
//         | Continue (s, moves) -> 

//             let states' = 
//                 moves
//                 |> (GameOperations.filterLocalLoopMoves GameMover.playMove s)
//                 |> GameOperations.moveOrdering s
//                 |> List.map (fun a ->  
//                     let s' = getNextState (state, a)
//                     Some a, s', getUtility s')
//                 |> List.filter (fun (_,s',_) -> Set.contains s' history |> not)
//                 |> List.filter (fun (_,s',_) -> state <> s')
//                 |> (fun xs -> 
//                     if List.isEmpty xs then [(None, Lost s, -1000.)]
//                     else xs )

//             match random with 
//             | Learning errorBandit -> 
//                 let randomValue = rand.NextDouble()
//                 if  randomValue < errorBandit && List.contains Stock moves then
//                     let stock = getNextState (state, Stock)
//                     [Some Stock, stock, GameOperations.getReward stock]
//                 elif randomValue < errorBandit && List.length states' > 1 then 
//                     let bestMove = states' |> List.maxBy (fun (_,_,u) -> u)     
//                     let statesWithoutBestMove = states' |> List.filter (fun x -> x <> bestMove)
//                     let index = rand.Next(0, (List.length statesWithoutBestMove) - 1)
//                     List.skip index statesWithoutBestMove |> List.head |> List.singleton
//                 else 
//                     states' 
//             | Playing -> states'

//     let getNextBestUtility random (state, history) = 
//         (state, history) |> getNextBestUtilities random |> List.maxBy (fun (_,_,u) -> u)  

//     let computeUtility errorBandit (state, history) = 
//         let a, s', utility = getNextBestUtility (Learning errorBandit) (state,history)
//         let reward = GameOperations.getReward state
//         let discountedFutureUtility = gamma * utility
//         a, s', reward + discountedFutureUtility

//     let asyncTask (tDuration, errorBandit, deltaTimeLimit) log game = 
//         let deltaTimer = Stopwatch.StartNew()
//         let gs = gameState() 

//         let getDelta () = 
//             gs.PostAndReply GetDelta

//         let setDelta f = 
//             gs.Post <| SetDelta f

//         while (getDelta () > epsilon * (1. - gamma) / gamma && deltaTimer.Elapsed.TotalMinutes < deltaTimeLimit) do
//             setDelta 0.

//             let rec runValueUpdate (t: Stopwatch) (state, history) = 
//                 match state with 
//                 | Won -> ()
//                 | Lost _ -> ()
//                 | Continue (game, _) -> 
//                     //let errorBandit = 
//                         //if tDuration = 10. && game.Spades = Two then 
//                         //    0.1
//                         //else errorBandit

//                     let currentUtility = getUtility state
//                     let a, s', bestUtilityOfNextState = computeUtility errorBandit (state, history)
//                     let history = Set.add s' history

//                     updateUtitlity (state, bestUtilityOfNextState)
//                     let difference = Math.Abs (bestUtilityOfNextState - currentUtility)
//                     if (difference > getDelta ()) then 
//                         setDelta difference

//                     addGame s'
//                     if (t.Elapsed.TotalMinutes < tDuration) then runValueUpdate t (s', history)

//             runValueUpdate (Stopwatch.StartNew()) (game, Set.empty)
//             getDelta () |> sprintf "%f" |> log 

//     let valueIteration game = 
//         addGame game
//         let logger s = printfnWithColor ConsoleColor.DarkYellow s
//         [   //(1., 0.1, 5.); (5., 0.01, 10.); (10., 0.05, 10.); (10., 0.05, 10.); (10., 0.05, 10.); 
//             (50., 0.05, 50.);]  
//         |> List.iter (fun config -> 

//             let configExplor = (6., 0.3, 6.)
//             let tasks = getGames () 
//             printfn "Running %d" (List.length tasks)

//             [game]
//             |> PSeq.ofList
//             |> PSeq.map (fun x -> if x = game then x,config, logger  else x,configExplor, (fun _ -> ()))
//             |> PSeq.iter (fun (x, config, log) -> asyncTask config log x)
//             printfn "All tasks completed")

//         printfn "Running last convergence"
//         asyncTask (100., 0.001, 50.) logger game
        

//     let game = 
//         let rand = new System.Random(0)
//         (GameMover.startGame (Card.deck Card.One) rand)


//     let playGame () = 
//         let uValuesForGame = globalGameState.PostAndReply GetAllUvalues

//         let rec play n (g, history) u = 
//             match g with 
//             | Won -> printfn "Game Won"
//             | Lost _ -> printfn "Game Lost"
//             | Continue (s,ms) -> 
//                 printfn "%s" <| Game.toString s
//                 let (_, s',u') = getNextBestUtility Playing (g, history)
//                 let history = Set.add s' history
//                 if (n > 0 && g <> s') then play (n - 1) (s', history) u'

//         play 10000000 (game, Set.empty) 0.


// module PolicyIteration = 

//     type Message = 
//         | GetQuality of (Game * MoveType * float AsyncReplyChannel)
//         | SetQuality of (Game * MoveType * float)
//         | GetCount of (Game * MoveType * int AsyncReplyChannel)
//         | SetCount of (Game * MoveType)
//         | GetPolicy of AsyncReplyChannel<Map<Game * MoveType, float>>


//     let gameState = 
//         MailboxProcessor.Start(fun inbox -> 
        
//             let rec loop (qstar, exploration) = 
//                 async {

//                     let! msg = inbox.Receive()
//                     match msg with 
//                     | GetQuality (g,m,rc) -> 
//                         let defaultValue = if m = Stock then 50. else 0.
//                         qstar |> Map.tryFind (g,m) |> Option.defaultValue defaultValue |> rc.Reply
//                     | SetQuality (g,m,v) -> 
//                         let qstar = qstar |> Map.add (g,m) v
//                         return! loop (qstar, exploration)
//                     | GetCount (g,m,rc) -> 
//                         exploration |> Map.tryFind (g,m) |> Option.defaultValue 0 |> rc.Reply
//                     | SetCount (g,m) -> 
//                         let v = exploration |> Map.tryFind (g,m) |> Option.defaultValue 0
//                         let exploration = exploration |> Map.add (g,m) (v + 1)
//                         return! loop (qstar, exploration)
//                     | GetPolicy rc -> 
//                         rc.Reply qstar

//                     return! loop (qstar, exploration)
//                 }

//             loop (Map.empty, Map.empty))

//     let getPolicy game move = 
//         gameState.PostAndReply (fun rc -> GetQuality (game, move, rc))

//     let getCount game move = 
//         gameState.PostAndReply (fun rc -> GetCount (game, move, rc))

//     let setPolicy game move value = 
//         gameState.Post <| SetQuality (game,move,value)

//     let setCount game move = 
//         gameState.Post <| SetCount (game,move)

//     let getPolicyForall () = 
//         gameState.PostAndReply GetPolicy

//     let gamma = 0.99
//     let alpha = 0.5 // weighting to take of each new sample
//     let k = 2. // exploration constant 

//     let explorationF k u n = u  + (k / n)

//     let getBestPolicyForMoves kk game moves = 

//         let getBestPolicy game m = 
//             let q = getPolicy game m 
//             let n = getCount game m |> float
//             explorationF k q n

//         moves
//         |> List.map (fun m -> m, getBestPolicy game m)
//         |> List.maxBy (fun (m,v) -> v)


//     let computePolicyUpdate currentGame action newState game moves = 
//         let r = GameOperations.getReward newState
//         let (m',qStarS') = getBestPolicyForMoves 0. game moves

//         let value = alpha * r + gamma * qStarS'
//         setPolicy currentGame action value
//         setCount currentGame action

//         getBestPolicyForMoves 2. game moves |> fst
//         //if List.isEmpty history |> not then

//     let rec compute (state, history) action = 
//         printfn "%s" <| Game.toString state
//         printfn "Policy: %f" <| getPolicy state action
//         printfn "State Count: %d" <| getCount state action
//         // printfn "History Count: %d" <| List.length history
//         printfn "Computing values\n"
//         let s' = GameOperations.playMoveToMinima action state

//         let handleLost () = 
//             printfnWithColor ConsoleColor.Red "Game Lost --------- No more moves"
//             //history |> List.iter (fun (state, action, s', g, ms) -> 
//                 //computePolicyUpdate [] state action s' g ms |> ignore)
//             setPolicy state action (alpha * -1000.) 
//             setCount state action

//         match s' with 
//         | Won -> 
//             printfnWithColor ConsoleColor.Green "Game Won"
//             history |> List.iter (fun (state, action, s', g, ms) -> 
//                 computePolicyUpdate state action s' g ms |> ignore)
//             setPolicy state action (alpha * 10000.) 
//             setCount state action
//         | Lost _ -> handleLost()
//         | Continue (g,ms) -> 

//             // let hist = history |> List.map (fun (_,_,state,_,_) -> state)
//             let ms = GameOperations.cleanMoves [] g ms
//             if List.isEmpty ms || List.length history > 500 then handleLost () 
//             else 
//                 let m' = computePolicyUpdate state action s' g ms
//                 // let history = (state, action, s', g, ms) :: history
//                 // compute (g, [history]) m'
//                 compute (g, []) m'

//     let policyIteration game = 
//         GameResult.iter (game()) <| fun game moves -> 
//             let action = getBestPolicyForMoves 2. game moves |> fst
//             for _ in 1 .. 1000 do 
//                 compute (game, []) action

//     let game () = 
//         let rand = new System.Random(0)
//         (GameMover.startGame (Card.deck Card.One) rand)
//         |> GameResult.map (fun game moves-> game |> GameMover.unHideGame, moves)

//     let playGame () =
//         let rec play history game = 
//             if Set.contains game history then "Game Lost - same state"
//             else
//                 game |> GameResult.fold "Game Lost" "Game Won" (fun game moves -> 
//                     printfn "%s" <| Game.toString game
//                     printfn "Playing Game\n"
//                     let action = getBestPolicyForMoves 0. game moves |> fst
//                     let s' = GameOperations.playMoveToMinima action game 
//                     let history = Set.add s' history
//                     s' |> play history) 
//         play Set.empty (game())


// module QlearningWithFeatues = 

//     type Message = 
//         | GetQuality of (float list AsyncReplyChannel)
//         | SetQuality of (float list)
//         | GetCount of (Game * MoveType * int AsyncReplyChannel)
//         | SetCount of (Game * MoveType)


//     let gameState = 
//         MailboxProcessor.Start(fun inbox -> 
        
//             let rec loop (weights, exploration) = 
//                 async {

//                     let! msg = inbox.Receive()
//                     match msg with 
//                     | GetQuality (rc) -> 
//                         rc.Reply weights
//                         return! loop (weights, exploration)
//                     | SetQuality weights -> 
//                         return! loop (weights, exploration)
//                     | GetCount (g,m,rc) -> 
//                         exploration |> Map.tryFind (g,m) |> Option.defaultValue 0 |> rc.Reply
//                         return! loop (weights, exploration)
//                     | SetCount (g,m) -> 
//                         let v = exploration |> Map.tryFind (g,m) |> Option.defaultValue 0
//                         let exploration = exploration |> Map.add (g,m) (v + 1)
//                         return! loop (weights, exploration)
//                 }

//             loop (List.empty, Map.empty))

//     let getWeights () = 
//         gameState.PostAndReply GetQuality

//     let getCount game move = 
//         gameState.PostAndReply (fun rc -> GetCount (game, move, rc)) |> float

//     let setWeights weights = 
//         gameState.Post (SetQuality weights) 

//     let setCount game move = 
//         gameState.Post <| SetCount (game,move)


//     let stockFeature history game move = 
//         MoveType.fold 0.5 (fun _ -> 0.) (fun _ -> 0.) move

//     let noValueMove history game move = 
//         move |> MoveType.foldMove 0. (fun move -> 

//         let columnsWithOne = 
//             game
//             |> Game.getAllTabsWithColumn
//             |> List.map (fun (c,t) -> c, t |> Tableau.getVisible |> List.length)
//             |> List.filter (fun (_,t) -> t = 1)
//             |> List.map fst

//         let emptyColum = 
//             game
//             |> Game.getAllTabsWithColumn
//             |> List.map (fun (c,t) -> c, t |> Tableau.getVisible |> List.length)
//             |> List.filter (fun (_,t) -> t = 0)
//             |> List.map fst

//         if List.contains move.To emptyColum  && List.contains move.From columnsWithOne then 
//             1.
//         else 0. )

//     let canFlip history game move = 
//         move |> MoveType.fold 0. (fun _ -> 1.) (fun _ -> 0.)

//     let canGrowRun history  game move = 
//         move |> MoveType.foldMove 0. (fun move -> 
//             let toColum = Game.getTabForColumn game move.To
//             if Tableau.length toColum > 0 then 1. else 0. )

//     let canMoveToEmptyColumn history  game move = 
//         move |> MoveType.foldMove 0. (fun move -> 
//             let toColum = Game.getTabForColumn game move.To
//             if Tableau.length toColum = 0 then 1. else 0. )

//     let canAllowFlip history game move = 
//         move |> MoveType.foldMove 0. (fun move -> 
//             let toColumn = Game.getTabForColumn game move.From
//             let lengthOfCardsToplay = toColumn |> Tableau.getVisible |> List.takeWhile (fun c -> c <> move.Card) |> List.length
//             if lengthOfCardsToplay + 1 = (toColumn |> Tableau.getVisible |> List.length) && Tableau.hasHiddenCards toColumn 
//             then 1. 
//             else 0. )

//     let breakRunForLongerRun history  game move = 
//         move |> MoveType.foldMove 0. (fun move -> 
//             let toColumn = Game.getTabForColumn game move.To
//             let fromColumn = Game.getTabForColumn game move.To
//             let length = fromColumn |> Tableau.getVisible |> List.takeWhile (fun c -> c <> move.Card) |> List.length |> ((+) 1)
//             let toRunLength = Tableau.getRun toColumn |> List.length
//             let fromRunLength = Tableau.getRun fromColumn |> List.length
//             if toRunLength + length > fromRunLength && fromRunLength > 1 then 1. else 0. )

//     let shouldFlip history game move = 
//         let isFlip = move |> MoveType.fold false (fun _ -> true) (fun _ -> false)

//         if List.length game.Stock = 0 && isFlip 
//         then 1.
//         else 0.

//     let stockPlayedWhenGameAtMaximumState history game move = 

//         let validMoves = GameMover.validMoves game

//         let canPlayStock = validMoves |> List.contains Stock
//         let canPlayOtherMoves = List.length validMoves > 1
//         let remainingMovesAreNotMaximising = 
//             validMoves |> List.map (fun m -> breakRunForLongerRun history game move) |> List.forall (fun x -> x = 1.)

//         if canPlayStock && canPlayOtherMoves && remainingMovesAreNotMaximising then 1. else 0. 


//     let makingRandomChoice history game move = 
//         let validMoves = GameMover.validMoves game 

//         let toColumnMoves = 
//             validMoves |> List.choose (fun move -> 
//                 move |> MoveType.foldMove None (fun move -> 
//                     move.To |> Some
//             ))

//         if List.length toColumnMoves > 1 then 1. else 0. 


//     // Not required when playing with playToLocalMinima
//     let revealLongestColumn history game move = 
//         let lastMoveWasStock = 
//             match history with 
//             | g::xs -> 
//                 match g with 
//                 | Won -> false
//                 | Lost _ -> false
//                 | Continue (g,_) -> 
//                     let gameWithCards = 
//                         game 
//                         |> Game.getAllTabsWithColumn
//                         |> List.map (fun (c,t) -> 
//                             if t |> Tableau.getVisible |> List.length = 0 then c,t
//                             else c,{Visible = List.skip 1 t.Visible; Hidden = t.Hidden})
//                         |> (fun tabs -> 
//                             Game.updateTableaus tabs game)
//                         |> (fun g -> {g with Stock = g.Stock})
//                     gameWithCards = g
//             | _ -> false

//         let columnOfLongestRunBeforeStock () = 
//             game 
//             |> Game.getAllTabsWithColumn 
//             |> List.filter (fun (_,t) -> t |> Tableau.getVisible |> List.length > 1)
//             |> List.map (fun (c,t) -> 
//                 let cards = t |> Tableau.getVisible  |> List.skip 1 
//                 c, {Visible = cards; Hidden = []} |> Tableau.getRun |> List.length)
//             |> List.maxBy (fun (c,length) -> length)
//             |> fst

//         let moveIsForFrom () = 
//             move |> MoveType.foldMove false (fun move -> columnOfLongestRunBeforeStock () = move.From)

//         if lastMoveWasStock && moveIsForFrom () then 1. else 0. 

//     let growLongestColumn history game move = 
//         let longestRunColumn = 
//             game
//             |> Game.getAllTabsWithColumn 
//             |> List.map (fun (c,t) -> c, t |> Tableau.getRun |> List.length)
//             |> List.maxBy (fun (c,t) -> t)
//             |> fst

//         move |> MoveType.foldMove 0. (fun move -> if move.To = longestRunColumn then 1. else 0.)


//     let features = 
//         [
//             growLongestColumn
//             stockPlayedWhenGameAtMaximumState
//             breakRunForLongerRun
//             stockFeature
//             noValueMove 
//             canFlip
//             canGrowRun
//             canMoveToEmptyColumn
//             canAllowFlip
//             shouldFlip
//         ]

//     let gamma = 0.2
//     let alpha = 0.5 // weighting to take of each new sample
//     let k = 0.9 // exploration constant 

//     let explorationF k u n = u  + (k / n)

//     let getQValue weights history game move = 
//         List.zip weights features
//         //|> List.map (fun (w,f) -> printfn "W:%f F:%f" w (f game move); (w,f))
//         |> List.map (fun (w,f) -> w * f history game move)
//         |> List.sum

//     let getQvalueMaxOverAction k history game moves = 
//         let weights = getWeights()
//         let explore qValue move = 
//             if k = 0. then 
//                 qValue 
//             else 
//                 explorationF k qValue <| getCount game move 

//         //printfn "Finding best qValue"
//         moves
//         |> List.map (fun m -> m, getQValue weights history game m)
//         |> List.map (fun (m, qValue) -> m, explore qValue m)
//         //|> List.map (fun (m, qValue) -> printfn "%f" qValue; (m,qValue))
//         |> List.maxBy (fun (m,v) -> v)

//     let computeWeightsForFeatures weights difference history game move = 
//         List.zip weights features 
//         |> List.map (fun (w,f) -> 
//             let fValue = f history game move
//             let result = w  + (fValue * difference * alpha)
//             //printfn "w:%f f:%f reward:%f Result:%f" w fValue difference result
//             result
//             )

//     let computeDifference weights currentGame currentMove reward history game moves = 
//         let bestQValue = getQvalueMaxOverAction 0. history game moves |> snd
//         let currentQvalue = getQValue weights history currentGame currentMove
//         let x = reward + (gamma * bestQValue) - currentQvalue
//         //printfn "reward:%f bestQValue:%f getQvalue:%f difference:%f" reward bestQValue currentQvalue x
//         x

//     let rec compute (state, history) action = 
//         printfn "%s" <| Game.toString state
//         printfn "Computed weights:"
//         let weights = getWeights()  

//         weights |> List.map (sprintf "%f") |> (fun xs -> String.concat "," xs) |>  printfn "%s"
//         let s' = GameOperations.playMoveToMinima action state

//         let handleWonGame () = 
//             printfnWithColor ConsoleColor.Red "Game Won"
//             let diff = 1. - getQValue weights  history state action
//             computeWeightsForFeatures weights diff history state action |> setWeights
//             setCount state action

//         let handleLostGame () = 
//             printfnWithColor ConsoleColor.Red "Game Lost --------- No more moves"
//             let diff = -1. - getQValue weights history state action
//             computeWeightsForFeatures weights diff history state action |> setWeights
//             setCount state action

//         match s' with 
//         | Won -> handleWonGame ()
//         | Lost _ -> handleLostGame()
//         | Continue (g,ms) ->
//             let ms = GameOperations.cleanMoves history g ms
//             let r = GameOperations.getReward s'
//             printfn "Reward: %f" r
//             ms |> List.map App.printMove |> (fun xs -> String.concat "," xs) |> printfn "%s"
//             match ms with 
//             | [] -> handleLostGame ()
//             | _ -> 
//                 let difference = computeDifference weights state action r history g ms
//                 computeWeightsForFeatures weights difference history state action |> setWeights
//                 let m' = getQvalueMaxOverAction k history g ms |> fst
//                 let history = s' :: history
//                 compute (g, history) m'

//     let qFeatureLearning game = 
//         let weights = List.init features.Length (fun x -> 1.) 
//         weights |> List.map (sprintf "%f") |> (fun xs -> String.concat "," xs) |>  printfn "%s"
//         setWeights weights
//         printfn "Set Weights"
//         GameResult.iter (game()) <| fun game moves -> 
//             let action = getQvalueMaxOverAction k [] game moves |> fst
//             printfn "Playing first move!"
//             for _ in 1 .. 1000 do 
//                 compute (game, List.empty) action

//     let game () = 
//         let rand = new System.Random()
//         (GameMover.startGame (Card.deck OneSuit) rand)
//         |> GameResult.map (fun game moves-> game |> GameMover.unHideGame, moves)

//     let playGame () =
//         let rec play history game = 
//             if List.contains game history then "Game Lost - same state"
//             else
//                 game |> GameResult.fold "Game Lost" "Game Won" (fun game moves -> 
//                     printfn "%s" <| Game.toString game
//                     printfn "Playing Game\n"
//                     let action = getQvalueMaxOverAction 0. history game moves |> fst
//                     let s' = GameOperations.playMoveToMinima action game 
//                     let history =  s' :: history
//                     s' |> play history) 
//         play [] (game())


module MonteCarloTreeSearch =

    let ucb1 nodeScore totalNodesVisited currentNodeVisited = 
        ()


    // let serach game = 


