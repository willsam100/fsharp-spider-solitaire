namespace SpiderSolitare
open System
open System.Diagnostics
open System.Threading.Tasks
open Logary

module Tuple = 
    let apply f g (x,y) = 
        (f x, g y)

    let append f x =
        x, f x

module F = 
    let uncurry f (a,b) = f a b
    let curry f a b = f (a,b)
    let flip f x y = 
        f y x

module Option = 
    let apply f a = 
        match f, a with 
        | Some f, Some a -> Some <| f a
        | _ -> None

module List = 
    let sequenceOption list =
        let (<*>) = Option.apply
        let retn = Some

        let cons head tail = head :: tail
        let initState = retn List.empty
        
        let folder head tail = retn cons <*> (head) <*> tail
        List.foldBack folder list initState 

//module Guid = 
    //let TryParse s = 
        //match Guid.TryParse s with
        //| true, g -> Some g
        //| false, _ -> None


[<AutoOpen>]
module LogicUtils = 

    let rand = new System.Random()

    let swap (a: _[]) x y =
        let tmp = a.[x]
        a.[x] <- a.[y]
        a.[y] <- tmp

    let shuffle (rand: System.Random) a =
        Array.iteri (fun i _ -> swap a i (rand.Next(i, Array.length a))) a


    let shuffleList rand a = 
        let arr = List.toArray a
        shuffle rand arr
        arr |> Array.toList

    //let isZero x = x = 0
    
    //let rec distribute e = 
    //    function 
    //    | [] -> [ [ e ] ]
    //    | x :: xs' as xs -> 
    //        (e :: xs) :: [ for xs in distribute e xs' -> x :: xs ]
    
    //let rec permute = 
        //function 
        //| [] -> [ [] ]
        //| e :: xs -> List.collect (distribute e) (permute xs)

type Suit = H | D | S | C

type Card = 
    Card of (int * Suit)
    with 
        override this.ToString() = 
            this |> function 
            | Card (n, suit) -> 
                let number = 
                    match n with
                    | n when n = 1 -> "A"
                    | n when n = 11 -> "J"
                    | n when n = 12 -> "Q"
                    | n when n = 13 -> "K"
                    | n -> sprintf "%d" n
                sprintf "%s:%A" number suit


module Card = 

    let getDetails card = 
        let (Card(    (n), s)   ) = card
        (n, s)

    let getValue = getDetails >> fst
    let getSuit = getDetails >> snd

    let create n s = 
        match n with 
        | n when n <= 13 && n >= 1 -> 
            Some (Card((n), s))
        | _ -> None

    type Difficulty = One | Two | Four

    let deck numberOfSuits = 
        let suits = 
            match numberOfSuits with 
            | One -> [S; S; S; S]
            | Two -> [H; S; H; S]
            | Four -> [ H; D; S; C ]

        let numbers = [ 1..13 ]
        seq { 
            for s in suits do
                for n in numbers do
                    yield Card(n, s)
        }
        |> Seq.toList

    let canAddCardToRun bottomCard topCard = 
        let (nB, sB) = getDetails bottomCard
        let (nT, sT) = getDetails topCard
        sB = sT && nB = (nT + 1)

    let printCard (card: Card) = card.ToString()


type Tableau = { 
    Visible: Card list
    Hidden: Card list
}

module Tableau = 
    
    let firstCard tab = 
        match tab.Visible with 
        | [] -> None 
        | xs -> List.head xs |> Some


    let length tab = List.length tab.Visible + List.length tab.Hidden
    let empty: Tableau = {Visible = List.empty; Hidden = List.empty}

    let getVisible tab = 
        tab.Visible

    let create cards = 
        match cards with 
        | [] -> empty
        | x::xs -> 
            {Visible = [x]; Hidden = xs}

    let canAddCardToTab bottomCard topCard = 
        Card.getValue bottomCard = Card.getValue topCard + 1

    let isSameSuit xs = 
        xs |> List.map (Card.getSuit) |> List.distinct |> List.length = 1

    let isAscendingInValue xs = 
        let rec folder expectedValue values = 
            match values with
            | [] -> true
            | actualValue::values -> 
                let expectedValue = Option.defaultValue actualValue expectedValue
                if expectedValue = actualValue then 
                    folder (expectedValue + 1 |> Some) values
                else 
                    false
            
        xs |> List.map (Card.getValue) |> folder None 

    let validate rules run = 
        rules 
        |> List.map (fun f -> f run) 
        |> List.reduce (&&)

    let isCompleteRun run = 
        let isFullLength xs = 
            List.length xs = 13
        run |> validate [isFullLength; isSameSuit; isAscendingInValue;]

    let dropLastCard run = 
        (List.take (List.length run - 1)) run

    let validateRun tab = 
        let rec recurse run =
            if List.length run > 13 then 
                run |> dropLastCard |> recurse
            else 
                if isCompleteRun run then 
                    tab |> List.skip 13
                else tab
        recurse tab

    let canFlipHiddenCard tab = 
        List.isEmpty tab.Visible && List.isEmpty tab.Hidden |> not

    let flipHiddenCard tab = 
        match tab.Visible, tab.Hidden with 
        | [], [] -> tab
        | _, [] -> tab
        | _, xs -> 
            let card = List.head tab.Hidden
            {tab with Visible = [card]; Hidden = List.tail tab.Hidden}

    let canAddCard canAddTwoCards card tableau = 
        match tableau.Visible with 
        | [] -> List.isEmpty tableau.Hidden
        | bottomCard::xs -> canAddTwoCards bottomCard card

    let addCard canAddCard validateRun tableau card =
        match canAddCard tableau card with 
        | true -> {tableau with Visible = validateRun (card :: tableau.Visible) }
        | false -> tableau

    let getRun tableau = 

         //TODO:: consider perfomance
        let isValidRun run = run |> validate [isSameSuit; isAscendingInValue]

        let rec folder run = 
            match run, isValidRun run with 
            | [], true -> run
            | _, true -> run
            | [], false -> []
            | run, false -> run |> dropLastCard |> folder

        folder tableau.Visible 

    let addStockCard card tableau = 
        {tableau with Visible = validateRun (card :: tableau.Visible)}

    let moveCards cardsToMove tab =
        let rec removeCards visibleTab cards = 
            match visibleTab, cards with 
            | [], [] -> []
            | xs, [] -> xs
            | [], ys -> []
            | x::xs, y::ys when x = y -> removeCards xs ys
            | x::xs, y::ys -> xs

        {tab with Visible = removeCards tab.Visible cardsToMove}

type Column = 
    C1 | C2 | C3 | C4 | C5 | C6 | C7 | C8 | C9 | C10

type Coords = 
    { From : Column
      To : Column
      Card: Card }

module Coord = 
    let getColumnIndex = 
        function 
        | C1 -> 0
        | C2 -> 1
        | C3 -> 2
        | C4 -> 3
        | C5 -> 4
        | C6 -> 5
        | C7 -> 6
        | C8 -> 7
        | C9 -> 8
        | C10 -> 9

    let indexToColumName index = 
        match index with
        | 0 -> C1 |> Some
        | 1 -> C2 |> Some
        | 2 -> C3 |> Some
        | 3 -> C4 |> Some
        | 4 -> C5 |> Some
        | 5 -> C6 |> Some
        | 6 -> C7 |> Some
        | 7 -> C8 |> Some
        | 8 -> C9 |> Some
        | 9 -> C10 |> Some
        | _ -> None

    let allColumns = [ C1; C2; C3; C4; C5; C6; C7; C8; C9; C10 ]

type SuitCompletedStatus = 
    | Zero
    | One
    | Two
    | Three
    | Four

type Game = 
    { One: Tableau
      Two: Tableau
      Three: Tableau
      Four: Tableau
      Five: Tableau
      Six: Tableau
      Seven: Tableau
      Eight: Tableau
      Nine: Tableau
      Ten: Tableau
      Stock : Card list
      Hearts : SuitCompletedStatus
      Spades : SuitCompletedStatus
      Clubs : SuitCompletedStatus
      Diamonds : SuitCompletedStatus }

    with 
        override x.ToString() = 
            let stockSize = List.length x.Stock
            [x.One; x.Two; x.Three; x.Four; x.Five; x.Six; x.Seven; x.Eight; x.Nine; x.Ten]
            |> List.mapi (fun i tab -> 
                let visible = tab.Visible |> List.map Card.printCard
                let hidden = tab.Hidden |> List.map (fun _ -> "***")
                   
                (visible @ hidden)
                |> List.map (sprintf "%-5s")
                |> String.concat ""
                |> (fun s -> sprintf "%d::%s" (i + 1) s))
            |> String.concat "\n"
            |> (fun s -> sprintf "%s\nStock: %d, H = %A; D = %A, S = %A, C = %A" s stockSize x.Hearts x.Diamonds x.Spades x.Clubs)

module Game = 
    
    let maxCardsInTableau = 54

    let canAddCards = Tableau.canAddCard Tableau.canAddCardToTab
    let addCard = Tableau.addCard (fun x y -> canAddCards y x) Tableau.validateRun
    let addCards xs tab = 

        let result = 
            xs |> List.rev |> List.fold addCard tab

        match Tableau.length result = (List.length xs + Tableau.length tab) with 
        | true -> result, None
        | false -> result, xs |> List.head |> Card.getSuit |> Some 

    let emptyGame = 
        { Stock = List.empty
          One = Tableau.empty
          Two = Tableau.empty
          Three = Tableau.empty
          Four = Tableau.empty
          Five= Tableau.empty
          Six = Tableau.empty
          Seven = Tableau.empty
          Eight = Tableau.empty
          Nine = Tableau.empty
          Ten = Tableau.empty
          Hearts  = Zero
          Spades = Zero
          Clubs = Zero
          Diamonds = Zero }
    
    let completeSuit game suit = 
        let incrementSuit = 
            function 
            | Zero -> One
            | One -> Two
            | Two -> Three
            | Three -> Four
            | Four -> Four

        match suit with 
            | Suit.H -> game.Hearts |> incrementSuit
                             |> (fun suitCompleteStatus -> { game with Hearts = suitCompleteStatus })
            | Suit.C -> game.Clubs |> incrementSuit
                             |> (fun suitCompleteStatus -> { game with Clubs = suitCompleteStatus })
            | Suit.D -> game.Diamonds |> incrementSuit
                             |> (fun suitCompleteStatus -> { game with Diamonds = suitCompleteStatus })
            | Suit.S -> game.Spades |> incrementSuit
                             |> (fun suitCompleteStatus -> { game with Spades = suitCompleteStatus })

    let getTabForColumn game column = 
        match column with 
        | C1 -> game.One
        | C2 -> game.Two
        | C3 -> game.Three
        | C4 -> game.Four
        | C5 -> game.Five
        | C6 -> game.Six
        | C7 -> game.Seven
        | C8 -> game.Eight
        | C9 -> game.Nine
        | C10 -> game.Ten

    let updateColumn column tab game = 
        match column with 
        | C1 -> {game with One = tab}
        | C2 -> {game with Two = tab}
        | C3 -> {game with Three = tab}
        | C4 -> {game with Four = tab}
        | C5 -> {game with Five = tab}
        | C6 -> {game with Six = tab} 
        | C7 -> {game with Seven = tab} 
        | C8 -> {game with Eight = tab}
        | C9 -> {game with Nine = tab}
        | C10 -> {game with Ten = tab}
    
    let createGame rand deck = 

        let constructTableaus (game, cards) column = 
            match column with 
            | C1 -> {game with One = Tableau.create (List.take 6 cards) }, cards |> List.skip 6
            | C2 -> {game with Two = Tableau.create (List.take 6 cards) }, cards |> List.skip 6
            | C3 -> {game with Three = Tableau.create (List.take 6 cards) }, cards |> List.skip 6
            | C4 -> {game with Four = Tableau.create (List.take 6 cards) }, cards |> List.skip 6
            | C5 -> {game with Five = Tableau.create (List.take 5 cards) }, cards |> List.skip 5
            | C6 -> {game with Six = Tableau.create (List.take 5 cards) }, cards |> List.skip 5
            | C7 -> {game with Seven = Tableau.create (List.take 5 cards) }, cards |> List.skip 5
            | C8 -> {game with Eight = Tableau.create (List.take 5 cards) }, cards |> List.skip 5
            | C9 -> {game with Nine = Tableau.create (List.take 5 cards) }, cards |> List.skip 5
            | C10 -> {game with Ten = Tableau.create (List.take 5 cards) }, cards |> List.skip 5

        let constructStock (game, cards) = 
            {game with Stock = cards}

        let cards = 
            Seq.replicate 2 deck 
            |> Seq.concat 
            |> Seq.toList
            |> LogicUtils.shuffleList rand

        Coord.allColumns |> Seq.fold (constructTableaus) (emptyGame, cards) |> constructStock

    let getAllTabsWithColumn game = 
        Coord.allColumns |> List.map (fun x -> x, getTabForColumn game x)

    let getAllTabs game = 
        getAllTabsWithColumn game |> List.map snd

    let updateTableaus tabs game = 
        tabs |> List.fold (fun game (c, t) -> updateColumn c t game ) game

    let findCardToPlay from card game = 
        let fromRun =  from |> getTabForColumn game |> Tableau.getRun
        let cardToMoveIndex = fromRun |> List.tryFindIndex (fun x -> x = card)
        cardToMoveIndex |> Option.map (fun i -> fromRun |> List.take (i + 1))
            
    let playMove coord game =

        findCardToPlay coord.From coord.Card game
        |> Option.filter (fun cardsToMove -> canAddCards (List.last cardsToMove) (coord.To |> getTabForColumn game))
        |> Option.map (fun cardsToMove -> 
            let newTab, suitCompleted = coord.To |> getTabForColumn game |> addCards cardsToMove 
            let game = updateColumn coord.To newTab game 
            let newFromTab = coord.From |> getTabForColumn game |> Tableau.moveCards cardsToMove
            let game = updateColumn coord.From newFromTab game
            suitCompleted |> Option.map (completeSuit game) |> Option.defaultValue game )

    let playStock game = 
        match game.Stock with 
        | [] -> None
        | xs when List.length xs < 10 -> None
        | xs -> 
            let stock = game.Stock |> List.take 10

            let game = {game with Stock = List.skip 10 game.Stock }

            List.map2 (fun card (c, tab) -> 
                c, Tableau.addStockCard card tab ) stock (getAllTabsWithColumn game) 
            |> (F.flip updateTableaus game)
            |> Some

    let flip coord game = 
        let tab = coord |> getTabForColumn game 
        updateColumn coord (Tableau.flipHiddenCard tab) game

    let isComplete game = 
        match game.Hearts, game.Spades, game.Diamonds, game.Clubs with 
        | Two, Two, Two, Two -> true
        | _, Four, _, _ -> true
        | _ -> false   

    let toString game =
        game.ToString()

type MoveType = 
    | Stock
    | Flip of Column
    | Move of Coords

type GameResult = 
    | Continue of (Game * MoveType list)
    | Lost of Game
    | Won

module GameResult = 

    let lostOrContinue f game newGame = 
        match newGame with 
        | None -> Lost game
        | Some game -> f game             

module GameMover = 
    type CardComparison = 
        { Source : Card
          Target : Card }
    
    open Game
    
    let canPlayStock game = 
        let hasCardsToPlay = game.Stock |> Seq.length > 0
        let cardIsOnAllSlots = Game.getAllTabs game |> List.forall (fun x -> x |> Tableau.getVisible |> List.length > 0)

        if hasCardsToPlay && cardIsOnAllSlots then Some Stock else None

    let getRuns game = 
        game
        |> Game.getAllTabsWithColumn 
        |> List.map (fun (c, t) -> c, Tableau.getRun t)
        |> List.filter (fun (c,t) -> List.isEmpty t |> not)


    let getValidMovesForCard fromColumn card game = 
        let canAddCard = Tableau.canAddCard Tableau.canAddCardToTab

        Coord.allColumns 
        |> List.filter (fun c -> c <> fromColumn)
        |> List.map (game |> Game.getTabForColumn |> Tuple.append)
        |> List.map (Tuple.apply id (canAddCard card))
        |> List.filter snd
        |> List.map (fun (toColumn,_) -> {From = fromColumn; To = toColumn; Card = card})
        |> List.map Move


    let validMoves game = 

        let cardMoves = 
            game 
            |> Game.getAllTabsWithColumn
            |> List.map (Tuple.apply id Tableau.getRun)
            |> List.map (fun (c,run) -> run |> List.map (fun card -> getValidMovesForCard c card game))
            |> List.concat
            |> List.concat

        let flip = 
            game 
            |> Game.getAllTabsWithColumn
            |> List.map (Tuple.apply id Tableau.canFlipHiddenCard)
            |> List.filter snd
            |> List.map fst
            |> List.map Flip

        let stockMoves = game |> canPlayStock |> Option.toList

        flip @ cardMoves @ stockMoves

    let isComplete f game = 
        match Game.isComplete game with 
        | true -> Won
        | false -> f game

    let lostOrContine game = 
        match validMoves game with 
        | [] -> Lost game
        | moves -> Continue (game, moves)
    
    let startGame deck rand = 
        Game.createGame rand deck |> lostOrContine

    let playMove move game = 
        let toGameResult = isComplete lostOrContine
        let toGameRusultOption = GameResult.lostOrContinue lostOrContine game

        match move with 
        | Stock -> Game.playStock game |> toGameRusultOption
        | Flip column -> game |> Game.flip column |> toGameResult
        | Move coord -> game |> Game.playMove coord |> toGameRusultOption

module App = 
    open Card
    open Game
    open System

    type AppMove = 
        | GetMoves of AsyncReplyChannel<List<int * MoveType>>
        | PlayMove of int * AsyncReplyChannel<GameResult>
        | GetGame of AsyncReplyChannel<GameResult>
       

    let toStringGame game =
        game  
        |> Game.getAllTabs
        |> List.mapi (fun i tab -> 
            let visible = tab.Visible |> List.map printCard
            let hidden = tab.Hidden |> List.map (fun _ -> "*")
               
            (visible @ hidden)
            |> List.map (sprintf "%-5s")
            |> List.toSeq
            |> String.concat ""
            |> (fun s -> sprintf "%-2d::%s" (i + 1) s))

    let printMove = function 
    | Stock -> "Stock"
    | Flip column -> sprintf "Flip: %A" column
    | Move coord -> sprintf "%A -> %A: %A" coord.From coord.To coord.Card

    let printMoves moves = 
        moves |> List.mapi (fun i m -> sprintf "%s" (printMove m))

    let printGameResult x = printfn "PRINTING GAME"; x |> function 
        | Lost game -> sprintf "LOST GAME\n" + (toString game)
        | Won -> "GAME HAS BEEN WON"
        | Continue (game, moves) -> 
            let add s y = sprintf "%s\n%s" s y  
            toString game + "\n" + (moves |> printMoves |> List.reduce (add))

    let myAgent rand = 
        MailboxProcessor.Start(fun inbox -> 

            let rec loop gameResult = 
                async { 
                    let! msg = inbox.Receive()
                    match msg with
                    | GetGame rc -> 
                        gameResult |> rc.Reply
                        return! loop gameResult
                    | GetMoves rc -> 
                        match gameResult with 
                        | Lost g -> rc.Reply []
                        | Won -> rc.Reply []
                        | Continue (g, moves) -> moves |> List.indexed |> rc.Reply 
                        return! loop gameResult
                    | PlayMove(moveIndex, rc) -> 
                        match gameResult with 
                        | Continue (game,moves) -> 
                            let move = moves |> List.indexed |> List.tryFind (fun (x,y) -> x = moveIndex)
                            match move with 
                            | None -> 
                                rc.Reply gameResult
                                return! loop gameResult
                            | Some (_, move) -> 
                                let gameResult = GameMover.playMove move game
                                rc.Reply gameResult
                                return! loop gameResult
                        | _ -> 
                            rc.Reply gameResult
                            return! loop gameResult
                }

            loop (GameMover.startGame (Card.deck One) rand))
    
    let start (gameAgent: MailboxProcessor<AppMove>) = 
        gameAgent.PostAndReply GetGame |> printGameResult |> printfn "%s"

    let playMoveAtIndex (gameAgent: MailboxProcessor<AppMove>) indexMove = 
        printfn "Playing: %d" indexMove
        (fun rc -> PlayMove(indexMove, rc))
        |> gameAgent.PostAndReply

    let playAndPrint gameAgent indexMove = 
        playMoveAtIndex gameAgent indexMove
        |> printGameResult
        |> printfn "%s"

    let getGame (gameAgent: MailboxProcessor<AppMove>) = 
        gameAgent.PostAndReply GetGame 
        //|> function 
        //| Lost g -> Some g
        //| Won -> None
        //| Continue (game, _) -> Some game

module MLTab = 

    let hiddenLength tab = List.length tab.Hidden
    let getStreak tableau = 
        let isValidRun run = run |> Tableau.validate [ Tableau.isAscendingInValue]

        let rec folder run = 
            match run, isValidRun run with 
            | [], true -> run
            | _, true -> run
            | [], false -> []
            | run, false -> run |> (List.take (List.length run - 1)) |> folder

        folder tableau.Visible 

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
            if searchCompleted then localAccum else
                let finalAccum = subtrees |> List.fold recurse localAccum 
                finalAccum 

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
        if containsGame playedMove.Game tree then tree else 

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
                if node.Game = game 
                then Some <| InternalNode (node,trees), true
                else acc, false

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

    let toGameAndMoves games = 
        let folder acc = function 
            | m, Won -> acc
            | m, Lost _ -> acc
            | m, Continue x -> (m,x) :: acc
        Seq.fold folder [] games

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
            |> toGameAndMoves 
            |> List.map (fun (move,(game,moves)) -> {Move = move; Game = game; Moves = moves; Heuristic = heuristic game})
            |> Some
        | false -> None

    let playMoves playMove heuristic knownStates moves game gameWon evaulateAndContinue tree memoGames =
        let games = moves |> List.map (Tuple.append (playMove game))
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
            |> List.map (Tuple.append SpiderTree.findBestScore)
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

    let argMax f actions = 
        actions |> List.map (fun a -> a, f a) |> List.sortByDescending snd |> List.head |> fst

    let rec greedy inDeadEndOrWin argMax result (getA: Game -> MoveType list) (h: Game -> int) moves (s: Game): (int * MoveType list) = 
        let recurse = greedy inDeadEndOrWin argMax result getA h
        if inDeadEndOrWin s then (h s, moves) else 
           let a = s |> getA |> argMax (fun a -> result s a |> h) 
           let moves = a :: moves
           result s a 
           |> recurse moves

    let rec rollout inDeadEndOrWin argMax result getA h moves s: (int * MoveType list) = 
        let recurse = rollout inDeadEndOrWin argMax result getA h
        if inDeadEndOrWin s then h s, moves else 
            let greedy = greedy inDeadEndOrWin argMax result getA h []
            let a = s |> getA |> argMax (fun a -> result s a |> greedy |> fst )
            let moves = a :: moves
            result s a |> recurse moves

    let rec nestedRollout inDeadEndOrWin argMax result getA h moves n s: (int * MoveType list) = 
        let recurse = nestedRollout inDeadEndOrWin argMax result getA h
        if inDeadEndOrWin s then h s, moves else 
            if n = 0 then 
                let greedy = greedy inDeadEndOrWin argMax result getA h []
                let a = s |> getA |> argMax (fun a -> result s a |> greedy |> fst)
                let moves = a :: moves
                result s a |> recurse moves n
            else
                let a = s |> getA |> argMax (fun a -> result s a |> recurse [] (n-1) |> fst)
                let moves = a :: moves
                result s a |> recurse moves (n)


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

    let filterLocalLoopMoves playMove s ms = 
        let playMoves s ms =
                ms |> List.map (fun a -> a, playMove a s)

        let isWin = function
            | Won -> true
            | _ -> false
                   
        let hasWonGame = 
            List.filter (fun (a, x) -> isWin x)

        if List.isEmpty ms then List.empty else 
        let firstLevel =  playMoves s ms 

        let movesThatWinGame = 
            firstLevel |> hasWonGame |> List.map fst

        let secondLevel = 
            firstLevel 
            |> List.filter (fun (a, _) -> List.contains a movesThatWinGame |> not) 
            |> Search.toGameAndMoves 
            |> List.map (fun (a, (s, ms)) -> a, playMoves s ms)

        let movesThatWinGame = 
            movesThatWinGame @ (secondLevel |> List.filter (fun (a, ss) -> ss |> hasWonGame |> List.isEmpty |> not) |> List.map fst)

        let accpetedMoves = 
            secondLevel
            |> List.filter (fun (a, _) -> List.contains a movesThatWinGame |> not) 
            |> List.map (fun (a, ss) -> a, Search.toGameAndMoves ss |> List.map (snd >> fst))
            |> List.filter (fun (a, ss) -> List.forall (fun x -> x = s) ss |> not) 
            |> List.map fst

        movesThatWinGame @ accpetedMoves

    let moveOrdering s ms = 
        if List.isEmpty ms then List.empty else 

        let columnsWithOneCard = 
            s 
            |> Game.getAllTabsWithColumn 
            |> List.filter (fun (c,t) -> Tableau.getVisible t |> List.length = 1)
            |> List.map fst

        let columnsWithRun = 
            s 
            |> Game.getAllTabsWithColumn 
            |> List.filter (fun (c,t) -> Tableau.getRun t |> List.length > 1)
            |> List.map fst

        let columnsWithStreak = 
            s 
            |> Game.getAllTabsWithColumn 
            |> List.filter (fun (c,t) -> MLTab.getStreak t |> List.length > 1)
            |> List.map fst

        let applyToMove f s m  = 
            match m with 
            | Stock -> s
            | Flip _ -> s
            | Move m -> f s m

        let scoreFaceUp s m = 
            if List.contains m.From columnsWithOneCard then s + 5 else s

        let addToRun s m = 
            if List.contains m.To columnsWithRun && List.contains m.From columnsWithRun |> not then s + 4 else s 

        let addToRunOrStreak s m = 
            if List.contains m.To columnsWithRun && List.contains m.From columnsWithRun || List.contains m.To columnsWithStreak then s + 3 else s 

        let isFlip s m = 
            match m with 
            | Stock -> s
            | Flip _ -> 100
            | Move _ -> s

        //"Moves:" |> log
        //App.printableGame s |> log
        ms 
        |> List.map (fun m -> 0, m)
        |> List.map (fun (s,m) -> isFlip s m, m)
        |> List.map (fun (s,m) -> applyToMove scoreFaceUp s m, m)
        |> List.map (fun (s,m) -> applyToMove addToRun s m, m)
        |> List.map (fun (s,m) -> applyToMove addToRunOrStreak s m, m)
        |> List.sortByDescending (fst)
        //|> List.map (fun (v, m) -> m |> App.printMove |> sprintf "%d: %s" v |>  log; m)
        //|> (fun x -> printfn "---\n"; x)
        |> List.map snd

    let rec multistageNestedRolloutFast 
                (hasTimeExpired: int -> Stopwatch -> bool) 
                isDeadEnd 
                getFromCache 
                addToCache
                filterLocalLoop
                argMax 
                maxH 
                moveOrdering
                result 
                isAtRoot
                (timer: Stopwatch) 
                playedActions 
            (hs: ((Game -> float) list)) (ns: int list) (s:GameResult) =
            let recurse = multistageNestedRolloutFast hasTimeExpired isDeadEnd getFromCache addToCache filterLocalLoop argMax maxH moveOrdering result isAtRoot

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
                                        addToCache (filterLocalLoopMoves GameMover.playMove) argMax maxH moveOrdering playMove 

    let hiddenCards tabs = 
        (54 - (tabs |> Array.map MLTab.hiddenLength |> Array.sum)) * 2 |> float

    let raiseToPower pow = 
        (float >> F.flip (F.curry Math.Pow) pow >> int) 

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

    let stageOne = 
        combine [numberOfStreaks; lengthOfRuns; numberOfRuns; hiddenCards; randomScore] 

    let stageTwo game = 
        combine [numberOfCards; lengthOfRuns; hiddenCards; randomScore; emptyTab; numberOfSuitsInTab] game

    let stageThree game = 
        suitCompletion game + combine [numberOfSuitsInTab; emptyTab; lengthOfRuns; hiddenCards; randomScore; ] game 

    let playGame () = 
        let rand = Random(1)

        let values = [5;0;0]

        let isAtRoot ns = 
            List.length ns = List.length values //&& List.head ns = List.head values

        let r, s = runMultistageNestedRolloutFast (hasTimerExpired (List.head values)) isAtRoot (Stopwatch.StartNew()) Game.emptyGame [stageOne; stageTwo; stageThree] values (GameMover.startGame (Card.deck Card.One) rand)
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











