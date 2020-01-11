module SpiderSolitare.MonteCarloTreeSearch

open SpiderSolitare.Representation
open System
open SpiderSolitare.Game
open Microsoft.ML
open System.IO
open System.Collections.Generic
// open StackExchange.Profiling
// open ExtCore.Collections
open FSharp.Collections.ParallelSeq

let rand = Random()
let cardEncoder = CardEncoderKeyed()

type IBransMover = 
    abstract member GetBestMove: Game -> MoveType list

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

type MutableNode(game:Game, move: MoveType, parent) = 
    member val N = 0 with get,set
    member val T = 0. with get, set
    member val Game = game
    member val Move = move
    member val Children: MutableNode list = [] with get,set
    member val Parent: MutableNode option = parent


let ucb1 total n nRoot = 
    if n = 0. then Double.MaxValue
    else total + (2. * Math.Sqrt(Math.Log(nRoot) / n) ) 

let nextNode nRoot (nodes: MutableNode list) = 
    let items = 
        nodes 
        |> List.map (fun x -> 
            x, ucb1 x.T (float x.N) nRoot )
        |> List.sortByDescending snd

    // items 
    // |> List.map snd 
    // |> List.map (fun x -> if x = Double.MaxValue then -10. else x ) 
    // |> List.map string
    // |> String.concat "," |> printfn "%s"
    
    items |> List.map fst

let isSimpleEnd g =

    let x = g |> Game.Game.getAllTabs |> List.map Tableau.length |> List.min
    x = 0

    // let x = g |> Game.Game.getAllTabs |> List.map (Game.Tableau.getRun >> List.length) |> List.max 
    // match x with 
    // | 6 -> true
    // | _  -> false

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
        | SuitCompletedStatus.Zero -> 0
        | SuitCompletedStatus.One -> 50
        | SuitCompletedStatus.Two -> 100
        | SuitCompletedStatus.Three -> 150
        | SuitCompletedStatus.Four -> 200
        | SuitCompletedStatus.Five -> 250
        | SuitCompletedStatus.Six -> 300
        | SuitCompletedStatus.Seven -> 350
        | SuitCompletedStatus.Eight -> 400

    float (x + suitCompleted) * 0.0025


let encodeGame game = 
    game |> encodeKeyGame cardEncoder |> Seq.map string |> String.concat ","


let getRandomMove pastGames game = 
    let allMoves = GameMover.validMoves game |> List.toArray
    Array.shuffle allMoves

    let isPlayedGame move = 
        match Game.GameMover.playMove move game with 
        | Game.Continue (g,_) -> pastGames |> Set.contains g    
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

let rec rollout pastGames count game = 

    match getRandomMove pastGames game with 
    | None -> 
        if reward game |> int = 1 then 1. else 0.
    | Some move -> 
        match Game.GameMover.playMove move game with 
        | Game.Continue (game,s) ->  
            if count <= 0 then 
                reward game
            else       
                let pastGames = Set.add game pastGames
                rollout pastGames (count - 1) game
        | Game.Lost _ ->
            0.
        | Game.Won -> 
            1.      

let expandNode (brainsMover: IBransMover) (pastGames: Set<Game>) (node: MutableNode) = 

    // let game = decodeKeyedGame cardEncoder (node.Game.Split (',') |> Array.map Int32.Parse |> Array.toList)
    let moves = GameMover.validMoves node.Game
    // let moves = brainsMover.GetBestMove node.Game

    moves 
    |> List.choose (fun move -> 
        match Game.GameMover.playMove move node.Game with 
        | Game.Continue (g,s) -> 
            match pastGames |> Set.contains g with 
            | true -> None
            | _ -> 
                // (g, {T = 0.; N = 0; Children = []; Game = g; Move = Some move}) |> Some
                (g, MutableNode(g, move, Some node)) |> Some
        | Game.Lost _ -> None
        | Game.Won -> None )

let updateNode node value = 
    { node with N = node.N + 1; T = node.T + value }

let mutateNode (node: MutableNode) value = 
    node.N <- node.N + 1
    node.T <- node.T + value
    //{ node with N = node.N + 1; T = node.T + value }    

let rec updateChildren nodes (node, value) =     
    let node = mutateNode node value
    match nodes with 
    | [] -> node
    | (n, t)::xs -> updateChildren xs (n, value)


let rec iteration (brainsMover: IBransMover) (pastGames: Set<Game.Game>) nodes: Set<Game.Game> * MutableNode = 

    let rec loop () = 
        let (node: MutableNode, _) = List.head nodes
        let nRoot = List.last nodes |> fst |> (fun x -> x.N) |> float
        match nextNode nRoot node.Children with 
        | [] -> 
            // use t = mp.Step "expand"
            let leaves = expandNode brainsMover pastGames node
            match leaves with 
            | [] -> 
                // let game = decodeKeyedGame cardEncoder (node.Game.Split (',') |> Array.map Int32.Parse |> Array.toList)
                let value = rollout pastGames 1 node.Game

                let parentNodes = List.tail nodes
                pastGames, updateChildren parentNodes (node, value)

            | _ -> 
                // leaves |> List.map (fst >> encodeGame) |> List.sort |> List.iter (fun x -> printfn "Adding: %d" <| x.GetHashCode())
                let pastGames = Set.union (leaves |> List.map fst |> Set.ofList) pastGames
                // let node = {node with Children = leaves |> List.map snd}
                node.Children <- leaves |> List.map snd
                let (_, tail) = List.head nodes
                let nodesTail = List.tail nodes
                iteration brainsMover pastGames ((node, tail) :: nodesTail)

        | n::tail -> 
            if n.N = 0 then 
                let value = rollout pastGames 3 node.Game
                let node = updateChildren nodes (n, value)
                pastGames, node

            else 
                let nodes = (n,tail) :: nodes
                loop ()
    loop nodes


let search brainsMover pastGames totalCount root =  
    let rec reSearch pastGames count node = 
        if count <= 0 then 
            // Console.SetCursorPosition (0, Console.CursorTop)
            // let space = (List.replicate (80) " " |> String.concat "")
            // printfn "%s    " space
            pastGames, node
        else 
            if count % 100 = 0 then 
                Console.SetCursorPosition (0, Console.CursorTop)
                let progress = 
                    let count = totalCount - count |> float
                    let progress = count / float totalCount
                    progress * 100.
                sprintf "%.2f%% P: %.2f%% T: %.2f  N: %d TC: %d" progress ((node.T / float node.N) * 100.) node.T node.N totalCount |> Console.Write

            let (pastGames, n) = iteration brainsMover pastGames [node, []]
            reSearch pastGames (count - 1) n

    reSearch pastGames totalCount root |> snd


