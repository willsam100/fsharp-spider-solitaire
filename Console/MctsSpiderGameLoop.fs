module MctsSpiderGameLoop 
open System
open System.Collections.Generic
open SpiderSolitare
open System.Diagnostics
open SpiderSolitare.MonteCarloTreeSearch
open SpiderSolitare.Game
open SpiderSolitare.Visulization
open SpiderSolitare.Brain
open SpiderSolitare.Representation

let r = Random()

let pickBest log getMetrics (root: MutableNode<Game, 'b>) = 
    if root.Children.IsEmpty then 
        // This means that the NN suggested a move that had already been played ie a loop. 
        // It should be considered a loss since looping should be penalized. 
        None
    else 
        if log then  printfn "Playing best move"
        root.Children |> List.maxBy (getMetrics >> snd) |> Some

let rec pickRandom log count (root: MutableNode<Game, 'b>) = 

    if root.Children.IsEmpty then 
        // This means that the NN suggested a move that had already been played ie a loop. 
        // It should be considered a loss since looping should be penalized. 
        printfn "Error (or maybe). Why are we here, why was the network empty?   --- ---------------   \n%A" root
        None
    else                
        if count < 0 then 
            let move = r.Next(0, root.Children.Length)
            if log then printfn "Playing UNEXPLORED random move. %d/%d" (move + 1) (root.Children.Length)
            root.Children.[move] |> Some
        else 
            let move = r.Next(0, root.Children.Length)
            let nextNode = root.Children.[move]
            match nextNode.Children, nextNode.TerminalValue with 
            | [],  None -> pickRandom log (count - 1) root
            | _ -> 
                if log then printfn "Playing random move. %d/%d" (move + 1) (root.Children.Length)
                Some nextNode

let updateHistory parentGame game move history = 
    let gameAndBestMove = 
        (List.length history + 1), 
        (parentGame  |> Representation.encodeKeyGame 26 cardEncoder |> Seq.map string |> String.concat ","), 
        (move |> MoveEncoding.moveToInt),
        (game |> Representation.encodeKeyGame 26 cardEncoder |> Seq.map string |> String.concat ",")
    gameAndBestMove :: history

type GameResult = {
    IsWin: bool
    GameNumber: int
    Game: Game
    MovesMade: float
    History: (int * string * int * string) list
    Progress: float list
}


let handlesNodes playBestFromGraph log gameNumber game (searcher:ISearcher) winningNodes = 
    match winningNodes with 
    | [] -> 
        printfn "Search failed for %d. Playing best effort from graph!" gameNumber
        playBestFromGraph ()
    | winningNodes -> 
        let (history, _) = 
            winningNodes 
            |> List.fold (fun (history, game) (nMove: MutableNode<Game, MoveType>) -> 
                if nMove.Move = None then   
                    failwithf "WTF: %A" nMove
                updateHistory game nMove.Game nMove.Move.Value history, nMove.Game) ([], game)

        if log then 
            winningNodes |> List.iter (fun nMove -> 
                if log then 
                    printfn "%A" nMove.Game                        
                    printfn "%A" nMove.Move.Value            
            )

        {
            IsWin = true
            GameNumber = gameNumber
            Game = winningNodes |> List.last |> fun x -> x.Game
            MovesMade = history.Length |> float
            History = history
            Progress = searcher.GetProgress()
        }

type GraphMove<'a> = PickBest of 'a | GR of GameResult


let rec playBestFromGraph log (searcher:ISearcher) gameNumber randomMoveThreshold totalCount count history (node:MutableNode<Game, MoveType>) game =
    let playedRandom, playMove = 
        match r.NextDouble() < randomMoveThreshold with
        | true -> false, pickBest log searcher.GetMetrics
        | false -> 
            true, pickRandom log 5

    let makeGameResult isWin history game = 
        {
            IsWin = isWin
            GameNumber = gameNumber
            Game = game
            MovesMade = List.length history |> float
            History = history
            Progress = searcher.GetProgress()
        }

    match playMove node with 
    | None ->   
        // we don't actually know why we are here. Some how we coudln't see that this was a dead state
        // Most likely this is the cause of a loop - we have already visited this state. 
        // printfn "Finishing without a good reason. %A" playedRandom
        // false, gameNumber, game, movesMade, history, searcher.GetProgress() // TODO: keep reviing this
        // searcher.PrintTree 4 root
        makeGameResult false history game

    | Some nMove -> 
        nMove.Parent <- None
        match nMove.TerminalValue with 
        | None ->
            if count <= 0 then 
                let history = updateHistory game nMove.Game nMove.Move.Value history
                makeGameResult false history nMove.Game
            else    
                if log then 
                    printfn "%A" nMove.Game                        
                    printfn "%A" nMove.Move.Value

                let history = updateHistory game nMove.Game nMove.Move.Value history

                if playedRandom then
                    if log then printfn "Played random, fast searching again..."
                    let winningNodes = searcher.Search nMove
                    let playBestFromGraph () = playBestFromGraph log searcher gameNumber randomMoveThreshold totalCount (count - 1) history nMove nMove.Game
                    handlesNodes playBestFromGraph log gameNumber game searcher winningNodes
                else        
                    if log then printfn "Played best move from graph"
                    playBestFromGraph log searcher gameNumber randomMoveThreshold totalCount (count - 1) history nMove nMove.Game

        | Some isWin -> 
            let history = updateHistory game nMove.Game nMove.Move.Value history
            makeGameResult isWin history nMove.Game


let playGame log randomMoveThreshold (searcher: ISearcher) totalCount game gameNumber: GameResult = 

    let root = MutableNode(game, 1.0, 0., None, 1, None, None, 0)
    searcher.Init root
    if log then 
        printfn "Starting game:"
        printfn "%A" game
        printfn ""
    
    let winningNodes = searcher.Search root

    let playBestFromGraph () = playBestFromGraph log searcher gameNumber randomMoveThreshold totalCount totalCount [] root game
    handlesNodes playBestFromGraph log gameNumber game searcher winningNodes