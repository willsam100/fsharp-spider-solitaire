// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open SpiderSolitare.Solver
[<EntryPoint>]
let main argv = 
    //printfn "Running bellman"
    // Bellman.valueIteration Bellman.game
    //BvalueIteration game

    printfn "Running MCTS"


    let r = Random(43)
    let deck = Game.CardModule.deck Game.OneSuit
    let game = Game.GameMover.createValidGame deck r |> Game.GameMover.unHideGame


    let n = MonteCarloTreeSearch.treeSearch game
    let n = n.Children |> List.maxBy (fun x -> x.T)
    printfn "%A %A %A" n.T n.N n.Move
    printfn "%A" game


    0 // return an integer exit code
