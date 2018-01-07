// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open SpiderSolitare.Solver
[<EntryPoint>]
let main argv = 
    //printfn "Running bellman"
    Bellman.valueIteration Bellman.game
    //BvalueIteration game
    0 // return an integer exit code
