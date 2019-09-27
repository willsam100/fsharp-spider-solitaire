module SpiderSolitare.Main
// open SpiderSolitare.Solver
open SpiderSolitare.Game
open System
open SpiderSolitare.Representation

[<EntryPoint>]
let main argv = 
    printfn "Running bellman"
    //Bellman.valueIteration Bellman.game
    //BvalueIteration game

    // let actionEncoder = ActionEncoder()
    // let cardEncoder = CardEncoder()

    // let rand = Random()
    // let g = (GameMover.startGame (Card.deck Card.One) rand)

    // match g with 
    // | Continue (g, moves) -> 
    //     printfn "%A" g
    //     let encodedGame = encodeGame cardEncoder g 
    //     encodedGame |> List.map string |> String.Concat |> (fun x -> printfn "%d" x.Length)
    //     encodedGame |> decodeGame cardEncoder |> printfn "%A"

    //     // moves
    //     // |> List.map (fun move -> move, actionEncoder.Encode move)
    //     // |> List.iter (fun (move, encodedMove) -> 
    //     //     printfn "Move %A has been encoded" move
    //     //     let printableEncodedMove = encodedMove |> Array.map string |> String.Concat
    //     //     let move' = actionEncoder.Decode encodedMove

    //     //     printfn "Move %A, Move': %A" move move')
    //     actionEncoder.GetActionOutSize()
           

    // | Lost _ -> printfn ""
    // | Won -> printfn ""


    0