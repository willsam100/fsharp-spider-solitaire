module Common
open SpiderSolitare.Representation
open System

let moveEncoder = ActionEncoder()
let gameDecoder = CardEncoderKeyed()

// let format (game:string) = 
//     let game = game.Split ","
//     let tableau = game |> Array.take (96 * 10) |> String.concat ","
//     let stock = game |> Array.skip (96 * 10) |> Array.truncate 10 |> String.concat ","
//     sprintf "%s,%s" tableau stock


// let gameDecoded game = 
//     game
//     |> format
//     |> fun x -> x.Split ","
//     |> Array.map Int32.Parse 
//     |> Array.toList
//     |> decodeKeyedGame gameDecoder