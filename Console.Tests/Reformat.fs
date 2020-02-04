module Reformat.Tests

open System
open Xunit
open Swensen.Unquote.Assertions

let genUniqueMoves () = 


[<Fact>]
let ``My test`` () =
    
    let input = [|
        {
            Game = "12344"
            Move = "a"
            MoveOrder = 0
            GameNumber = 0
            RowNumber = 0
            MoveCount = 30
        }
    |]
    
    

    test <@ 

        input 
        |> Reformat.filterDuplicateGamesStateMoves
        |> Array.length 
            = 42

        
    @>
