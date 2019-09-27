// #load "/Users/sam.williams/projects/tensorflow/FSharpTensorFlow/.paket/load/net472/main.group.fsx"
module SpiderSolitare.Representation
open System
open SpiderSolitare.Game
open Microsoft.ML
open System.IO

// module Spider = 
//     type Card = S = 0 | C = 1 | H = 2 | D= 3
    // let cardValue = [2 .. 10] @ [11; 12; 13; 14] 

// type DataPoint() =
//     member val Card: string = Unchecked.defaultof<string> with get, set

// type TransformedData() =
//     member val CardEncoded: UInt32 = Unchecked.defaultof<UInt32> with get, set

// let private modelSchema: DataViewSchema = null;
// let private mlContext = MLContext()
// let private trainedModel = mlContext.Model.Load("/Users/sam.williams/Desktop/spiderCardOneHot.zip", ref modelSchema);
// let private predictionEngine = mlContext.Model.CreatePredictionEngine<DataPoint, TransformedData>(trainedModel);

// module Move = 

type ActionEncoder() = 

    let moveToEncoding = 
        printfn "Reading encoding..."
        let data = File.ReadAllLines "/Users/sam.williams/Desktop/onehotAction.csv"
        
        data 
        |> Array.map (fun row -> 
            let columns = row.Split(',')
            let card = columns.[0]
            let values = columns.[1]
            card, values.ToCharArray() |> Array.map (string >> float32) )
        |> Map.ofArray 

    let encodingToMove =
        moveToEncoding 
        |> Map.toList
        |> List.map (fun (a,b) -> 
            b |> Array.map string |> String.Concat, a)
        |> Map.ofList

    let encodeMove move = 
        printfn "Encoding move: %A" move

        let getColumn c = 
            match c with 
            | C1 -> 1
            | C2 -> 2
            | C3 -> 3
            | C4 -> 4
            | C5 -> 5
            | C6 -> 6
            | C7 -> 7
            | C8 -> 8
            | C9 -> 9
            | C10 -> 10

        match move with 
        | Stock -> moveToEncoding |> Map.find "stock"
        | Flip c -> moveToEncoding |> Map.find (sprintf "flip%d" (getColumn c))
        | Move card -> 

            let suit = 
                match CardModule.getSuit card.Card  with 
                | H -> "H"
                | D -> "D"
                | S -> "S"
                | C -> "C"

            let value = CardModule.getValue card.Card
            let toColumn = getColumn card.To
            let fromColumn = getColumn card.From
            let stringRep = sprintf "%s%d-%d-%d" suit value toColumn fromColumn
            printfn "%s" stringRep
            moveToEncoding |> Map.find stringRep

    let decodeMove (encodedMove: float32 []) = 

        let s = encodedMove |> Array.map string |> String.Concat
        let moveString = 
            encodingToMove 
            |> Map.find s

        match moveString with 
        | "stock" -> Stock
        | x when x.Contains "flip" -> moveString.Replace("flip", "") |> int32 |> Coord.parseColumn |> Flip
        | _ -> 
            let moveString = moveString.Split '-'
            let suit = moveString.[0].[0] |> string |> CardModule.parseSuit
            let value = moveString.[0].Substring(1) |> int32

            let card = 
                match CardModule.create value suit with 
                | Some x -> x
                | None -> failwithf "Invalid card values: %A %d" suit value

            let toColumn = moveString.[1] |> int32 |> Coord.parseColumn
            let fromColumn = moveString.[2] |> int32 |> Coord.parseColumn
            Move {Coords.Card = card; To = toColumn; From = fromColumn}

    member __.Encode card = encodeMove card
    member __.Decode encodedMove = decodeMove encodedMove
    member __.GetActionOutSize () = 
        moveToEncoding |> Map.toList |> List.head |> snd |> Array.length |> printfn "%d"

type CardEncoderKeyed() = 

    let cardToEncode = 
        printfn "Reading card encoding..."
        let data = File.ReadAllLines "/Users/sam.williams/Desktop/onehotCardKey.csv"
        
        data 
        |> Array.map (fun (row: string) -> 

            let columns = row.Split ','
            let card = columns.[0]
            let values = columns.[1]
            card, values |> int32) 
        |> Map.ofArray 

    let encodingToCard =
        cardToEncode 
        |> Map.toList
        |> List.map (fun (a,b) -> b, a)
        |> Map.ofList

    let ecodedEmtpyCard = cardToEncode |> Map.find "-"

    let encode card = 
        let suit = 
            match CardModule.getSuit card  with 
            | H -> "H"
            | D -> "D"
            | S -> "S"
            | C -> "C"

        let value = CardModule.getValue card
        cardToEncode |> Map.find (sprintf "%s%d" suit value) 

    let decodeCard card = 
        let cardString = encodingToCard |> Map.find card
       
        match cardString with 
        | "-" -> None
        | _ -> 
            let suit = cardString.[0] |> string
            let value = cardString.Substring 1 |> int32
            match CardModule.create value (CardModule.parseSuit suit) with 
            | Some x -> Some x
            | None -> failwithf "Invalid encoding card: %s" cardString

    member __.Encode card = encode card
    member __.EmptyCard = ecodedEmtpyCard
    member __.DecodeCard card = decodeCard card

type CardEncoder() = 

    let cardToEncode = 
        printfn "Reading card encoding..."
        let data = File.ReadAllLines "/Users/sam.williams/Desktop/onehotCard.csv"
        
        data 
        |> Array.map (fun (row: string) -> 

            let columns = row.Split ','
            let card = columns.[0]
            let values = columns.[1]
            card, values |> Seq.map float32 |> Seq.toList) 
        |> Map.ofArray 

    let encodingToCard =
        cardToEncode 
        |> Map.toList
        |> List.map (fun (a,b) -> b, a)
        |> Map.ofList

    let ecodedEmtpyCard = cardToEncode |> Map.find "-"

    let encode card = 
        let suit = 
            match CardModule.getSuit card  with 
            | H -> "H"
            | D -> "D"
            | S -> "S"
            | C -> "C"

        let value = CardModule.getValue card
        cardToEncode |> Map.find (sprintf "%s%d" suit value) 

    let decodeCard card = 
        let cardString = encodingToCard |> Map.find card
       
        match cardString with 
        | "-" -> None
        | _ -> 
            let suit = cardString.[0] |> string
            let value = cardString.Substring 1 |> int32
            match CardModule.create value (CardModule.parseSuit suit) with 
            | Some x -> Some x
            | None -> failwithf "Invalid encoding card: %s" cardString

    member __.Encode card = encode card
    member __.EmptyCard = ecodedEmtpyCard
    member __.DecodeCard card = decodeCard card

let encodeGame (cardEncoder: CardEncoder) (game: Game) =
    let encodeTableau tab = 
        let tab = (tab.Visible |> List.toArray |> Array.map (cardEncoder.Encode)) //@ (tab.Hidden |> List.map (cardEncoder.Encode))
        let totalTableauSize = Array.replicate ((52 * 2) - tab.Length) cardEncoder.EmptyCard
        Array.append tab totalTableauSize

    (game |> Game.getAllTabs |> List.toArray |> Array.collect encodeTableau) |> List.concat |> List.toArray
    //  @ (game.Stock |> List.map cardEncoder.Encode) 

let encodeKeyGame (cardEncoder: CardEncoderKeyed) (game: Game) =
    let encodeTableau tab = 
        let tab = (tab.Visible |> List.map (cardEncoder.Encode)) //@ (tab.Hidden |> List.map (cardEncoder.Encode))
        let totalTableauSize = List.replicate ((52 * 2) - tab.Length) cardEncoder.EmptyCard
        tab @ totalTableauSize // pad to be array of max possible length for a tableau

    (game |> Game.getAllTabs |> List.collect encodeTableau) |> List.map float32 |> List.toArray
    //  @ (game.Stock |> List.map cardEncoder.Encode) 

let decodeGame (cardEncoder:CardEncoderKeyed) (game: int32 list) = 

    let allCards = 
        game 
        |> List.chunkBySize (52 * 2)
        |> List.map (fun xs -> xs |> List.choose cardEncoder.DecodeCard)
        |> List.mapi (fun i xs -> 
            let column = 
                if i + 1 > 10 then 
                    None
                else 
                    Coord.parseColumn (i + 1) |> Some
            column, xs)

    let stock = allCards |> List.filter (fun (x,y) -> x = None) |> List.collect snd
    let tabs = 
        allCards 
        |> List.choose (fun (x,y) -> x |> Option.map (fun z -> z,y))
        |> List.map (fun (c,t) -> c, {Visible = t; Hidden = []})

    let game = Game.updateTableaus tabs Game.emptyGame 
    {game with Stock = stock}


// let test int = 
//     predictionEngineReverse.Predict (TransformedData(CardEncoded = int))

// let cardValue = [2 .. 10] @ [11; 12; 13; 14] 
// let xAxis = [0 .. 9] |> List.mapi (fun i x -> i,x) |> Set.ofList

// type Tab = Tab of int
// type Rank = Rank of int

// let readRank (Rank i) = 
//     match i + 2 with // Rank begins at 0 offset
//     | 11 -> "J"
//     | 12 -> "Q"
//     | 13 -> "K"
//     | 14 -> "A"
//     | x when x < 15 && x >= 2 -> string x
//     | x  -> failwithf "Invalid card number: %d" x

// // let rankToInt r = 
// //     match r with 
// //     | "J" -> 11
// //     | "Q" -> 12
// //     | "K" -> 13
// //     | "A" -> 14
// //     | "2" -> 2 
// //     | "2" ->
// //     | "2" ->
// //     | "2" ->
// //     | "2" ->
// //     | "2" ->
// //     | "2" ->
// //     | "2" ->
    
// let readTabRank (t, r) = (t, readRank r)

// let getPickUpCard int = 
//     pickUpCard 
//     |> List.chunkBySize cardValue.Length
//     |> List.mapi (fun i x -> Tab i, x)
//     |> List.skip (int / cardValue.Length)
//     |> List.head
//     |> (fun (i, x) -> 
//         let r = x |> List.mapi (fun i _ -> Rank i) |> List.skip (int % cardValue.Length) |> List.head 
//         i, r)

// // last output should be (Tab 9, "A")
// // [0 .. 129] |> List.iter (getPickUpCard >> readTabRank >> (printfn "%A"));;

// let moveOneHot = List.replicate (cardValue.Length * xAxis * xAxis) 0   

// let getMove int = 
//     moveOneHot 
//     |> List.chunkBySize (cardValue.Length * xAxis)
//     |> List.mapi (fun i x -> 
//         Tab i, x |> List.chunkBySize xAxis ) 
//     |> List.skip (int / (cardValue.Length + xAxis))
//     |> List.head
//     |> (fun (i, x) -> 
//         // printfn "%d %d %d %d %d %d %d" int (int / (cardValue.Length + xAxis)) (int / cardValue.Length) (int / xAxis) (int % cardValue.Length) (int % xAxis) ((int / (cardValue.Length + xAxis)) / xAxis)
//         // printfn "%A" x
//         let rank, tabs = 
//             x 
//             |> List.mapi (fun i x -> Rank i, x) 
//             |> List.skip (int / xAxis) 
//             |> List.head
//         // printfn "%A %A" rank tabs        
//         let targetTab = tabs |> List.mapi (fun i _ -> Tab i) |> List.skip (int % xAxis) |> List.head
//         i, rank , targetTab)
//         // let (rank, tab) = x |> List.skip (int % 14) |> List.head
//         // let targetTab = tab |> List.skip (int % 10) |> List.head
//         // i, rank, targetTab)
// [0 .. 140] |> List.iter (getMove >> (printfn "%A"));;