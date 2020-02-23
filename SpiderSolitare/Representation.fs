// #load "/Users/sam.williams/projects/tensorflow/FSharpTensorFlow/.paket/load/net472/main.group.fsx"
module SpiderSolitare.Representation
open System
open SpiderSolitare.Game
open Microsoft.ML
open System.IO
open System.Collections.Generic

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
        let data = File.ReadAllLines "/Users/willsam100/projects/gym/onehotAction.csv"
        
        data 
        |> Array.map (fun row -> 
            let columns = row.Split(',')
            let card = columns.[0]
            let values = columns.[1]
            card, values |> Seq.map (string >> Int16.Parse) |> Seq.toArray)
        |> Map.ofArray 
        

    let encodingToMove =
        moveToEncoding 
        |> Map.toList
        |> List.map (fun (a,b) -> 
            b |> Array.map string |> String.Concat, a)
        |> Map.ofList

    let encodeMove move = 

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
            moveToEncoding |> Map.find stringRep

    let decodeMove (encodedMove: string) = 
        let moveString = encodingToMove |> Map.find encodedMove

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
        // let data = File.ReadAllLines "/Users/willsam100/projects/gym/onehotCardKey.csv"
        let data = File.ReadAllLines "/Users/willsam100/Desktop/onehotCardKey.csv"
        
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
        try 
            let cardString = encodingToCard |> Map.find card
           
            match cardString with 
            | "-" -> None
            | _ -> 
                let suit = cardString.[0] |> string
                let value = cardString.Substring 1 |> int32
                match CardModule.create value (CardModule.parseSuit suit) with 
                | Some x -> Some x
                | None -> failwithf "Invalid encoding card: %s" cardString
        with 
        | e -> 
            printfn "%A" card
            encodingToCard |> Map.toList |> List.map (fst >> string) |> String.concat "," |> printfn "%s"
            raise e

    member __.Encode card = encode card
    member __.EmptyCard = ecodedEmtpyCard
    member __.DecodeCard card = decodeCard card

type CardEncoder() = 

    let cardToEncode = 
        printfn "Reading card encoding..."
        let data = File.ReadAllLines "/Users/willsam100/projects/gym/onehotCard.csv"

        data 
        |> Array.map (fun (row: string) -> 

            let columns = row.Split ','
            let card = columns.[0]
            let values = columns.[1]
            card, values |> Seq.map (string >> Int16.Parse) |> Seq.toList) 
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
        
        let cardString = 
            try encodingToCard |> Map.find card
            with 
            | :? KeyNotFoundException as e -> 
                printfn "%A" card
                raise e
       
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
        match tab.Visible with 
        | [] -> Array.replicate 34 cardEncoder.EmptyCard
        | _ -> 
            let tab = (tab.Visible |> List.toArray |> Array.map (cardEncoder.Encode)) //@ (tab.Hidden |> List.map (cardEncoder.Encode))
            let totalTableauSize = Array.replicate (34 - tab.Length) cardEncoder.EmptyCard
            Array.append tab totalTableauSize

    let stockEncoded = 
        let total = 50
        let encoded = game.Stock |> List.map cardEncoder.Encode
        encoded @ (List.replicate (total - encoded.Length) cardEncoder.EmptyCard)
        |> List.concat

    ((game |> Game.getAllTabs |> List.toArray |> Array.collect encodeTableau) |> List.concat) @ stockEncoded 

let encodeKeyGame (cardEncoder: CardEncoderKeyed) (game: Game) =
    let encodeTableau tab = 
        let tab = (tab.Visible |> List.map (cardEncoder.Encode)) //@ (tab.Hidden |> List.map (cardEncoder.Encode))
        let totalTableauSize = List.replicate (96 - tab.Length) cardEncoder.EmptyCard
        tab @ totalTableauSize // pad to be array of max possible length for a tableau

    let stock = 
        let stock = game.Stock |> List.map (cardEncoder.Encode >> int32)
        let emptyStock = List.replicate (50 - game.Stock.Length) cardEncoder.EmptyCard
        emptyStock @ stock // Put the empty stock first, so that the data does not change order as it is used. 

    (game |> Game.getAllTabs |> List.collect encodeTableau) @ stock

let decodeKeyedGame (cardEncoder:CardEncoderKeyed) (game: int32 list) = 

    let allCards = 
        game 
        |> List.chunkBySize (96)
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

    let game = 
        Game.updateTableaus tabs Game.emptyGame 
    {game with Stock = stock}


let decodeGame (cardEncoder:CardEncoder) (game: string) = 

    let maxTabSize = (34)
    // let maxTabSize = (1)
    let cardCount = 14
 
    let allCards = 
        game
        |> Seq.map (string >> Int16.Parse)
        |> Seq.toArray
        |> Array.chunkBySize cardCount
        |> Array.map (Array.toList >> cardEncoder.DecodeCard)
        |> Array.chunkBySize maxTabSize
        |> Array.map (Array.choose id)
        |> Array.mapi (fun i xs -> 
            let column = 
                if i + 1 > 10 then 
                    None
                else 
                    Coord.parseColumn (i + 1) |> Some
            column, xs)

    let stock = allCards |> Array.filter (fun (x,y) -> x = None) |> Array.collect snd |> Array.toList
    let tabs = 
        allCards 
        |> Array.choose (fun (x,y) -> x |> Option.map (fun z -> z,y))
        |> Array.map (fun (c,t) -> c, {Visible = t |> Array.toList; Hidden = []})
        |> Array.toList

    let game = Game.updateTableaus tabs Game.emptyGame 
    {game with Stock = stock}


// CardModule.create 10 S |> Option.get; CardModule.create 9 S |> Option.get] |> List.map (fun x -> ce.Encode x) |> List.concat |> List.map string |> String.Concat

