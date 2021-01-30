// #load "/Users/sam.williams/projects/tensorflow/FSharpTensorFlow/.paket/load/net472/main.group.fsx"
module SpiderSolitare.Representation
open SpiderSolitare.Game
open Microsoft.ML
open System
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

module OneHot = 

    let makeEmpty size = List.replicate size 0
         
    let toOneHot size index = 
        makeEmpty size |> List.mapi (fun i _ -> if i = index then 1 else 0)

    let toString (l: int list) = 
        if l.Length = 0 then
            failwithf "Bad input: %A" l

        l
        |> List.map string
        |> String.concat "," 

    let oneHotString size index = 
        toOneHot size index |> toString

    let logicalOr xs = 
        xs 
        |> List.map List.length 
        |> List.pairwise
        |> List.fold (fun acc (x,y) -> acc && (x = y)) true
        |> fun areSameLength -> 
            if not areSameLength then 
                failwithf "Coding error: %A" xs

        let indexs = xs |> List.map (List.findIndex (fun x -> x = 1))
        let empty = makeEmpty (xs.[0].Length)

        empty 
        |> List.mapi (fun i x -> if indexs |> List.contains i then 1 else x)
        |> toString

    let toInt = List.findIndex (fun x -> x = 1)

    let toInts l = 
        l
        |> List.mapi (fun i x -> i, x)
        |> List.filter (fun (x,y) -> y = 1)
        |> List.map fst

         

module CardEncoding = 

    let toInt (card:Card) = 
        let s,value = CardModule.getSuit card, CardModule.getValue card
        let suitValue =  
            match s with 
            | S -> 13 * 0
            | C -> 13 * 1
            | D -> 13 * 2
            | H -> 13 * 3
        suitValue + (value - 1)

    let toOneHot suitCount (card:Card) = 
        let count = suitCount * 13
        let oneHot = toInt card
        let empty = List.replicate count 0
        empty |> List.mapi (fun i x -> if i = oneHot then 1 else 0)

    let toCard (card:int list) = 
        let index = card |> List.findIndex (fun x -> x = 1)

        let suit = 
            match index / 13 with 
            | 0 -> S
            | 1 -> C
            | 2 -> D
            | 3 -> H
            | x -> failwithf "Bad input decoding card:%d " x

        let value = index - (index / 13 * 13) + 1
        Card(value, suit)

    let toOptionOneHot suitCount (card:Card option) = 
        let count = suitCount * 13
        let empty = List.replicate (count + 1) 0

        match card with 
        | None -> 
            empty |> List.mapi (fun i x -> if i = empty.Length - 1 then 1 else 0)
        | Some card -> 
            let s,value = CardModule.getSuit card, CardModule.getValue card
            let suitValue =  
                match s with 
                | S -> 13 * 0
                | C -> 13 * 1
                | D -> 13 * 2
                | H -> 13 * 3

            let oneHot = suitValue + (value - 1)
            empty |> List.mapi (fun i x -> if i = oneHot then 1 else 0)

    let toOptionCard (card:int list) = 

        if card.Length <> 14 then 
            failwithf "Bad input length: %A" card

        if List.last card = 1  then   
            None 
        else
            try 
                let index = card |> List.findIndex (fun x -> x = 1)
                let suit = 
                    match index / 13 with 
                    | 0 -> S
                    | 1 -> C
                    | 2 -> D
                    | 3 -> H
                    | x -> failwithf "Bad input decoding card:%d " x

                let value = index - (index / 13 * 13) + 1
                Card(value, suit) |> Some
            with 
            | e -> Exception((sprintf "%A" card), e) |> raise

type ActionEncoder() = 

    let moveToEncoding = 
        printfn "Reading encoding..."
        let data = File.ReadAllLines "../onehotAction.csv"
        
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

    member __.Encode card = 
        try 
            encodeMove card
        with 
        | e -> Exception(sprintf "Move:%A" card, e) |> raise
    member __.Decode encodedMove = decodeMove encodedMove
    member __.GetActionOutSize () = 
        moveToEncoding |> Map.toList |> List.head |> snd |> Array.length |> printfn "%d"

type CardEncoderKeyed() = 

    let cardToEncode = 
        printfn "Reading card encoding..."
        // let data = File.ReadAllLines "/Users/willsam100/projects/gym/onehotCardKey.csv"
        let data = File.ReadAllLines "../onehotCardKey.csv"
        
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

    member __.Encode card = 
        try encode card with | e -> Exception(sprintf "Encode error: %A" card, e) |> raise
    member __.EmptyCard = ecodedEmtpyCard
    member __.DecodeCard card = decodeCard card

type CardEncoder() = 

    let cardToEncode = 
        printfn "Reading card encoding..."
        let data = File.ReadAllLines "../onehotCard.csv"

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

let encodeKeyGame tabsize (cardEncoder: CardEncoderKeyed) (game: Game) =
    let encodeTableau tab = 
        let tab = (tab.Visible |> List.map (cardEncoder.Encode)) |> List.truncate tabsize //@ (tab.Hidden |> List.map (cardEncoder.Encode))
        let totalTableauSize = List.replicate tabsize cardEncoder.EmptyCard
        (tab @ totalTableauSize) |> List.truncate tabsize // pad to be array of max possible length for a tableau

    let stock = 
        let stock = game.Stock |> List.map (cardEncoder.Encode >> int32) |> List.truncate tabsize
        let maxStockSize = Math.Min(50, tabsize)
        let emptyStock = 
            if maxStockSize - game.Stock.Length <= 0 then []
            else List.replicate (maxStockSize - game.Stock.Length) cardEncoder.EmptyCard
        stock @ emptyStock |> List.truncate tabsize

    (game |> Game.getAllTabs |> List.collect encodeTableau) @ stock

let encodeOneHotGame tabsize suitCount (game: Game) =
    let encodeTableau tab = 
        let tab = (tab.Visible |> List.map (Some >> CardEncoding.toOptionOneHot suitCount)) |> List.truncate tabsize //@ (tab.Hidden |> List.map (cardEncoder.Encode))
        let totalTableauSize = List.replicate tabsize (CardEncoding.toOptionOneHot suitCount None)
        (tab @ totalTableauSize) |> List.truncate tabsize // pad to be array of max possible length for a tableau

    let stock = 
        let stock = game.Stock |> List.map (Some >> CardEncoding.toOptionOneHot suitCount) |> List.truncate tabsize
        let maxStockSize = Math.Min(50, tabsize)
        let emptyStock = 
            if maxStockSize - game.Stock.Length <= 0 then []
            else List.replicate (maxStockSize - game.Stock.Length) (CardEncoding.toOptionOneHot suitCount None)
        stock @ emptyStock |> List.truncate tabsize
    (game |> Game.getAllTabs |> List.collect encodeTableau) @ stock |> List.concat

let decodeKeyedGame size (cardEncoder:CardEncoderKeyed) (game: int32 list) = 

    let allCards = 
        game 
        |> List.chunkBySize size
        |> List.map (fun xs -> xs |> List.choose cardEncoder.DecodeCard)
        |> List.mapi (fun i xs -> 
            let column = 
                if i + 1 > 10 then 
                    None
                else 
                    Coord.parseColumn (i + 1) |> Some
            column, xs)

    if List.length allCards <> 10 && List.length allCards <> 11 then 
        failwithf "Column count was off: %d, card count:%d,tab:%d\n %A" allCards.Length game.Length size game

    let stock = allCards |> List.skip 10 |> List.collect snd // |> List.filter (fun (x,y) -> x = None) |> List.collect snd
    let tabs = 
        allCards 
        |> List.choose (fun (x,y) -> x |> Option.map (fun z -> z,y))
        |> List.map (fun (c,t) -> c, {Visible = t; Hidden = []})

    let game = 
        Game.updateTableaus tabs Game.emptyGame 
    {game with Stock = stock}

let decodeOneHotGame size suitCount (game: int32 list) = 

    let cardEncodingCount = (suitCount * 13 + 1)

    let allCards = 
        game 
        |> List.chunkBySize (size * cardEncodingCount)
        |> List.map (List.chunkBySize cardEncodingCount)
        |> List.map (fun xs -> xs |> List.choose CardEncoding.toOptionCard)
        |> List.mapi (fun i xs -> 
            let column = 
                if i + 1 > 10 then 
                    None
                else 
                    Coord.parseColumn (i + 1) |> Some
            column, xs)

    if List.length allCards <> 10 && List.length allCards <> 11 then 
        failwithf "Column count was off: %d, card count:%d,tab:%d\n %A" allCards.Length game.Length size game

    let stock = allCards |> List.skip 10 |> List.collect snd // |> List.filter (fun (x,y) -> x = None) |> List.collect snd
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

// let encodeMove move = 
//     match move with 
//     | Stock -> 0
//     | Flip _ -> 1
//     | Move c -> 
//         let from = c.From |> Coord.toInt |> fun x -> x - 1
//         let toColumn = c.To |> Coord.toInt |> fun x -> x - 1
//         from * 10 + (toColumn + 2)

// allMoves |> List.map (fun x -> x = (x |> encodeMove |> decodeMove) )

let moveEncoder = ActionEncoder()
let cardEncoder = CardEncoderKeyed()

let transformGame game left right = 
    let tabLeft = Game.getTabForColumn game left
    let tabRight = Game.getTabForColumn game right
    Game.updateColumn left tabRight game
    |> Game.updateColumn right tabLeft

let transformMove move left right = 
    match move with
    | Move c ->
        match c.From, c.To with
        | f, t when f = left && t = right -> { c with From = right; To = left } |> Move
        | f, t when f = right && t = left -> { c with From = left; To = right } |> Move
        | f, _ when f = left -> { c with From = right; } |> Move
        | f, _ when f = right -> { c with From = left; } |> Move
        | _, t when t = left -> { c with To = right; } |> Move
        | _, t when t = right -> { c with To = left; } |> Move
        | _, _ -> c |> Move
    | x -> x

let allTransforms =
    (C1, C1) :: // prepend identify to keep the original in the output
        (Coord.allColumns
            |> List.collect (fun x -> Coord.allColumns |> List.map (fun y -> x,y) )
            |> List.filter (fun (x,y) -> x <> y) )

let applyTransform t =     
    allTransforms
    |> List.map (fun (left, right) -> t left right )
    |> List.distinct

module MoveEncoding = 

    let encodeMove (m: int16[]) = 
        let move = int16 1
        m |> Array.findIndex (fun x -> x = move)

    let decodeMove int = 
        let x = Array.replicate 1171 0
        x.[int] <- 1
        x

    let intToMove move = 
        move
        |> decodeMove 
        |> Array.map string 
        |> String.concat ""
        |> moveEncoder.Decode

    let moveToInt move = 
        move |> moveEncoder.Encode |> encodeMove

    let getColumn probs = 
        probs
        |> List.mapi(fun i f -> 
            if i = 0 then 
                None
            else 
                let c = Coord.parseColumn i
                Some (c, f) )
        |> List.choose id

    let getTargetCardForCoord game m = 
        let c, tab = 
            game
            |> Game.getAllTabsWithColumn
            |> List.find (fun (c,_) -> c = m.To)
        c, Tableau.firstCard tab    

    let getTargetCardForMove game move = 
        move |> MoveType.foldMove None (getTargetCardForCoord game >> Some)    

    let oneHotAllMoves (game:Game) = 
        let allMoves = List.replicate 1171 0

        GameMover.validMoves game
        |> List.map moveEncoder.Encode
        |> List.map Array.toList
        |> List.map (fun oneHotMove -> oneHotMove |> List.findIndex (fun x -> x = (int16 1)) )
        |> List.fold (fun oneHotAllMoves moveIndex -> 
            oneHotAllMoves |> List.mapi (fun i x -> if i = moveIndex then 1 else x)
            ) allMoves
        |> List.map string
        |> String.concat ","

    let oneHotToMoves (moves: int list) =         
        moves 
        |> List.mapi (fun i x -> i,x) 
        |> List.filter (fun (_,x) -> x = 1) 
        |> List.map fst
        |> List.map intToMove

      
    let getCard col count game = 
        game 
        |> Game.getAllTabsWithColumn
        |> List.find (fun (c,_) -> c = col)
        |> fun (_, tab) -> 
            if count > tab.Visible.Length then 
                None
            else 
                Some tab.Visible.[count - 1]

    let policyMove (left:Column, right:Column) (move:MoveType) = 
        transformMove move right left

    let generateAlPermuations f t count = 
        f |> List.map (fun x -> 
            t |> List.map (fun y -> 
                count |> List.map (fun c -> 
                    x,y,c)))
        |> List.concat
        |> List.concat

    let updateCard card = 
        cardEncoder.DecodeCard (card + 2) // Cards are encoded with an empty value. We don't want that, so it's reduced by 1. We add another 1 as the array is zero index while the encoding is not. 

    let createMoveWtihProb left right (f, t, c: int * float) = 
        let prob = snd f * snd t * snd c // This slows down performance. The NN is not good at learning this. 
        // let prob = snd f * snd t

        updateCard (fst c)
        |> Option.map (fun card -> {From = fst f; To = fst t; Card = card})
        |> Option.map Move
        |> Option.map (policyMove (left, right))
        // |> MoveType.fold (Some Stock)  (Flip >> Some) ()
        |> Option.map (fun x -> x, prob)

    let probsToAllMoves left right (from, too, count) = 
        count 
        |> List.skip 1 // Skip 0 for stock
        |> List.mapi (fun i x -> i, x) 
        |> generateAlPermuations (getColumn from) (getColumn too )
        |> List.choose (createMoveWtihProb left right)
        |> fun xs -> (Stock, Double.MinValue) :: xs


    let probsToAllMovesNoSwap =  probsToAllMoves C1 C1 

    let getMoveCount game c = 
        game
        |> Game.getAllTabsWithColumn 
        |> List.find (fun (col,t) -> col = c.From)
        |> snd
        |> Tableau.getLengthOfMove c.Card

    let oneHotViaCountCount = 10 * 9 * 12 + 1

    let oneHotKeyed game move = 
        match move with
        | Stock -> oneHotViaCountCount - 1
        | Flip _ -> failwithf "Can't encode move Flip"
        | Move co -> 
            let from = co.From |> Coord.toInt |> fun x -> x - 1
            let t =
                let f = from + 1
                let t = co.To |> Coord.toInt
                if t = f then failwithf "Bad move: %A" co
                if t > f then t - 2
                else t - 1

            let count = getMoveCount game co
            let index = (count - 1) + (t * 12) + (from * 12 * 9) 
            index

    let updateCardWithCount count game coord = 
        getCard coord.From count game
        |> Option.map (fun card -> Move {coord with Card = card} )

    let oneHotToMove game move = 
        if move = (oneHotViaCountCount - 1) then Some Stock else
        
        let from = (move / (12 * 9)) |> fun x -> x + 1
        let too = 
            let f = 12 * 9
            let noFrom = move - (move / f) * f 
            let t = noFrom / 12  |> fun x -> x + 1 
            if t >= from then t + 1 else t

        let count = 
            let f = 12 * 9
            let noFrom = move - (move / f) * f 
            let v = noFrom - (noFrom / 12 * 12)
            v + 1

        {
            From = Coord.parseColumn from
            To = Coord.parseColumn too
            Card = Card(1,S) }
        |> updateCardWithCount count game

    let run game c1 c2 cardNumber = 
        let value = oneHotKeyed game (Move {From = c1; To = c2; Card = Card(cardNumber, S)})
        let move = oneHotToMove game value
        printfn "%d %A" value move

    let genGame gameNumber = 
        let r = Random(gameNumber)
        let deck = CardModule.deck OneSuit //|> List.take (13)
        GameMover.createValidGame deck r |> GameMover.unHideGame

    let playMove moves game = 
        moves |> List.fold (fun game move -> 
            game |> GameResult.bind (fun game _ -> GameMover.playMove move game)
        ) (Continue (game, GameMover.validMoves game))

    let sortGame (game: Game) = 
        let getCard index tab = tab |> Tableau.getCard index |> Option.map (CardEncoding.toInt) |> Option.defaultValue -1

        let tabs = 
            game
            |> Game.getAllTabsWithColumn 
            |> List.map (fun (c,x) -> c,x, (x |> Tableau.getRun |> List.length, getCard 0 x, getCard 1 x, getCard 2 x)) 
            |> List.sortByDescending (fun (_,_, order) -> order)
            |> List.map (fun (c,x,_) -> c,x)

        let mapping = 
            let newCol = tabs |> List.map fst
            let oldCol = game |> Game.getAllTabsWithColumn |> List.map fst
            List.zip newCol oldCol

        let tabs = 
            tabs 
            |> List.map snd
            |> List.mapi (fun i x -> i + 1, x)
            |> List.map (fun (i,x) -> Coord.parseColumn i, x)
        
        mapping, Game.updateTableaus tabs game

    let sortMoveWithMappings mapping move = 
        move 
        |> MoveType.map (fun coord -> 
            let cTo = mapping |> List.find (fun (x,y) -> x = coord.To) |> snd
            let cFrom = mapping |> List.find (fun (x,y) -> x = coord.From) |> snd
            Move {coord with To = cTo; From = cFrom } )

    let reverseMove mapping = 
        MoveType.map (fun coord -> 
            let mapping = mapping |> List.map (fun (x,y) -> y,x)
            let cTo = mapping |> List.find (fun (x,y) -> x = coord.To) |> snd 
            let cFrom  = mapping |> List.find (fun (x,y) -> x = coord.From) |> snd
            {coord with To = cTo; From = cFrom} |> Move )

    let moves = 
        [
            Move { From = C2; To = C4; Card = Card (1,S)}
            Move { From = C4; To = C8; Card = Card (2,S)}
            Move { From = C1; To = C10; Card = Card (6,S)}
            Move { From = C2; To = C3; Card = Card (12,S)}
        ]

    let run2 () = 
        
        let g = genGame 42 |> playMove moves 
        let validMoves = g |> GameResult.getGame |> GameMover.validMoves

        genGame 42 |> printfn "%A\n"
        printfn "%A" (g |> GameResult.getGame)
        printfn "%A" validMoves
        let g = GameResult.getGame g
        let mapping, g' = sortGame g
        printfn "%A" mapping
        printfn "%A" g'
        validMoves |> List.map (sortMoveWithMappings mapping) |> List.sort = (GameMover.validMoves g' |> List.sort) |> printfn "%A" 

        validMoves 
        |> List.map (sortMoveWithMappings mapping) 
        |> List.map (reverseMove mapping) 
        |> List.sort = (validMoves |> List.sort) |> printfn "%A" 

    