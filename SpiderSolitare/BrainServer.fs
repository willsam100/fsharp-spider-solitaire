module SpiderSolitare.Brain 
open System.Diagnostics
open System.Threading
open System.Collections.Generic
open SpiderSolitare.Game
open System.Net.Http
open System
open SpiderSolitare
open Newtonsoft.Json
open SpiderSolitare.MonteCarloTreeSearch
open SpiderSolitare.Representation

type Request = {
    Game: string
    ValidMoves: string
}

type ResponsePolicy = {
    C: float list
    // T: float list
    // N: float list
}

// type ResponsePolicy = {
//     F: float list
//     T: float list
//     C: float list
// }

type ResponseValue = {
    Value: float
}

type LongestColumn = {
    Columns:int list
    Probs: float list
}


type ResponseMoves = {
    Moves: float list
}


let allMoves = List.replicate 1171 0

let timed name f = 
    let s = Stopwatch()
    s.Restart()
    let x = f ()
    printfn "Timed %s: %A" name s.Elapsed
    x

let mutliLabelMoves (g:string) decodeKeyedGame = 
    g.Split "," 
    |> Array.map Int32.Parse 
    |> Array.toList 
    |> decodeKeyedGame
    |> GameMover.validMoves 
    |> List.map (fun x -> x |> moveEncoder.Encode)
    |> List.fold (fun acc x -> 
        let t = Array.zip acc x 
        t |> Array.map (fun (l,r) -> 
            let one = int16 1 
            if l = one || r = one then one else int16 0)
    ) (Array.replicate 1171 (int16 0))

    |> Array.map string 
    |> String.concat ","
    
let singleEncoded (g:string) =
    
    let shortGame =
        g.Split "," 
        |> Array.take (96 * 10)
        |> Array.chunkBySize 96
        |> Array.collect (Array.truncate 13)
    
    [ 2 .. 14 ]
    |> List.map (fun x ->
        let card = string x
        shortGame
        |> Array.map (fun c -> if c = card then "1" else "0")
        |> String.concat "," )
    |> String.concat ","

// let gamePlusOne increment (g: string) = 
    
//     let tabs = 
//         g.Split "," 
//         |> Array.splitInto 10

//     if tabs |> Array.map Array.length |> Array.distinct |> Array.length <> 1 then
//         printfn "%s" g
//         failwith "Bad input, not all tabs are the same length"

//     tabs
//     |> Array.map (Array.map Int32.Parse)
//     |> Array.collect (Array.map (fun x -> (if x >= 15 then 1 else x + increment) |> string))
//     |> String.concat ","
    
let gamePlusOne increment (g: string) = 
    g.Split "," 
    |> Array.map Int32.Parse
    |> Array.map (fun x -> 
        let value = x + increment
        match x, value with 
        | 1, _ -> "1"
        | _, 15 -> "2"
        | _, 1 -> "14"
        | _, _ -> string value )
    |> String.concat ","

let movePlusOne increment = function
    | Flip f -> f |> Flip |> Some
    | Stock -> Stock |> Some
    | Move c -> 
        let value = CardModule.getValue c.Card
        match value, value + increment with 
        | 1, 0 -> None // Queen to King is not a valid move. This will change to King -> Ace.
        | 12, 13 -> None // Queen to King is not a valid move. This will change to King -> Ace.
        | 13, _ -> None // Move with Kings are questionable. A transformation may not be valid ie to an empty column
        | _, _ -> { c with Card = CardModule.increment increment c.Card} |> Move |> Some

let format96 (game:string) = 
    let game = game.Split ","

    if game.Length <= (96 * 10) + 10 then 
        game |> String.concat "," |> printfn "%s"
        failwith "Game is too short"

    let tableau = game |> Array.take (96 * 10) |> String.concat ","
    let stock = game |> Array.skip (96 * 10) |> Array.truncate 10 |> String.concat ","

    sprintf "%s,%s" tableau stock

let format13 (game:string) = 
    let game = game.Split ","

    if game.Length <= (96 * 10) + 10 then 
        game |> String.concat "," |> printfn "%s"
        failwith "Game is too short"

    game 
    |> Array.chunkBySize 96
    |> Array.take 10
    |> Array.collect (Array.truncate 13)
    |> String.concat ","

let format26to13 (game:string) = 
    let game = game.Split ","

    if game.Length <> 286 then 
        game |> String.concat "," |> printfn "%s"
        printfn "Length: %d" game.Length
        failwith "Game is too short"

    game 
    |> Array.chunkBySize 26
    |> Array.take 10
    |> Array.collect (Array.truncate 13)
    |> String.concat ","


let format26Stock (game:string) = 
    let game = game.Split ","

    if game.Length <= (96 * 10) + 10 then 
        game |> String.concat "," |> printfn "%s"
        failwith "Game is too short"

    let tableau = 
        game 
        |> Array.chunkBySize 96
        |> Array.take 10
        |> Array.collect (Array.truncate 26)
        |> String.concat ","

    let stock = 
        game 
        |> Array.skip (96 * 10) 
        |> Array.truncate 10

    let stock = 
        let empty = Array.replicate (26 - stock.Length) "1"
        Array.append stock empty |> String.concat ","

    sprintf "%s,%s" tableau stock


let formatExpand tabSize (game:string) =
    let tabs = game.Split "," |> Array.chunkBySize tabSize
    let fullTabs = tabs |> Array.collect (fun x ->  Array.replicate (96 - x.Length) "1" |> Array.append x )
    Array.append fullTabs (game.Split "," |> Array.skip (tabSize * 10)) |> String.concat ","

let gameDecoded tabSize (game: string) = 
    game
    |> fun x -> x.Split ","
    |> Array.map Int32.Parse 
    |> Array.toList
    |> decodeKeyedGame tabSize cardEncoder

// let format (game:string) = 
//     let game = game.Split ","
//     // let tableau = game |> Array.take (96 * 10) |> String.concat ","
//     // let stock = game |> Array.skip (96 * 10) |> Array.truncate 10 |> String.concat ","

//     // sprintf "%s,%s" tableau stock

//     let tableau = game |> Array.take (96 * 10)
//     let stock = game |> Array.skip (96 * 10)

//     let emptyPadding = Array.replicate (96 * (95 - 10)) "0" // 95 as the last row is the stock. 10 rows are the tableaus. 96 is the length of each tableau

//     let hasStock = 
//         stock 
//         |> Array.chunkBySize 10
//         |> Array.map (fun xs -> if xs |> Array.forall (fun c -> c <> "0") then "15" else "0")

//     let lastRow = Array.append hasStock (Array.replicate (96 - hasStock.Length) "0") |> String.concat ","

//     let tableau = 
//         let tableau = tableau |> Array.map (fun x -> if x = "1" then "0" else x)
//         Array.append tableau emptyPadding |> String.concat ","

//     sprintf "%s,%s" tableau lastRow

type BrainMoverServer(port) = 
    let mutable p: Process = null

    let runPythonServer port = 
        let startInfo = ProcessStartInfo("/Users/willsam100/projects/spider-ml/flask_run.sh")
        // let startInfo = ProcessStartInfo("bash")

        startInfo.RedirectStandardInput <- true
        startInfo.RedirectStandardOutput <- true
        startInfo.UseShellExecute <- false
        startInfo.Arguments <- string port
        startInfo.WindowStyle <- ProcessWindowStyle.Hidden
        Process.Start(startInfo)

    member this.StartServer() = 

        if isNull p |> not then 
            this.Stop()

        p <- runPythonServer (string port)
        Thread.Sleep 5000

    member this.Stop() = 

        if isNull p then ()
        else 
            let pid = p.Id
            let pKill = Process.Start("/Users/willsam100/projects/SpiderSolitare/Console/kill.sh", string pid)
            pKill.WaitForExit()
            p.Kill()
            p.WaitForExit()
            p <- null
            // Thread.Sleep 5000


type BrainsMoverClient(ports: int list) = 
    
//    let gamesPolicyNet = new Dictionary<Game.Game, (MoveType * float) list>()
//    let gamesValueNet = new Dictionary<Game.Game, float>()
//    let gamesMovesNet = new Dictionary<Game.Game, MoveType list>()
//    let moves = new Dictionary<string, Game.MoveType>()
    let r = Random()

    let handler = new HttpClientHandler(MaxConnectionsPerServer = 10);
    let client = new HttpClient(handler)
    let ob = new Object()

    let getPort(): int = 
        let x = r.Next(0, List.length ports)
        ports.[x]
        
        

    let addHeader (headers : Headers.HttpHeaders) (name, value : string) =
        headers.Add (name, value)

    let addBody (req : HttpRequestMessage) headers body =
        req.Content <- new StringContent (body)
        let contentTypeHeader =
            headers |> List.tryFind (fun (n, _) -> n = "Content-Type")
        contentTypeHeader
        |> Option.iter (fun (_, v) -> req.Content.Headers.ContentType.MediaType <- v)

    let result (t: Tasks.Task<_>) = t |> Async.AwaitTask

    let composeMessage meth (url : Uri) headers body =
        let req = new HttpRequestMessage (meth, url)
        Option.iter (addBody req headers) body

        headers
        |> List.partition (fun (n, _) -> n = "Content-Type")
        |> snd
        |> List.iter (addHeader req.Headers)
        req

    let get url headers =
        // HttpMethod is qualified to avoid collision with FSharp.Data.HttpMethod,
        // if FSharp.Data is imported in a script as well as Furl.
        composeMessage Net.Http.HttpMethod.Get (Uri url) headers None
        |> client.SendAsync
        |> result

    let post url headers body =
        let rec loop count = 
            async {
                let port = getPort()
                let url = url port
                if count <= 0 then 
                    printfn "Bailing, flask is flaking"
                    failwith "Retry count exceeded"

                let! response = 
                    composeMessage HttpMethod.Post (Uri url) headers (Some body)
                    |> client.SendAsync
                    |> Async.AwaitTask
                    |> Async.Catch
                
                return! 
                    response 
                    |> function
                    | Choice1Of2 x -> async { return x }
                    | Choice2Of2 e -> 
                        match e with 
                        | :? AggregateException -> 
                            // if count % 5 = 0 then 
                            //     kickBrain()

                            Thread.Sleep (r.Next(1, 5) * 500)
                            loop (count - 1)
                        | e -> 
                            // if count % 5 = 0 then 
                            //     kickBrain()
                            Thread.Sleep (r.Next(1, 5) * 500)
                            loop (count - 1)
            }
        loop 2000 // Super high number, only to bail out on on infite loops due to coding error. 

    let bodyText (resp : HttpResponseMessage) =
        resp.Content.ReadAsStringAsync().Result

    let toOneHot moveIndex = 
        allMoves 
        |> List.mapi (fun i x -> if i = moveIndex then "1" else "0")
        |> String.concat ""

    let getPolicy (game) continuation = 
    
        // match gamesPolicyNet.TryGetValue game with 
        // | true, r -> r
        // | false, _ -> 
        try 
            // State is not shared accross threads. 
            // This results in duplicate data.
            // Without this below, the data builds up and we get a 'memory leak' if we run the code for a long time. 
            // It brings the program to halt, as it starts thrasing on disk (oberseved 128GB or RAM usage).
            // A full game does not have more than 500 moves, most of the time training is down to the first card deck
            // which should be completed within 100 moves. 
            // if gamesPolicyNet.Count > 1000 then 
            //     gamesPolicyNet.Clear()

                // let encoded = MonteCarloTreeSearch.encodeGame 13 game
                // let encoded = encodeGameWithStock 26 (26 * 11) game
                let encoded = encodeOneHotGame 13 1 game |> List.map string |> String.concat ","
                let r = continuation encoded
                // gamesPolicyNet.Add (game, r)
                r
        with 
        | e -> 
            printfn "Exception occured:"
            printfn "%A" game
            // printfn "%A" <| MonteCarloTreeSearch.encodeGame 13 game
            printfn "%s" <| e.ToString()
            raise e
        

    let getValue game continuation = 
        // match gamesValueNet.TryGetValue game with 
        // | true, v -> v
        // | false, _ -> 
            // let reward = MonteCarloTreeSearch.reward game        

            try 
                // State is not shared accross threads. 
                // This results in duplicate data.
                // Without this below, the data builds up and we get a 'memory leak' if we run the code for a long time. 
                // It brings the program to halt, as it starts thrasing on disk (oberseved 128GB or RAM usage).
                // A full game does not have more than 500 moves, most of the time training is down to the first card deck
                // which should be completed within 100 moves. 
                // if gamesValueNet.Count > 10000 then 
                //     gamesValueNet.Clear()

                let encoded = encodeGameWithStock 26 (26 * 11) game
                let v = continuation encoded
                // gamesValueNet.Add (game, v)
                v

            with 
            | e -> 
                printfn "Exception occured:"
                printfn "%A" game
                printfn "%A" <| MonteCarloTreeSearch.encodeGame 13 game
                printfn "%s" <| e.ToString()
                raise e

    let getMoves game continuation = 
//        match gamesMovesNet.TryGetValue game with 
//        | true, v -> v
//        | false, _ -> 
        try 

            // State is not shared accross threads. 
            // This results in duplicate data.
            // Without this below, the data builds up and we get a 'memory leak' if we run the code for a long time. 
            // It brings the program to halt, as it starts thrasing on disk (oberseved 128GB or RAM usage).
            // A full game does not have more than 500 moves, most of the time training is down to the first card deck
            // which should be completed within 100 moves. 
            // if gamesMovesNet.Count > 10000 then 
            //     gamesMovesNet.Clear()

            let encoded = MonteCarloTreeSearch.encodeGame 96 game
            let v = continuation encoded
//                gamesMovesNet.Add (game, v)
            v

        with 
        | e -> 
            printfn "Exception occured:"
            printfn "%A" game
            printfn "%A" <| MonteCarloTreeSearch.encodeGame 96 game
            printfn "%s" <| e.ToString()
            raise e                

    let getColumnOfLongestRun game continuation = 
        try 

            // State is not shared accross threads. 
            // This results in duplicate data.
            // Without this below, the data builds up and we get a 'memory leak' if we run the code for a long time. 
            // It brings the program to halt, as it starts thrasing on disk (oberseved 128GB or RAM usage).
            // A full game does not have more than 500 moves, most of the time training is down to the first card deck
            // which should be completed within 100 moves. 
            // if gamesMovesNet.Count > 10000 then 
            //     gamesMovesNet.Clear()
            let encoded =  game |> MonteCarloTreeSearch.encodeGame 13
            continuation encoded

        with 
        | e -> 
            printfn "Exception occured:"
            printfn "%A" game
            printfn "%A" <| MonteCarloTreeSearch.encodeGame 13 game
            printfn "%s" <| e.ToString()
            raise e                


    let getMove moves move = 
        // match moves.TryGetValue move with 
        // | true, m-> m
        // | false, _ -> 

            // if moves.Count > 10000 then 
            //     moves.Clear()

            // printfn "Raw: %s" move
        
            // let m = move |> Int32.Parse |> decodeMove
            // // printfn "Moves:"
            // // moves |> List.iter (printfn "%A")
            // // printfn "--"
            // // printfn "%A" m

            // match m with 
            // | Move m ->
            //     moves
            //     |> List.filter (fun x -> x.To = m.To && x.From = m.From)
            //     |> List.tryHead
            //     |> Option.map (fun x -> Move x)
            //     |> Option.defaultValue (Move m)
            // | x -> x            
        move |> Int32.Parse |> MoveEncoding.intToMove

    member this.GetPolicy (encoded:string) = 
        async {
            let body = {Game = encoded; ValidMoves = ""} |> JsonConvert.SerializeObject
            let! body = post (sprintf "http://localhost:%d/predict") ["Content-Type", "application/json"]  body 
            let body = body |> bodyText
            let bestMoves = body |> JsonConvert.DeserializeObject<ResponsePolicy>
            // return (bestMoves.F, bestMoves.T, bestMoves.C)
            return bestMoves.C //@ bestMoves.T @ bestMoves.N
        }

    interface MonteCarloTreeSearch.IBrainsMover with 
        member this.Flush() = ()
//            moves.Clear()
//            gamesPolicyNet.Clear()
//            gamesMovesNet.Clear()
//            gamesValueNet.Clear()


        member this.GetBestMove(game:Game) = 
//            timed "policy" <| fun () -> 
                getPolicy game
                    (fun (encoded:string) -> 
                        async {
                            let body = {Game = encoded; ValidMoves = ""} |> JsonConvert.SerializeObject
                            let! body = post (sprintf "http://localhost:%d/predict") ["Content-Type", "application/json"]  body 
                            let body = body |> bodyText
                            let bestMoves = body |> JsonConvert.DeserializeObject<ResponsePolicy>
                            // return (bestMoves.F, bestMoves.T, bestMoves.C)
                            return bestMoves.C
                        }
                    )
        
        member this.GetValue game = 
//            timed "value" <| fun () -> 
                getValue game 
                    (fun encoded -> 
                        async {
                            let body = {Game = encoded; ValidMoves = ""} |> JsonConvert.SerializeObject
                            let! responseValue = 
                                post (sprintf "http://localhost:%d/value") ["Content-Type", "application/json"]  body 
                                
                            let responseValue = responseValue |> bodyText |> JsonConvert.DeserializeObject<ResponseValue>
                            return responseValue.Value   
                        }         
                    )


