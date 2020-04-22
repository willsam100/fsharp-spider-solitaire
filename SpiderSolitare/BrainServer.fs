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
    Moves:string seq
    Probs: float list
}

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

let moveEncoder = Representation.ActionEncoder()
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
    
let shortGamePlusOne increment (g: string) = 
    g.Split "," 
    |> Array.map Int32.Parse
    |> Array.map (fun x -> (if x + increment >= 15 || x = 1 then 1 else x + increment) |> string)
    |> String.concat ","

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
    |> formatExpand tabSize
    |> fun x -> x.Split ","
    |> Array.map Int32.Parse 
    |> Array.toList
    |> decodeKeyedGame cardEncoder

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
        let startInfo = ProcessStartInfo("/Users/willsam100/projects/plaidML/flask_run.sh")
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


type BrainsMoverClient(port) = 
    
    let gamesPolicyNet = new Dictionary<Game.Game, (MoveType * float) list>()
    let gamesValueNet = new Dictionary<Game.Game, float>()
    let gamesMovesNet = new Dictionary<Game.Game, MoveType list>()
    let moves = new Dictionary<string, Game.MoveType>()
    let r = Random()
    let client = new HttpClient ()

    let addHeader (headers : Headers.HttpHeaders) (name, value : string) =
        headers.Add (name, value)

    let  addBody (req : HttpRequestMessage) headers body =
        req.Content <- new StringContent (body)
        let contentTypeHeader =
            headers |> List.tryFind (fun (n, _) -> n = "Content-Type")
        contentTypeHeader
        |> Option.iter (fun (_, v) -> req.Content.Headers.ContentType.MediaType <- v)

    let result (t : System.Threading.Tasks.Task<_>) = t.Result

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

    let post port url headers body =
        let rec loop count = 
            try
                if count <= 0 then 
                    failwith "Retry count exceeded"

                composeMessage Net.Http.HttpMethod.Post (Uri url) headers (Some body)
                |> client.SendAsync
                |> result

            with 
            | :? AggregateException -> 
                // if count % 5 = 0 then 
                //     kickBrain()

                Thread.Sleep (r.Next(1, 5) * 1000)
                loop (count - 1)
            | _ -> 
                // if count % 5 = 0 then 
                //     kickBrain()
                Thread.Sleep (r.Next(1, 5) * 1000)
                loop (count - 1)
        loop 2000 // Super high number, only to bail out on on infite loops due to coding error. 

    let bodyText (resp : HttpResponseMessage) =
        resp.Content.ReadAsStringAsync().Result

    let toOneHot moveIndex = 
        allMoves 
        |> List.mapi (fun i x -> if i = moveIndex then "1" else "0")
        |> String.concat ""

    let getPolicy game continuation = 
        match gamesPolicyNet.TryGetValue game with 
        | true, r -> r
        | false, _ -> 
            try 
                // State is not shared accross threads. 
                // This results in duplicate data.
                // Without this below, the data builds up and we get a 'memory leak' if we run the code for a long time. 
                // It brings the program to halt, as it starts thrasing on disk (oberseved 128GB or RAM usage).
                // A full game does not have more than 500 moves, most of the time training is down to the first card deck
                // which should be completed within 100 moves. 
                // if gamesPolicyNet.Count > 1000 then 
                //     gamesPolicyNet.Clear()

                let encoded = MonteCarloTreeSearch.encodeGame game |> format13
                let r = continuation encoded
                gamesPolicyNet.Add (game, r)
                r

            with 
            | e -> 
                printfn "Exception occured:"
                printfn "%A" game
                printfn "%A" <| MonteCarloTreeSearch.encodeGame game
                printfn "%s" <| e.ToString()
                raise e

    let getValue game continuation = 
        match gamesValueNet.TryGetValue game with 
        | true, v -> v
        | false, _ -> 
            try 
                // State is not shared accross threads. 
                // This results in duplicate data.
                // Without this below, the data builds up and we get a 'memory leak' if we run the code for a long time. 
                // It brings the program to halt, as it starts thrasing on disk (oberseved 128GB or RAM usage).
                // A full game does not have more than 500 moves, most of the time training is down to the first card deck
                // which should be completed within 100 moves. 
                // if gamesValueNet.Count > 10000 then 
                //     gamesValueNet.Clear()

                let encoded = MonteCarloTreeSearch.encodeGame game |> format26Stock
                let v = continuation encoded
                gamesValueNet.Add (game, v)
                v

            with 
            | e -> 
                printfn "Exception occured:"
                printfn "%A" game
                printfn "%A" <| MonteCarloTreeSearch.encodeGame game
                printfn "%s" <| e.ToString()
                raise e

    let getMoves game continuation = 
        match gamesMovesNet.TryGetValue game with 
        | true, v -> v
        | false, _ -> 
            try 

                // State is not shared accross threads. 
                // This results in duplicate data.
                // Without this below, the data builds up and we get a 'memory leak' if we run the code for a long time. 
                // It brings the program to halt, as it starts thrasing on disk (oberseved 128GB or RAM usage).
                // A full game does not have more than 500 moves, most of the time training is down to the first card deck
                // which should be completed within 100 moves. 
                // if gamesMovesNet.Count > 10000 then 
                //     gamesMovesNet.Clear()

                let encoded = MonteCarloTreeSearch.encodeGame game |> format96
                let v = continuation encoded
                gamesMovesNet.Add (game, v)
                v

            with 
            | e -> 
                printfn "Exception occured:"
                printfn "%A" game
                printfn "%A" <| MonteCarloTreeSearch.encodeGame game
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
            let encoded =  game |> MonteCarloTreeSearch.encodeGame  |> format13
            continuation encoded

        with 
        | e -> 
            printfn "Exception occured:"
            printfn "%A" game
            printfn "%A" <| MonteCarloTreeSearch.encodeGame game
            printfn "%s" <| e.ToString()
            raise e                


    let getMove moves move = 
        // match moves.TryGetValue move with 
        // | true, m-> m
        // | false, _ -> 

            // if moves.Count > 10000 then 
            //     moves.Clear()

            // printfn "Raw: %s" move
        
            let m = move |> Int32.Parse  |> decodeMove
            // printfn "Moves:"
            // moves |> List.iter (printfn "%A")
            // printfn "--"
            // printfn "%A" m

            match m with 
            | Move m ->
                moves
                |> List.filter (fun x -> x.To = m.To && x.From = m.From)
                |> List.tryHead
                |> Option.map (fun x -> Move x)
                |> Option.defaultValue (Move m)
            | x -> x            


    interface MonteCarloTreeSearch.IBransMover with 
        member this.Flush() =
            moves.Clear()
            gamesPolicyNet.Clear()
            gamesMovesNet.Clear()
            gamesValueNet.Clear()


        member this.GetBestMove(game:Game.Game) = 
//            timed "policy" <| fun () -> 
                getPolicy game
                    (fun encoded -> 
                        let body = {Game = encoded; ValidMoves = ""} |> JsonConvert.SerializeObject
                        let body = post port (sprintf "http://localhost:%d/predict" port) ["Content-Type", "application/json"]  body |> bodyText
                        let bestMoves = body |> JsonConvert.DeserializeObject<ResponsePolicy>

                        let moves = 
                            game 
                            |> GameMover.validMoves
                            |> List.choose (function 
                                | Move m -> Some m
                                | _ -> None)

                        Seq.zip bestMoves.Moves bestMoves.Probs |> Seq.toList |> List.map (fun (x,y) -> getMove moves x, y)
                    )
        
        member this.GetValue game = 
//            timed "value" <| fun () -> 
                getValue game 
                    (fun encoded -> 
                        let body = {Game = encoded; ValidMoves = ""} |> JsonConvert.SerializeObject
                        let responseValue = 
                            post port (sprintf "http://localhost:%d/value" port) ["Content-Type", "application/json"]  body |> bodyText |> JsonConvert.DeserializeObject<ResponseValue>
                        responseValue.Value            
                    )

        member this.GetColumnOfLongestRun game = 
//            timed "value" <| fun () -> 
                getColumnOfLongestRun game 
                    (fun encoded -> 
                        let body = {Game = encoded; ValidMoves = ""} |> JsonConvert.SerializeObject
                        let response = 
                            post port (sprintf "http://localhost:%d/column" port) ["Content-Type", "application/json"]  body |> bodyText 
                        let responseValue = response |> JsonConvert.DeserializeObject<LongestColumn>

                        responseValue.Columns
                        |> List.map (fun x -> Coord.parseColumn (x + 1))
                        |> fun cs -> List.zip cs responseValue.Probs           
                    )

        member this.GetCnnView game = 
//            timed "value" <| fun () -> 
                getColumnOfLongestRun game 
                    (fun encoded -> 
                        let body = {Game = encoded; ValidMoves = ""} |> JsonConvert.SerializeObject
                        let response = 
                            post port (sprintf "http://localhost:%d/columnview" port) ["Content-Type", "application/json"]  body |> bodyText 

                        // printfn "%s" response

                        let responseValue = response |> JsonConvert.DeserializeObject<CnnView>

                        responseValue
                    )

        member this.GetMnist encoded = 
//            timed "value" <| fun () -> 
                let body = {Game = encoded; ValidMoves = ""} |> JsonConvert.SerializeObject
                let response = 
                    post port (sprintf "http://localhost:%d/mnistview" port) ["Content-Type", "application/json"]  body |> bodyText 

                // printfn "%s" response
                let responseValue = response |> JsonConvert.DeserializeObject<MnistView>
                responseValue


        member this.GetMoves game = 
            getMoves game 
                (fun encoded -> 
                    let body = {Game = encoded; ValidMoves = ""} |> JsonConvert.SerializeObject
                    let responseValue = 
                        post port (sprintf "http://localhost:%d/moves" port) ["Content-Type", "application/json"]  body |> bodyText |> JsonConvert.DeserializeObject<ResponseMoves>

                    printfn "%s" 
                        (responseValue.Moves |> List.map (fun x -> sprintf "%.2f" x) |> String.concat ",")

                    responseValue.Moves
                    |> List.mapi (fun i x -> i,x )
                    |> List.filter (fun (i,x) -> x > 0.5)
                    |> List.map (fst >> toOneHot >>  moveEncoder.Decode)
                ) 