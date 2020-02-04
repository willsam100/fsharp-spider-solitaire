module Brain 
open System.Diagnostics
open System.Threading
open System.Collections.Generic
open SpiderSolitare.Game
open System.Net.Http
open System
open SpiderSolitare
open Newtonsoft.Json

type Request = {
    Game: string
}

type ResponsePolicy = {
    Moves:string seq
    Probs: float list
}

type ResponseValue = {
    Value: float
}

let moveEncoder = Representation.ActionEncoder()
let allMoves = List.replicate 1171 0


let format (game:string) = 
    let game = game.Split ","
    let tableau = game |> Array.take (96 * 10) |> String.concat ","
    let stock = game |> Array.skip (96 * 10) |> Array.truncate 10 |> String.concat ","

    sprintf "%s,%s" tableau stock

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

type BrainServerProcess(port) = 
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
        printfn "Starting server: %d" port

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
            printfn "Kill process: %d (%d)" port pid
            p <- null
            // Thread.Sleep 5000


type BrainsMover(port) = 
    
    let gamesPolicyNet = new Dictionary<Game.Game, (MoveType * float) list>()
    let gamesValueNet = new Dictionary<Game.Game, float>()
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
                if gamesValueNet.Count > 1000 then 
                    gamesValueNet.Clear()

                let encoded = MonteCarloTreeSearch.encodeGame game |> format
                let r = continuation encoded
                gamesPolicyNet.Add (game, r)
                r

            with 
            | e -> 
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
                if gamesValueNet.Count > 1000 then 
                    gamesValueNet.Clear()

                let encoded = MonteCarloTreeSearch.encodeGame game |> format
                let v = continuation encoded
                gamesValueNet.Add (game, v)
                v

            with 
            | e -> 
                printfn "%A" game
                printfn "%A" <| MonteCarloTreeSearch.encodeGame game
                printfn "%s" <| e.ToString()
                raise e

    let getMove move = 
        match moves.TryGetValue move with 
        | true, m-> m
        | false, _ -> 
            let m = move |> Int32.Parse |> toOneHot |> moveEncoder.Decode
            moves.Add (move, m)
            m

    interface MonteCarloTreeSearch.IBransMover with 
        member this.GetBestMove(game:Game.Game) = 
            getPolicy game
                (fun encoded -> 
                    let body = {Game = encoded} |> JsonConvert.SerializeObject
                    let bestMoves = 
                        post port (sprintf "http://localhost:%d/predict" port) ["Content-Type", "application/json"]  body |> bodyText |> JsonConvert.DeserializeObject<ResponsePolicy>

                    Seq.zip bestMoves.Moves bestMoves.Probs |> Seq.toList |> List.map (fun (x,y) -> getMove x, y)
                )
        
        member this.GetValue game = 
            getValue game 
                (fun encoded -> 
                    let body = {Game = encoded} |> JsonConvert.SerializeObject
                    let responseValue = 
                        post port (sprintf "http://localhost:%d/value" port) ["Content-Type", "application/json"]  body |> bodyText |> JsonConvert.DeserializeObject<ResponseValue>
                    responseValue.Value            
                )

               



