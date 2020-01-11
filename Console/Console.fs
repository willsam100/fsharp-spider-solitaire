module Console
open System
open System.IO
open SpiderSolitare
open StackExchange.Profiling
open Newtonsoft.Json
open System.Net.Http
open System.Collections.Generic
open System.Threading.Tasks
open System.Diagnostics
open System.Threading

type Request = {
    Game: string
}

type Response = {
    Moves:string seq
}

let moveEncoder = Representation.ActionEncoder()
let allMoves = List.replicate 1171 0


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
            p.Kill()
            p.WaitForExit()
            printfn "Kill process: %d (%d)" port pid
            let pKill = Process.Start("pkill", sprintf "-TERM -P %d" pid)
            pKill.WaitForExit()
            p <- null
            Thread.Sleep 5000


type BrainsMover(port) = 
    
    let games = new Dictionary<Game.Game, string>()
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

    let getGame game = 
        match games.TryGetValue game with 
        | true, g -> g
        | false, _ -> 
            try 
                let encoded = MonteCarloTreeSearch.encodeGame game
                games.Add (game, encoded)
                encoded
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
            let encoded = getGame game
            let body = {Game = encoded} |> JsonConvert.SerializeObject
            let bestMoves = 
                post port (sprintf "http://localhost:%d/predict" port) ["Content-Type", "application/json"]  body |> bodyText |> JsonConvert.DeserializeObject<Response>
            bestMoves.Moves |> Seq.toList |> List.map getMove

type Msg = 
    | Write of string
    | Finish of AsyncReplyChannel<unit>

[<EntryPoint>]
let main argv =
    
    let moveEncoder = Representation.ActionEncoder()
    let mctsSearchIterationCount = 2000
    let moveCount = 1
    let file rNumber = sprintf "/Users/willsam100/Desktop/spider%d.csv"  89
    let s = Stopwatch()

    let progress count = 
        let p = (float moveCount - float count) / float moveCount 
        p * (float mctsSearchIterationCount / 1.01) |> int

    let writeFile history = 
        if File.Exists (file 0) then 
            File.AppendAllText (file 0, "\n" + history)                
        else 
            File.WriteAllText (file 0, history)       

    let mb = MailboxProcessor.Start(fun inbox -> 
        let rec loop () = 
            async {
                let! history = inbox.Receive()

                match history with 
                | Write history -> 
                    writeFile history 
                | Finish rc -> 
                    rc.Reply ()

                return! loop ()
            }
        loop ())

    let perodicSaveAsync rNumber count history node = 
        mb.Post (history |> List.rev |> String.concat "\n" |> Write)

    let updateHistory game move history = 
        let gameAndBestMove = 
            sprintf "%s,%s" 
                (game |> Representation.encodeKeyGame MonteCarloTreeSearch.cardEncoder |> Seq.map string |> String.concat ",") 
                (moveEncoder.Encode move |> Array.map string |> String.concat ",") 
        gameAndBestMove :: history

    let rec printTree depth (node: MonteCarloTreeSearch.Node) = 
        printfn "%s%f %d %A %d" depth node.T node.N node.Move (node.Game.GetHashCode())
        match node.Children with 
        | [] -> ()
        | xs -> 
            xs |> List.iter (printTree (depth + "  "))

    let rec getGames (node: MonteCarloTreeSearch.Node) = 
        // let game = Representation.decodeKeyedGame MonteCarloTreeSearch.cardEncoder (node.Game.Split (',') |> Array.map Int32.Parse |> Array.toList)
        match node.Children with 
        | [] -> [node.Game]
        | xs -> node.Game :: (xs |> List.collect getGames)


    let rec playGame brainsMover gameNumber history count pastGames game (node: MonteCarloTreeSearch.Node) = 
        let iterationCount = mctsSearchIterationCount - progress count + (if  node.N = 0 ||  node.T / (float node.N) <=  0.4 then 50000 else 0)
        // printfn "Iteration Count: %d - %d" iterationCount count
        
        s.Restart()
        let n = MonteCarloTreeSearch.search brainsMover pastGames iterationCount node
        s.Stop()
        printfn ""
        printfn "C:%d PG:%d %A (%d)" iterationCount (Set.count pastGames) s.Elapsed (s.ElapsedMilliseconds / 1000L)

        if List.isEmpty n.Children then 
            perodicSaveAsync gameNumber 0 history node        
            printf "No more moves: %d" gameNumber
            printfn "%A\n" game

        else 
            let nMove = n.Children |> List.maxBy (fun x -> x.T / (float n.N))

            let gameResult = Game.GameMover.playMove nMove.Move.Value game
            match gameResult with 
            | Game.Continue (game',_s) -> 

                if count <= 0 then 
                    printfn "Final result (not saved):"
                    printfn "Game Number: %d" gameNumber
                    printfn "%A" game'

                else             
                    printfn "%A" game'
                    let pastGames = Set.add game' pastGames
                    let pastGames = Set.union pastGames (getGames nMove |> Set.ofList)
                    playGame brainsMover gameNumber (updateHistory game nMove.Move.Value history) (count - 1) pastGames game' nMove

            | Game.Lost _ -> 
                perodicSaveAsync gameNumber 0 history node       
                printfn "Lost (not saved): %d" gameNumber
                printfn "%A" game
            | Game.Won -> 
                let history = updateHistory game nMove.Move.Value history
                perodicSaveAsync gameNumber 0 history node        
                printfn "Game completed and saved!"

    let playSingleGame brainsMover rNumber =
        let r = Random(rNumber)
        let deck = Game.CardModule.deck Game.OneSuit
        let game = Game.GameMover.createValidGame deck r |> Game.GameMover.unHideGame

        let root = {
            MonteCarloTreeSearch.T = 0.; 
            MonteCarloTreeSearch.N = 0; 
            MonteCarloTreeSearch.Children = []; 
            MonteCarloTreeSearch.Game = game; 
            MonteCarloTreeSearch.Move = None}  
        playGame brainsMover rNumber [] moveCount (Set.singleton game) game root

    let playGamesForRange port range = 
        let brain = BrainServerProcess(port)
        // brain.StartServer()
        let brainsMover = BrainsMover(port)
        try 
            range |> List.iter (playSingleGame brainsMover)
            // brainsMover.KillBrain()
        finally 
            brain.Stop()

    [4 .. 4] 
    |> List.splitInto 6
    |> List.mapi (fun i x -> 5100 + i * 2, x)
    |> List.map (fun (port, range) -> Task.Run(fun () ->  playGamesForRange port range) )
    |> List.toArray    
    |> Task.WhenAll    
    |> Async.AwaitTask
    |> Async.RunSynchronously

    mb.PostAndReply Finish

    // Console.WriteLine(MiniProfiler.Current.RenderPlainText());
    0 // return an integer exit code
