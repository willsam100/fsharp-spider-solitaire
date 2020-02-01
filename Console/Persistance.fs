module Persisance 
open System.IO

type Msg = 
    | Write of string
    | Finish of AsyncReplyChannel<unit>

type Saver(policyFile, valueFile) = 
    let writeFile history = 
        if File.Exists (file) then 
            File.AppendAllText (file, "\n" + history)                
        else 
            File.WriteAllText (file, history)         

    let mb = MailboxProcessor.Start(fun inbox -> 
        let rec loop () = 
            async {
                let! history = inbox.Receive()

                match history with 
                | Write history ->  writeFile history 
                | Finish rc ->  rc.Reply ()
                return! loop ()
            }
        loop ())

    member this.SaveGameMoves(isWin, game, move) = 
        mb.Post (history |> List.rev |> String.concat "\n" |> Write)

    member this.Finish() = 
        mb.PostAndReply Finish

    member this.Format() = 
        Script.readAndFormat file