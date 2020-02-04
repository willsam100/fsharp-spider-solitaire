module Persisance 
open System.IO
open System
open System.Text

type Msg = 
    | WritePolicy of gameNumber:int * (int * string * int) list
    | WriteValue of isWin:bool * gameNumber:int * (int * string) list
    | Finish of AsyncReplyChannel<unit>

type MgsP = 
    | Input of (int * Reformat.LegacyFile [])
    | FinishP of AsyncReplyChannel<unit>

type Saver(policyFile:string, valueFile:string) = 

    let writeFilePolicyFileBuilder (writer:StreamWriter) gameNumber history = 
        history 
        |> List.iter (fun (moveOrder:int, game:string, move:int) -> 
            sprintf "%d,%s,%d,%d,%d" moveOrder game move gameNumber history.Length |> writer.WriteLine ) 

    let writeFileValueFileBuilder (writer:StreamWriter)  isWin gameNumber history = 

        history 
        |> List.iter (fun (moveOrder:int, game:string) -> 
            sprintf "%d,%s,%s,%d" moveOrder game (if isWin then "1" else "0") gameNumber |> writer.WriteLine )

    let mb = MailboxProcessor.Start(fun inbox -> 
        let rec loop (policyBuilder: StreamWriter, valueBuilder: StreamWriter) = 
            async {
                let! history = inbox.Receive()

                match history with 
                | WritePolicy (gameNumber, history) ->  writeFilePolicyFileBuilder policyBuilder gameNumber history
                | WriteValue (isWin, gameNumber, history) -> writeFileValueFileBuilder valueBuilder isWin gameNumber history
                | Finish rc ->  
                    policyBuilder.Flush()
                    valueBuilder.Flush()
                    rc.Reply ()

                policyBuilder.Flush()
                valueBuilder.Flush()
                return! loop (policyBuilder,valueBuilder)
            }
        loop (new StreamWriter(new FileStream(policyFile, FileMode.Append)), new StreamWriter(new FileStream(valueFile, FileMode.Append)) ))

    let mbP = MailboxProcessor.Start(fun inbox -> 
        let rec loop (policyBuilder: StreamWriter, valueBuilder: StreamWriter) = 
            async {
                let! input = inbox.Receive()

                match input with 
                | Input (gn, games) -> 
                    games 
                    |> Array.map (fun (x: Reformat.LegacyFile) -> x.RowNumber, x.Reward, x.MoveOrder, x.Game, x.Move)
                    |> Array.groupBy (fun (_,r,_,_,_) -> r)
                    |> Array.iter (fun (isWin, xs) -> 
                        let (isWin, gameNumber, history) = isWin, gn, (xs |> Array.sortBy (fun (rn, _, _, _, _) -> rn) |> Array.map (fun (_, _,moveOrder,game,move) -> moveOrder, game, move) |> Seq.toList)
                        if isWin then 
                            writeFilePolicyFileBuilder policyBuilder gameNumber history
                        writeFileValueFileBuilder valueBuilder isWin gameNumber (history |> List.map (fun (moveOrder, game,_) -> moveOrder, game))
                    ) 
                | FinishP rc -> 
                    policyBuilder.Flush()
                    valueBuilder.Flush()
                    rc.Reply()

                return! loop (policyBuilder, valueBuilder)
            }
        loop (new StreamWriter(new FileStream(policyFile, FileMode.Append)), new StreamWriter(new FileStream(valueFile, FileMode.Append)) ))

    member this.SaveGameMoves isWin gameNumber history = 
        if isWin then 
            mb.Post (WritePolicy (gameNumber, List.rev history))
        mb.Post (WriteValue (isWin, gameNumber, history |> List.rev |> List.map (fun (moveOrder, game,_) -> moveOrder, game)))


    member this.Finish() = 
        printfn "waiting to finish"
        mbP.PostAndReply FinishP
        mb.PostAndReply Finish

    member this.SaveLegacyFormat gn (games: Reformat.LegacyFile [] ) = 
        mbP.Post (Input (gn, games))

    member this.Format() = 
        Reformat.readAndFormatPolicy policyFile
        Reformat.readAndFormatValue valueFile