module Persisance 
open System.IO
open System
open System.Text
open System.Diagnostics

type Msg = 
    // | WritePolicy of isWin:bool * gameNumber:int * (int * string * int) list
    // | WriteValue of isWin:bool * gameNumber:int * (int * string) list
    | WritePolicyReply of isWin:bool * gameNumber:int * (int * string * int) list * AsyncReplyChannel<unit>
    | WriteValueReply of isWin:bool * gameNumber:int * (int * string) list * AsyncReplyChannel<unit>
    | Finish of AsyncReplyChannel<unit>

type MgsP = 
    | Input of (int * Reformat.LegacyFile [])
    | FinishP of AsyncReplyChannel<unit>

type Saver(policyFile:string, valueFile:string) = 

    let writeFilePolicyFileBuilder (writer:StreamWriter) gameNumber isWin history = 
        history 
        |> List.iter (fun (moveOrder:int, game:string, move:int) -> 
            sprintf "%d,%s,%d,%d,%d" moveOrder game move gameNumber history.Length |> writer.WriteLine ) 

    let writeFileValueFileBuilder (writer:StreamWriter) isWin gameNumber history = 

        history 
        |> List.iter (fun (moveOrder:int, game:string) -> 
            sprintf "%d,%s,%s,%d" moveOrder game (if isWin then "1" else "0") gameNumber |> writer.WriteLine )

    let mb = MailboxProcessor.Start(fun inbox -> 
        let rec loop () = // (policyBuilder: StreamWriter, valueBuilder: StreamWriter, counter) = 
            async {
                let! history = inbox.Receive()
                
                match history with 
                // | WritePolicy (isWin, gameNumber, history) ->  
                //     writeFilePolicyFileBuilder policyBuilder gameNumber isWin history
                // | WriteValue (isWin, gameNumber, history) -> writeFileValueFileBuilder valueBuilder isWin gameNumber history
                | WritePolicyReply (isWin, gameNumber, history, rc) ->  
                
                    let policyBuilder = new StreamWriter(new FileStream(policyFile, FileMode.Append))
                    writeFilePolicyFileBuilder policyBuilder gameNumber isWin history; 
                    policyBuilder.Flush()
                    policyBuilder.Close()
                    rc.Reply()


                | WriteValueReply (isWin, gameNumber, history, rc) -> 
                    let valueBuilder = new StreamWriter(new FileStream(valueFile, FileMode.Append))
                    writeFileValueFileBuilder valueBuilder isWin gameNumber history; 
                    valueBuilder.Flush()
                    valueBuilder.Close() 
                    rc.Reply()

                | Finish rc ->  
                    // policyBuilder.Flush()
                    // valueBuilder.Flush()

                    // policyBuilder.Close()
                    // valueBuilder.Close()

                    rc.Reply ()

                // let counter = 
                //     if counter <= 0 then 
                //         policyBuilder.Flush()
                //         valueBuilder.Flush()
                //         2
                //     else counter         

                return! loop () // (policyBuilder,valueBuilder, counter - 1)
            }
        loop ()) //new StreamWriter(new FileStream(policyFile, FileMode.Append)), new StreamWriter(new FileStream(valueFile, FileMode.Append)), 2))

    // let mbP = MailboxProcessor.Start(fun inbox -> 
    //     let rec loop (policyBuilder: StreamWriter, valueBuilder: StreamWriter) = 
    //         async {
    //             let! input = inbox.Receive()

    //             match input with 
    //             | Input (gn, games) -> 
    //                 games 
    //                 |> Array.map (fun (x: Reformat.LegacyFile) -> x.RowNumber, x.Reward, x.MoveOrder, x.Game, x.Move)
    //                 |> Array.groupBy (fun (_,r,_,_,_) -> r)
    //                 |> Array.iter (fun (isWin, xs) -> 
    //                     let (isWin, gameNumber, history) = isWin, gn, (xs |> Array.sortBy (fun (rn, _, _, _, _) -> rn) |> Array.map (fun (_, _,moveOrder,game,move) -> moveOrder, game, move) |> Seq.toList)
    //                     writeFilePolicyFileBuilder policyBuilder gameNumber isWin history
    //                     writeFileValueFileBuilder valueBuilder isWin gameNumber (history |> List.map (fun (moveOrder, game,_) -> moveOrder, game))
    //                 ) 
    //             | FinishP rc -> 
    //                 policyBuilder.Flush()
    //                 valueBuilder.Flush()
    //                 rc.Reply()

    //             return! loop (policyBuilder, valueBuilder)
    //         }
    //     loop (new StreamWriter(new FileStream(policyFile, FileMode.Append)), new StreamWriter(new FileStream(valueFile, FileMode.Append)) ))

    member this.SaveGameMoves isWin gameNumber history =  


        // let s = Stopwatch()
        // s.Restart()

        // let policyBuilder = new StreamWriter(new FileStream(policyFile, FileMode.Append))
        // let valueBuilder = new StreamWriter(new FileStream(valueFile, FileMode.Append))

        // writeFilePolicyFileBuilder policyBuilder gameNumber isWin (List.rev history)
        // writeFileValueFileBuilder valueBuilder isWin gameNumber (history |> List.rev |> List.map (fun (moveOrder, game,_) -> moveOrder, game))

        // policyBuilder.Flush()
        // valueBuilder.Flush()

        // policyBuilder.Close()
        // valueBuilder.Close()        

        // printfn "%A" s.Elapsed

        // let r = Random()
        // if r.NextDouble() < 0.5 then // 1 in 8
        mb.PostAndReply (fun rc -> WritePolicyReply (isWin, gameNumber, List.rev history, rc))
        mb.PostAndReply (fun rc -> WriteValueReply (isWin, gameNumber, history |> List.rev |> List.map (fun (moveOrder, game,_) -> moveOrder, game), rc))       
        // else 
            // mb.Post (WritePolicy (isWin, gameNumber, List.rev history))
            // mb.Post (WriteValue (isWin, gameNumber, ))  

    member this.Finish() = 
        printfn "waiting to finish"
        // mbP.PostAndReply FinishP
        mb.PostAndReply Finish

    member this.SaveLegacyFormat gn (games: Reformat.LegacyFile [] ) = ()
    //     mbP.Post (Input (gn, games))

    member this.Format() = 
        Reformat.readAndFormatPolicy policyFile
        Reformat.readAndFormatValue valueFile