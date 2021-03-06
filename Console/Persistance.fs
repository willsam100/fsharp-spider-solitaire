module Persisance 
open System.IO
open System
open System.Text
open System.Diagnostics
open SpiderSolitare.Brain
open SpiderSolitare.MonteCarloTreeSearch

type Msg = 
    // | WritePolicy of isWin:bool * gameNumber:int * (int * string * int) list
    // | WriteValue of isWin:bool * gameNumber:int * (int * string) list
    | WritePolicyReply of isWin:bool * gameNumber:int * (int * string * int) list * AsyncReplyChannel<unit>
    | WriteValueReply of isWin:bool * gameNumber:int * (int * string) list * AsyncReplyChannel<unit>
    | WriteQLearningReply of isWin:bool * (string * int * string) list * AsyncReplyChannel<unit>
    | Finish of AsyncReplyChannel<unit>

type MgsP = 
    // | Input of (int * Reformat.LegacyFile [])
    | FinishP of AsyncReplyChannel<unit>

type Saver(policyFile:int -> string, valueFile:int->string, qlearnFile:string option) = 

    let writeFilePolicyFileBuilder (writer:StreamWriter) gameNumber isWin history = 
        let isWin = if isWin then 1 else 0
        history 
        |> List.iter (fun (moveOrder:int, game:string, move:int) -> 
            sprintf "%d,%d,%s,%d,%d,%d" isWin moveOrder game move gameNumber history.Length |> writer.WriteLine ) 

    // let writeEpisodeQLearning (writer:StreamWriter) _ history = 
    //     history 
    //     |> List.mapi (fun i (x,y,z) -> 
    //         let isLast = history |> List.tryLast |> Option.get = (x,y,z)
    //         i, isLast, x,y,z )

    //     |> List.collect (fun (i, isLast, game, move, nextGame) ->  Reformat.permuteColumns (game, move, nextGame) |> List.map (fun (x,y,z) -> i, isLast, x,y,z) )
    //     |> List.iter (fun (i, isLast, game:string, move:int, nextGame:string) -> 
    //         let isDone = if isLast then 1. else 0.
    //         let realReward = gameDecoded 26 nextGame |> reward
    //         let reward = 
    //             if isLast then 
    //                 if realReward = winningNodeReward then winningNodeReward else 0.                    
    //             else realReward

    //         sprintf "%d,%f,%.1f,%s,%s" move (reward) isDone ( game) ( nextGame)|> writer.WriteLine ) 

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
                
                    let policyBuilder = new StreamWriter(new FileStream(policyFile gameNumber, FileMode.Append))
                    writeFilePolicyFileBuilder policyBuilder gameNumber isWin history; 
                    policyBuilder.Flush()
                    policyBuilder.Close()
                    rc.Reply()

                | WriteValueReply (isWin, gameNumber, history, rc) -> 
                    let valueBuilder = new StreamWriter(new FileStream(valueFile gameNumber, FileMode.Append))
                    writeFileValueFileBuilder valueBuilder isWin gameNumber history; 
                    valueBuilder.Flush()
                    valueBuilder.Close() 
                    rc.Reply()

                | WriteQLearningReply (isWin, history, rc) -> 
                    // qlearnFile |> Option.iter (fun qlearnFile ->
                    //     let qLearningBuilder = new StreamWriter(new FileStream(qlearnFile, FileMode.Append))
                    //     writeEpisodeQLearning qLearningBuilder isWin history; 
                    //     qLearningBuilder.Flush()
                    //     qLearningBuilder.Close() 
                    // )
                    rc.Reply()

                | Finish rc ->  
                    rc.Reply () 

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
        if isWin then 
            mb.PostAndReply (fun rc -> WritePolicyReply (isWin, gameNumber, List.rev history, rc))
        mb.PostAndReply (fun rc -> WriteValueReply (isWin, gameNumber, history |> List.rev |> List.map (fun (moveOrder, game,_) -> moveOrder, game), rc))       
        // mb.PostAndReply (fun rc -> WriteQLearningReply (isWin, history |> List.rev |> List.map (fun (_, currentGame, nextGame,move) -> currentGame, nextGame, move), rc))       
        // else 
            // mb.Post (WritePolicy (isWin, gameNumber, List.rev history))
            // mb.Post (WriteValue (isWin, gameNumber, ))  

    member this.Finish() = 
        printfn "waiting to finish"
        // mbP.PostAndReply FinishP
        mb.PostAndReply Finish

    // member this.SaveLegacyFormat gn (games: Reformat.LegacyFile [] ) = ()
    //     mbP.Post (Input (gn, games))

    member this.Format() = 
        // Reformat.readAndFormatPolicy policyFile
        ()
        // Reformat.readAndFormatValue valueFile