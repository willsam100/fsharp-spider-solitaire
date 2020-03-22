namespace SpiderConsole.Tests

[<RequireQualifiedAccess>]
module Async =
    let Sequentially asyncs =
        async {
            return asyncs |> Seq.map Async.RunSynchronously |> Seq.toArray
    }
