// Learn more about F# at http://fsharp.org


[<EntryPoint>]
let main argv =
    
    let policyRaw = "/Users/willsam100/Desktop/spider-policy-raw.csv"
    let valueRaw = "/Users/willsam100/Desktop/spider-value-raw.csv"
    let policy = "/Users/willsam100/Desktop/spider-policy-net.csv"
    let spiderData = "/Users/willsam100/Desktop/spider-data/data-1.csv"

    let value = "/Users/willsam100/projects/spider-ml/data/data-1.csv"

    match argv with 
    | [||] -> 
        // PolicyRaw.showChartsForRawPolicyData policyRaw
        PolicyCleanData.plotValue spiderData

    | [| "validate"|] -> 
        PolicyCleanData.validate spiderData

    | _ -> printfn "Bad input"

    0 // return an integer exit code
