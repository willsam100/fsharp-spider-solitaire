﻿// Learn more about F# at http://fsharp.org


[<EntryPoint>]
let main argv =
    
    let policyRaw = "/Users/willsam100/Desktop/spider-policy-raw.csv"
    let valueRaw = "/Users/willsam100/Desktop/spider-value-raw.csv"
    let policy = "/Users/willsam100/Desktop/spider-policy-net.csv"
    
    // PolicyRaw.showChartsForRawPolicyData policyRaw
    PolicyCleanData.showChartsForPolicyCleanData policy

    0 // return an integer exit code
