// Learn more about F# at http://fsharp.org

open System
open Parser
open QueryBuilder
open QueryExecutor
[<EntryPoint>]
let main argv =
    let term = Parser.tests
    let qterm = QueryBuilder.Build term
    printfn "QueryBuilder QTerm: %A" qterm

    let term2 = QueryExecutor.executeTerm qterm
    printfn "QueryExecutor Generated Term: %A" term2
    printfn "Hello World from F#!"
    0 // return an integer exit code
