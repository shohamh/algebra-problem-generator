// Learn more about F# at http://fsharp.org

open System
open AlgebraProblemGenerator
open Parser
open QueryBuilder
open QueryExecutor
open Utils

[<EntryPoint>]
let main argv =

    let mutable lst=[1;2;3;4]
    let lst = Utils.insert 0 0 lst
    //printfn "%A" (QueryBuilder.insert 0 0 lst)
    printfn "%A" lst

    let term = Parser.tests
    let qterm = QueryBuilder.Build term [Constraint.Free]
    printfn "QueryBuilder QTerm: %A" qterm

    let term2 = QueryExecutor.executeTerm qterm
    printfn "QueryExecutor Generated Term: %A" term2
    printfn "Hello World from F#!"
    0 // return an integer exit code
