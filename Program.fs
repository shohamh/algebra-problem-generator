// Learn more about F# at http://fsharp.org

open System
open AlgebraProblemGenerator
open Parser
open QueryBuilder
open QueryExecutor
open Utils

let mathMLStrings = [
    "<math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n  <mstyle displaystyle=\"true\">\n  <mrow>  <mn>2</mn><mi>z</mi><mi> r </mi><mo>-</mo><mn>3</mn><mo>-</mo><mi>b</mi><mo>/</mo><mi>b</mi><mo>/</mo><mi>b</mi><mo>-</mo><mi>b</mi>\n </mrow> </mstyle>\n</math>"; 
    "<math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n  <mstyle displaystyle=\"true\">\n  <mrow><mi>x</mi><mo>-</mo><mi>y</mi> </mrow> </mstyle>\n</math>"
]


let generateSimilarTerm (term : Term) =
    let qterm = QueryBuilder.Build term [Constraint.Free]
    printfn "QueryBuilder QTerm: %A\n" qterm
    let term2 = QueryExecutor.executeTerm qterm
    printfn "QueryExecutor Generated Term: %A\n" term2

    term2

let generateSimilarTermFromMathML (mathML : string) =
    let term = term mathML
    match term with
    | Some t ->
        Some <| generateSimilarTerm t
    | None ->
        None

[<EntryPoint>]
let main argv =
    List.map generateSimilarTermFromMathML mathMLStrings

    0 // return an integer exit code
