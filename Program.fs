// Learn more about F# at http://fsharp.org

open System
open AlgebraProblemGenerator
open Parser
open FParsec
open QueryBuilder
open QueryExecutor
open Utils
open Argu

let mathMLStrings = [
    "<math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n  <mstyle displaystyle=\"true\">\n  <mrow>  <mn>2</mn><mi>z</mi><mi> r </mi><mo>-</mo><mn>3</mn><mo>-</mo><mi>b</mi><mo>/</mo><mi>b</mi><mo>/</mo><mi>b</mi><mo>-</mo><mi>b</mi>\n </mrow> </mstyle>\n</math>"; 
    "<math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n  <mstyle displaystyle=\"true\">\n  <mrow><mi>x</mi><mo>-</mo><mi>y</mi> </mrow> </mstyle>\n</math>"
]


let generateSimilarTerm (term : Term) =
    let qterm = QueryBuilder.build term [Constraint.Free]
    // printfn "QueryBuilder QTerm: %A\n" qterm
    let term2 = QueryExecutor.executeTerm qterm
    // printfn "QueryExecutor Generated Term: %A\n" term2

    term2

let generateSimilarTermFromMathML (mathML : string) =
    let term = term mathML
    match term with
    | Some t ->
        Some <| generateSimilarTerm t
    | None ->
        None

let generateSimilarMathMLFromMathML (mathML : string) : string =
    let termOption = generateSimilarTermFromMathML mathML
    let mtag = Option.map termToMtag termOption
    let outputMML = Option.map mtagToMathML mtag
    match outputMML with
    | Some mml ->
        mml
    | None ->
        ""

let checkEquality (mathml1: string) (mathml2: string) : bool =
    let term1 = term mathml1
    printfn "term1: %A" term1
    let term2 = term mathml2
    printfn "term2: %A" term2

    term1 = term2

type CLIArguments =
    | CheckEquality of mathml1:string * mathml2:string
    | GenerateSimilarTerm of mathml:string
    | Debug
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | CheckEquality _ -> "Checks for equality between two mathML strings."
            | GenerateSimilarTerm _ -> "Generates a similar mathML term."
            | Debug -> "Prints debug messages for parsers."


[<EntryPoint>]
let main argv =
    let parser = ArgumentParser.Create<CLIArguments>(programName = "dotnet")
    let tempres = parser.Parse argv
    let parserResults =  tempres.GetAllResults()
    //printfn "%A" parserResults
    let rec parseCommandline parserResults =
        match parserResults with
        | [] -> 
            printf ""
        | x::xs ->
            match x with
            | CheckEquality (mathml1, mathml2) ->
                printfn "%A" (checkEquality mathml1 mathml2)
            | GenerateSimilarTerm mathml ->
                printfn "%s" (generateSimilarMathMLFromMathML mathml)
            parseCommandline xs
    parseCommandline parserResults
    // printfn "hi"
    // printfn "%A" Parser.tests 
    // printfn "bye"
    // let testedTerms : Term option list = List.map generateSimilarTermFromMathML mathMLStrings
    // let requestedTerms = 
    //     if List.length (Array.toList argv) > 0 then
    //         List.map generateSimilarMathMLFromMathML (Array.toList argv)
    //     else
    //         List.empty


    // let total = testedTerms @ requestedTerms

    0 // return an integer exit code
