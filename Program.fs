// Learn more about F# at http://fsharp.org

open System
open TRegex
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
    //printfn "generated term: %A" term2
    //TODO: fix V
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
let getTermDomains (mathml : string) : string list =
    match term mathml with
    | None -> []
    | Some term ->
        let baseExpressionStrings = ["sqrt"; "/"; "+"; "*"; "-"]
        let baseExpressions = List.choose id (List.map (test pTRegex) baseExpressionStrings)
        List.map tregexToString <| collectDomains term baseExpressions

let checkEquality (mathml1: string) (mathml2: string) : bool =
    let term1 = term mathml1
    printfn "term1: %A" term1
    let term2 = term mathml2
    printfn "term2: %A" term2

    term1 = term2

let isFinalAnswerForm (mathml: string) : bool =
    let b = mathmlToMtag mathml
    match b with
    | Some (Mtag.Root (Mtag.Row [Mtag.Row mtagList])) | Some (Mtag.Root (Mtag.Row mtagList)) ->
        let splitByEquals = split ((=) <| Mtag.Operator Equals) mtagList
        let getVariable (x: Mtag) : string option =
            match x with
            | Identifier str ->
                Some str
            | _ -> None

        let rec variablesInMtag (mtag: Mtag) : Mtag list =
            match mtag with
            | Identifier str ->
                [Identifier str]
            | Number _ | Operator _ ->
                []
            | Sup (m1, m2) | Fraction (m1, m2) | Sub (m1, m2) ->
                variablesInMtag m1 @ variablesInMtag m2
            | Root mt | Fenced mt | Sqrt mt ->
                variablesInMtag mt
            | Row mtagList ->
                List.collect variablesInMtag mtagList

        let listOfPossibleSingleVariables = List.map (List.item 0) <| List.filter (List.item 0 >> getVariable >> Option.isSome) splitByEquals

        let perVar (var: Mtag) : bool =
            match var with
            | Identifier _ ->
                let otherParts = List.filter (fun x -> x <> [var]) splitByEquals
                
                let varsInOtherParts = List.map (Mtag.Row >> variablesInMtag) otherParts
                // if the var exists in some other part of the equation, then it's not a final answer
                not <| List.contains true (List.map (fun varsInPart -> List.contains var varsInPart) varsInOtherParts)

                //List.length (List.filter (fun x -> List.contains (fun y -> y = var) x) (varsInParts)) > 1
            | _ -> false
            
        if List.isEmpty listOfPossibleSingleVariables then
                false
            else
                List.contains true (List.map perVar listOfPossibleSingleVariables)
            //if List.length variablesInMtag (fst <| List.item 0 listOfPossibleSingleVariables) = 1 then

//            else



    | _ -> false



type CLIArguments =
    | CheckEquality of mathml1:string * mathml2:string
    | GenerateSimilarTerm of mathml:string
    | IsFinalAnswerForm of mathml:string
    | GetTermDomains of mathml:string
    | Debug
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | CheckEquality _ -> "Checks for equality between two mathML strings."
            | GenerateSimilarTerm _ -> "Generates a similar mathML term."
            | IsFinalAnswerForm _ -> "if answer is of the form of 'one variable'='things without that one variable'"
            | GetTermDomains _ -> "Get the TRegex form patterns in a mathML term."
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
            | GetTermDomains mathml ->
                printfn "%A" (getTermDomains mathml)
            | IsFinalAnswerForm mathml ->
                printfn "%b" (isFinalAnswerForm mathml) 
            parseCommandline xs
    parseCommandline parserResults
    // let tregex = (Parser.test pTRegex "sqrt < ( / < ( ^ < (2 . x) ) < (x.2)) < 5")
    // printfn "%A" tregex.Value
    // printfn "%s" <| tregexToString tregex.Value
    // printfn "full circle works: %b" (tregex = (Parser.test pTRegex (tregexToString tregex.Value)))
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
