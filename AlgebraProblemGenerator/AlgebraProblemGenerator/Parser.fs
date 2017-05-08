module Parser

open FParsec
open AlgebraProblemGenerator

let mathmltest = "<math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n  <mstyle displaystyle=\"true\">\n    <mi> r </mi>\n  </mstyle>\n</math>"

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

let ws = spaces
//let pstr = pstring
let pstr s = ws >>. pstringCI s .>> ws
//let sstr = skipStringCI .>> ws

//let skipmathMLTag s = sstr "<math xmlns=\"http://www.w3.org/1998/Math/MathML\">" .>> preturn s >>. sstr "</math>"


let tests = 
    //test (skipmathMLTag "hi" mathmltest)
    let parseVariable = pstr "x"
    test parseVariable " X "
    //test pfloat "hi"
    //test pchar 3