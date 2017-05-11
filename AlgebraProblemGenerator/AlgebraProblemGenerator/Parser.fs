module Parser

open FParsec
open AlgebraProblemGenerator

let mathmltest = "<math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n  <mstyle displaystyle=\"true\">\n    <mi> r </mi>\n  </mstyle>\n</math>"

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

//let pstr = pstring
let pstr s = spaces >>. pstringCI s .>> spaces
let sstr s = spaces >>. skipStringCI s .>> spaces 


type Mthings =
| Identifier of string
| Operator of string
| Float of float

// BTag = Begin Tag
// ETag = End Tag
// ITag = the insides between a BTag and a ETag

// returns the attributes of the tag:
let pBTag tagname = between (sstr ("<" + tagname)) (sstr ">") (manyChars (noneOf ">")) //TODO: watch out for \> in before the end of the tag (>)

let pETag tagname = sstr ("</" + tagname + ">")


let pMi = between (pBTag "mi") (pETag "mi") (Mthings.Identifier (many1 anyChar))
let pMo = between (pBTag "mo") (pETag "mo") (Mthings.Operator (pstr "+" <|> pstr "-"))
let pMn = between (pBTag "mn") (pETag "mn") (Mthings.Float pfloat)
let pMrow = between (pBTag "mrow") (pETag "mrow") (many <| choice [pMi; pMo; pMn])
let pMstyle p = between (pBTag "mstyle") (pETag "mstyle") p
let pMath p = between (pBTag "math") (pETag "math") p

let pMainTags = [pMrow; pMi; pMo; pMn]

let pMathML = pMath (pMstyle (many <| choice (pMainTags )))




let tests = 
    //test (skipmathMLTag "hi" mathmltest)
    printfn "Parser tests:"
    printfn "----------------------------\n"
    let parseVariable = pstr "x"
    test parseVariable " X "
    let skipVariable = sstr "x"
    test skipVariable " X "
    test pMathML "<math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n  <mstyle displaystyle=\"true\">\n    <mi> r </mi><mo>+</mo><mn>3</mn>\n  </mstyle>\n</math>"
    test (pBTag "mstyle") "<mstyle displaystyle=\"true\">"
    test (pETag "mstyle") "</mstyle>"

    printfn "\n----------------------------\n\n"