module Parser

open FParsec
open AlgebraProblemGenerator

let mathmltest = "<math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n  <mstyle displaystyle=\"true\">\n    <mi> r </mi>\n  </mstyle>\n</math>"

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg


// Debug trace thing
let (<!>) (p: Parser<_,_>) label : Parser<_,_> =
    fun stream ->
        printfn "%A: Entering %s" stream.Position label
        let reply = p stream
        printfn "%A: Leaving %s (%A)" stream.Position label reply.Status
        reply

//let pstr = pstring
let pstr s = spaces >>. pstringCI s .>> spaces
let sstr s = spaces >>. skipStringCI s .>> spaces 


type Mtag =
| Root of Mtag
| Fraction of Mtag * Mtag
| Row of Mtag list
| Identifier of string
| Operator of string
| Number of float

// BTag = Begin Tag
// ETag = End Tag
// ITag = the insides between a BTag and a ETag

// returns the attributes of the tag:
let pBTag tagName = between (sstr ("<" + tagName)) (sstr ">") (manyChars (noneOf ">")) //TODO: watch out for '\>' before the actual end of the tag (>)
let pETag tagName = between (sstr "</") (sstr ">") (pstr tagName)
let pTag tagName = between (pBTag tagName) (pETag tagName) 


let pMi = (pTag "mi" ((many1 anyChar) |>> (string >> Mtag.Identifier))) <!> "pMi"
let pMo = (pTag "mo" ((pstr "+" <|> pstr "-") |>> Mtag.Operator)) <!> "pMo"
let pMn = (pTag "mn" ((pfloat) |>> Mtag.Number)) <!> "pMn"
let pMrow = (pTag "mrow" ((many <| choice [pMi; pMo; pMn]) |>> Mtag.Row)) <!> "pMrow"
let pMstyle = (pTag "mstyle") <!> "pMstyle"
let pMathTag = (pTag "math") <!> "pMathTag"

let pMainTags = [pMrow; pMi; pMo; pMn]
let pMathML = pMathTag (pMstyle (many <| choice pMainTags)) <!> "pMathML"


let tests = 
    printfn "Parser tests:"
    printfn "----------------------------\n"
    test pMathML "<math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n  <mstyle displaystyle=\"true\">\n    <mi> r </mi><mo>+</mo><mn>3</mn>\n  </mstyle>\n</math>"
    printfn "\n----------------------------\n\n"