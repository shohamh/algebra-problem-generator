module Parser

open FParsec
open AlgebraProblemGenerator


let mathmltest = "<math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n  <mstyle displaystyle=\"true\">\n    <mi> r </mi>\n  </mstyle>\n</math>"



let test p str =
    match run p str with
    | Success(result, _, _) ->
        printfn "Test\n==============\n Success: %A\n==============\n" result
        Some result                        
    | Failure(errorMsg, _, _) -> 
        printfn "Test\n==============\n Failure: %A\n==============\n" errorMsg
        None


let debug = false

// Debug trace thing
let (<!>) (p: Parser<'a, 'b>) label =
    if debug then
        fun (stream : CharStream<'b>) ->
            printfn "%A: Entering %s with input: %s" stream.Position label (stream.PeekString 1000) 
            let reply = p stream
            printfn "%A: Leaving %s (%A)" stream.Position label reply.Status
            reply
    else
        p

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
let pBTag tagName  = between (sstr ("<" + tagName)) (sstr ">") (manyTill (noneOf ">") (lookAhead (sstr ">"))) <!> "pBTag" //TODO: watch out for '\>' before the actual end of the tag (>)
let pETag tagName  = between (sstr "</") (sstr ">") (pstr tagName) <!> "pETag"
let pTag tagName p = between (pBTag tagName) (pETag tagName) p <!> "pTag"

let identifier = identifier(IdentifierOptions())

let pMi        = pTag "mi" (identifier |>> Mtag.Identifier) <!> "pMi"
let pMo        = pTag "mo" (pstr "+" <|> pstr "-" |>> Mtag.Operator) <!> "pMo"
let pMn        = pTag "mn" (pfloat |>> Mtag.Number) <!> "pMn"

let pMrowTags  = [pMi; pMo; pMn]

let pMrow      = pTag "mrow" (many <| choice pMrowTags |>> Mtag.Row) <!> "pMrow"
let pMstyle  p = pTag "mstyle" p <!> "pMstyle"
let pMathTag p = pTag "math" p <!> "pMathTag"

let pMainTags  = [pMrow; pMo; pMi; pMn]
let pMathML    = pMathTag (pMstyle (many <| choice pMainTags)) <!> "pMathML"




///////////////////////////////////////////
// Converting the Mtag type to our Terms
///////////////////////////////////////////

let rec convert (mtag : Mtag) : Term =
    match mtag with
    | Root mtag -> convert mtag
    | Fraction (numerator, denominator) -> Term.BinaryTerm (convert numerator, BinaryOp.Divide, convert denominator)
    | Row mtagList -> 
        let beforePlus = List.takeWhile (x == Operator "+") mtagList

    | Identifier str -> Term.TVariable str
    | Operator str -> Term.TVariable str
    | Number num -> 
        let constant = 
            if num = infinity then
                Constant.Infinity
            else if num = -infinity then
                Constant.NegativeInfinity
            else
                Constant.Real num
        Term.TConstant constant



let tests = 
    printfn "Parser tests:"
    printfn "----------------------------\n"
    let result = test pMathML "<math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n  <mstyle displaystyle=\"true\">\n  <mrow>  <mi> r </mi><mo>+</mo><mn>3</mn>\n </mrow> </mstyle>\n</math>"
    let term = 
        match result with
        | Some mtagList ->
            convert (List.item 0 mtagList) 
        | None ->
            Term.TConstant (Constant.Real 0.0)
    printfn "Term: %A" term
    printfn "\n----------------------------\n\n"

