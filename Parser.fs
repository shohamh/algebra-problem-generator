module Parser

open FParsec
open AlgebraProblemGenerator
open System.Threading

let mathmltest = "<math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n  <mstyle displaystyle=\"true\">\n    <mi> r </mi>\n  </mstyle>\n</math>"



let test p str =
    match run p str with
    | Success(result, _, _) ->
        printfn "Parse MathML\n==============\n Success: %A\n==============\n" result
        Some result                        
    | Failure(errorMsg, _, _) -> 
        printfn "Parse MathML\n==============\n Failure: %A\n==============\n" errorMsg
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
| Term of Term //TODO: fix ugly hack when you have brain, not like 2am me

// BTag = Begin Tag
// ETag = End Tag
// ITag = the insides between a BTag and a ETag

// returns the attributes of the tag:
let pBTag tagName  = between (sstr ("<" + tagName)) (sstr ">") (manyTill (noneOf ">") (lookAhead (sstr ">"))) <!> "pBTag" //TODO: watch out for '\>' before the actual end of the tag (>)
let pETag tagName  = between (sstr "</") (sstr ">") (pstr tagName) <!> "pETag"
let pTag tagName p = between (pBTag tagName) (pETag tagName) p <!> "pTag"

let identifier = identifier(IdentifierOptions())

let pMi        = pTag "mi" (identifier |>> Mtag.Identifier) <!> "pMi"

let operatorParsers = [pstr "+"; pstr "-"; pstr "*"; pstr "/"]
let pMo        = pTag "mo" (choice operatorParsers |>> Mtag.Operator) <!> "pMo"
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
//TODO: List.partition instead of takeWhile and skip
let split (predicate: 'T -> bool) (lst: 'T list) : ('T list) list =
    let rec splitUtil acc lst =
        let beforePredicate = List.takeWhile (predicate >> not) lst
        let afterPredicate = List.skip beforePredicate.Length lst
        match afterPredicate with
        | x::rest when predicate x -> splitUtil (beforePredicate::acc) rest // x is the element to split with
        | _ -> beforePredicate::acc
    List.rev <| splitUtil [] lst

// shim an element between each pair of elements in a list
//TODO: test this shit
let rec shim (elem: 'T) (lst: 'T list) =
    match lst with
    | [] -> []
    | [x] -> [x]
    | x::xs -> x::elem::(shim elem xs)

let rec mtagToTerm (mtag : Mtag) : Term =
    match mtag with
    | Root mtag -> mtagToTerm mtag
    | Fraction (numerator, denominator) -> Term.BinaryTerm (mtagToTerm numerator, BinaryOp.Divide, mtagToTerm denominator)
    | Row mtagList when List.length mtagList = 1 -> 
        mtagToTerm <| List.item 0 mtagList
    | Row mtagList ->
        let splitByPlusMinus = split (fun x -> x = Operator "+" || x = Operator "-") mtagList
        if List.length splitByPlusMinus = 1 then
            // no plus or minuses in the expression
            let splitByMultiplyDivide = split (fun x -> x = Operator "*" || x = Operator "/") mtagList
            if List.length splitByMultiplyDivide = 1 then
                // no multiply or divide in the expression, add multiplication between what is there
                let lst = List.item 0 splitByMultiplyDivide 
                match lst with
                | [x] -> mtagToTerm x
                | _ -> mtagToTerm (Mtag.Row <| shim (Operator "*") lst) // if we have something like 3xy, make it 3*x*y, side effect: <mn>3</mn><mn>4</mn> -> 3*4, doesn't seem harmful
            else
                // only multiply and divide in the expression
                let rec f lst =
                    let opIndex = List.tryFindIndexBack (fun x -> x = Operator "*" || x = Operator "/") lst
                    match opIndex with
                    | Some index ->
                        match List.item index lst with
                        | Operator "*" ->
                            // keep looking for *
                            let notMultiplyIndex = List.tryFindIndexBack (fun x -> x = Operator "/") lst
                            match notMultiplyIndex with
                            | Some ind -> 
                                let x, y = List.splitAt ind lst
                                Term.BinaryTerm (Term.AssociativeTerm(Multiply, List.rev (List.map mtagToTerm x)), Divide, f y) 
                            | None ->
                                let operands = split (fun x -> x = Operator "*") lst
                                // TODO: prove that there has to be one operand in each sublist from split,
                                //       maybe switch to Mtag.Row'ing them and deal with it in the other match case for Mtag.Row (len 1)
                                Term.AssociativeTerm(Multiply, List.map (List.item 0 >> mtagToTerm) operands)
                        | Operator "/" ->
                            let beforeDivide, afterDivide = List.splitAt index lst
                            let afterDivideWithoutOp = List.skip 1 afterDivide
                            Term.BinaryTerm (mtagToTerm (Mtag.Row beforeDivide), BinaryOp.Divide, f afterDivideWithoutOp)
                        | _ ->
                            printfn "ERROR: found unexpected operator when looking for multiplication and division"
                            Term.TConstant(Real 999999999.9)
                    | None ->
                        mtagToTerm <| Mtag.Row lst
                f mtagList
        else // no plus in row
            let splitByMinus = split (fun x -> x = Operator "-") mtagList
            let firstElement = List.item 0 splitByMinus
            let afterMinus = List.skip 1 splitByMinus
            let mapping x =
                match x with
                | [] -> []
                | y::ys ->
                    let z = 
                        match y with
                        | Number n -> Number -n
                        | Identifier i -> Mtag.Term (Term.UnaryTerm (Negative, Term.TVariable i))
                        // | Fraction (x, y) -> Mtag.Term (Term.UnaryTerm (Negative, Term.BinaryTerm())) //TODO
                    z::ys
            let afterMinusFixed = List.map mapping afterMinus
            let finalList = firstElement :: afterMinusFixed
            let t = split (fun x -> x = Operator "+") (List.concat (shim [Operator "+"] finalList))
            Term.AssociativeTerm (AssociativeOp.Plus, List.map (Mtag.Row >> mtagToTerm) t)
        
        //Term.AssociativeTerm (AssociativeOp.Plus, List.map convert splitByPlus)
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
    | Term t -> t

let term (mathML : string) =
    let parsedResult = test pMathML mathML
    match parsedResult with
    | Some mtagList ->
        Some <| mtagToTerm (List.item 0 mtagList)
    | None ->
        None


let tests = 
    printfn "Parser tests:"
    printfn "----------------------------\n"

    let mathMLStrings = [
        "<math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n  <mstyle displaystyle=\"true\">\n  <mrow>  <mn>2</mn><mi>z</mi><mi> r </mi><mo>-</mo><mn>3</mn><mo>-</mo><mi>b</mi><mo>/</mo><mi>b</mi><mo>/</mo><mi>b</mi><mo>-</mo><mi>b</mi>\n </mrow> </mstyle>\n</math>"; 
        "<math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n  <mstyle displaystyle=\"true\">\n  <mrow><mi>x</mi><mo>-</mo><mi>y</mi> </mrow> </mstyle>\n</math>"
    ]

    let results = List.map term mathMLStrings

    List.map (printfn "Parsed Term: %A") results

    printfn "\n----------------------------\n\n"
    results

