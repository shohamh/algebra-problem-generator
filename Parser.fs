module Parser

open FParsec
open AlgebraProblemGenerator
open System.Threading
open NUnit.Framework
open FsUnit
open Utils
open System.Runtime.InteropServices.ComTypes

let mathmltest = "<math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n  <mstyle displaystyle=\"true\">\n    <mi> r </mi>\n  </mstyle>\n</math>"



let test p str =
    match run p str with
    | Success(result, _, _) ->
        printfn "Parse MathML\n==============\n Success: %A\n==============\n" result
        Some result                        
    | Failure(errorMsg, _, _) -> 
        printfn "Parse MathML\n==============\n Failure: %A\n==============\n" errorMsg
        None


let debug = true

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
| Sup of Mtag * Mtag  // power/exponent
| Sub of Mtag * Mtag
| Row of Mtag list
| Fenced of Mtag  // parentheses are a fencing of a mrow element or of a single other element
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

let operatorParsers = [pstr "+"; pstr "-"; pstr "*"; pstr "/"; pstr "^"]
let pMo        = pTag "mo" (choice operatorParsers |>> Mtag.Operator) <!> "pMo"
let pMn        = pTag "mn" (pfloat |>> Mtag.Number) <!> "pMn"

let pMathTag p = pTag "math" p <!> "pMathTag"

let pMrow, pMrowRef = createParserForwardedToRef()
let pMfenced, pMfencedRef = createParserForwardedToRef()
let pMsup, pMsupRef = createParserForwardedToRef()
let pMstyle, pMstyleRef = createParserForwardedToRef()


let pMtag = choice [pMrow; pMstyle; pMsup; pMfenced; pMo; pMi; pMn]

do pMrowRef := pTag "mrow" (many pMtag) |>> Mtag.Row <!> "pMrow"
do pMfencedRef := pTag "mfenced" (pMrow) |>> Mtag.Fenced <!> "pMfenced"
do pMsupRef := pTag "msup" (tuple2 pMtag pMtag) |>> Mtag.Sup <!> "pMsup"
do pMstyleRef := pTag "mstyle" pMtag <!> "pMstyle"

let pMathML    = pMathTag (many pMtag) |>> (fun x -> Mtag.Root (Mtag.Row x)) <!> "pMathML"

let split (predicate: 'T -> bool) (lst: 'T list) : ('T list) list =
    let rec splitUtil acc lst =
        let beforePredicate = List.takeWhile (predicate >> not) lst
        let afterPredicate = List.skip beforePredicate.Length lst
        match afterPredicate with
        | x::rest when predicate x -> splitUtil (beforePredicate::acc) rest // x is the element to split with
        | _ -> beforePredicate::acc
    List.rev <| splitUtil [] lst

let rec mtagToTerm (mtag : Mtag) : Term =
    match mtag with
    | Root mtag -> mtagToTerm mtag
    | Fraction (numerator, denominator) -> Term.BinaryTerm (mtagToTerm numerator, BinaryOp.Divide, mtagToTerm denominator)
    | Sub (above, below) -> mtagToTerm above//TODO: what even is this
    | Sup (base_, exponent) -> Term.BinaryTerm (mtagToTerm base_, BinaryOp.Exponent, mtagToTerm exponent)
    | Fenced mt -> mtagToTerm mt //TODO: is this ok?
    | Row mtagList when List.length mtagList = 1 -> 
        mtagToTerm <| List.item 0 mtagList
    | Row mtagList ->
        let splitByPlusMinus = split (fun x -> x = Operator "+" || x = Operator "-") mtagList
        if List.length splitByPlusMinus > 1 then
            // gets an Mtag list of small expressions separated by plus & minus.
            // returns an Mtag list list, of lists of small expressions separated by Operator "+" (not included),
            // will negate relevant terms
            // then we List.concat (intersperse (Operator "+") lst)
            let rec negateRelevantParts (expression : Mtag list) (negateNext : bool) : Mtag list =
                let splitByPlusMinus = split (fun x -> x = Operator "+" || x = Operator "-") expression
                match splitByPlusMinus with
                | [] -> []
                | [x] -> x
                | x::xs -> 
                    let fixedX =
                        match negateNext with
                        | true -> negate x
                        | false -> Mtag.Row x
                    match List.item (List.length x) expression with
                    | Operator "-" ->
                        fixedX :: Operator "+" :: (negateRelevantParts (List.skip (List.length x + 1) expression) true)
                    | Operator op ->
                        fixedX :: Operator op :: (negateRelevantParts (List.skip (List.length x + 1) expression) false)

            let noMinuses = negateRelevantParts mtagList false
            let lst = split (fun x -> x = Operator "+") noMinuses
            Term.AssociativeTerm (AssociativeOp.Plus, List.map (Mtag.Row >> mtagToTerm) lst)
            
        else
            // no pluses or minuses in the expression
            let splitByMultiplyDivide = split (fun x -> x = Operator "*" || x = Operator "/") mtagList
            if List.length splitByMultiplyDivide > 1 then
                // there are multiply and divide in the expression
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
            else
                // no multiply or divide (or multiply or divide) in the expression, add multiplication between what is there (i.e. 3xy = 3*x*y)
                let lst = List.item 0 splitByMultiplyDivide 
                match lst with
                | [x] -> mtagToTerm x
                | _ -> mtagToTerm (Mtag.Row <| intersperse (Operator "*") lst) // if we have something like 3xy, make it 3*x*y, side effect: <mn>3</mn><mn>4</mn> -> 3*4, doesn't seem harmful
            
                
        
    | Identifier str -> Term.TVariable str
    | Operator str ->
        printfn "shouldn't happen, operator str"
        Term.TVariable str
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
and negate (mt : Mtag list) : Mtag =
    match mt with
    | [x] ->
        match x with
        | Number n -> Mtag.Number -n
        | Identifier i -> Mtag.Term (Term.UnaryTerm (Negative, Term.TVariable i))
        // | Fraction (x, y) -> Mtag.Term (Term.UnaryTerm (Negative, Term.BinaryTerm())) //TODO
    | xs -> Mtag.Term <| Term.UnaryTerm (Negative, mtagToTerm (Mtag.Row xs))




let rec termToMtag (term : Term) =
    match term with
    | TConstant tc ->
        match tc with
        | Infinity -> Mtag.Number System.Double.PositiveInfinity
        | NegativeInfinity -> Mtag.Number System.Double.NegativeInfinity
        | Real r -> Mtag.Number r
    | TVariable var -> Mtag.Identifier var
    | UnaryTerm (uop, t1) ->
        match uop with
        | Negative ->
            Mtag.Row [Mtag.Operator "-"; termToMtag t1]
        // | NaturalLog ->
        // | Sqrt ->
        // | Square ->
        // | Trig tr ->
        // | InvTrig it ->
    | BinaryTerm (t1, bop, t2) ->
        match bop with
        | BinaryOp.Multiply ->
            Mtag.Row [termToMtag t1; Mtag.Operator "*"; termToMtag t2]
        | Divide -> // TODO: Fraction
            Mtag.Row [termToMtag t1; Mtag.Operator "/"; termToMtag t2]
        | Exponent -> //TODO: sup (super, above)
            Mtag.Row [termToMtag t1; Mtag.Operator "^"; termToMtag t2]
    | AssociativeTerm (aop, termList) ->
        match aop with
        | Plus ->
            Mtag.Row <| Utils.intersperse (Mtag.Operator "+") (List.map termToMtag termList)
        | Multiply ->
            Mtag.Row <| Utils.intersperse (Mtag.Operator "*") (List.map termToMtag termList)
    | _ ->
        Mtag.Number 0.0

let mtagToMathML (mtag : Mtag) =
    ""

let term (mathML : string) =
    let parsedResult = test pMathML mathML
    match parsedResult with
    | Some mtag ->
        Some <| mtagToTerm mtag
    | None ->
        None


let tests =
    printfn "bb"
    printfn "Parser tests:"
    printfn "----------------------------\n"

    let mathMLStrings = [
        "<math xmlns='http://www.w3.org/1998/Math/MathML'>\n  <mn> 5 </mn>\n  <mi> x </mi>\n  <mo> - </mo>\n  <mn> 3 </mn>\n  <mo> - </mo>\n  <mn> 4 </mn>\n  <mi> x </mi>\n  <mo> - </mo>\n  <mn> 2 </mn>\n  <mo> - </mo>\n  <mn> 3 </mn>\n  <mo> + </mo>\n  <mn> 7 </mn>\n</math>\n"
        "<math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n  <mrow>  <mn>2</mn><mi>z</mi><mi> r </mi><mo>-</mo><mn>3</mn><mo>-</mo><mi>b</mi><mo>/</mo><mi>b</mi><mo>/</mo><mi>b</mi><mo>-</mo><mi>b</mi>\n </mrow> \n</math>"; 
        "<math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n  <mstyle displaystyle=\"true\">\n  <mrow><mi>x</mi><mo>-</mo><mi>y</mi> </mrow> </mstyle>\n</math>"
    ]

    let results = List.map term mathMLStrings

    List.map (printfn "Parsed Term: %A") results

    printfn "\n----------------------------\n\n"
    results
