﻿module Parser

open FParsec
open TRegex
open AlgebraProblemGenerator
open Utils
open System

let mathmltest = "<math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n  <mstyle displaystyle=\"true\">\n    <mi> r </mi>\n  </mstyle>\n</math>"



let test p str =
    match run p str with
    | Success(result, _, _) ->
        // printfn "Parse MathML\n==============\n Success: %A\n==============\n" result
        Some result                        
    | Failure(errorMsg, _, _) -> 
        // printfn "Parse MathML\n==============\n Failure: %A\n==============\n" errorMsg
        None


let mutable debug = false
let mutable debugIndent = 0

let indentSpaces number =
    String.replicate number " "

let indent = indentSpaces debugIndent

// Debug trace thing
let (<!>) (p: Parser<'a, 'b>) label =
    if debug then
        fun (stream : CharStream<'b>) ->
            printfn ""
            debugIndent <- debugIndent + 4
            printfn "%s %A: Entering %s with input: %s" (indentSpaces debugIndent) stream.Position label (stream.PeekString 1000) 
            let reply = p stream
            printfn "%s %A: Leaving %s (%A)" (indentSpaces debugIndent) stream.Position label reply.Status
            debugIndent <- debugIndent - 4
            reply
    else
        p

//let pstr = pstring
let pstr s = spaces >>. pstringCI s .>> spaces
let sstr s = spaces >>. skipStringCI s .>> spaces 

type Operator = 
| Plus
| Minus
| Multiply
| Divide
| Exponent
| Equals

type Mtag =
| Root of Mtag
| Fraction of Mtag * Mtag
| Sup of Mtag * Mtag  // power/exponent
| Sub of Mtag * Mtag  // good for things like log base 10 of whatever
| Row of Mtag list
| Fenced of Mtag  // parentheses are a fencing of a mrow element or of a single other element
| Identifier of string
| Operator of Operator
| Number of float
| Sqrt of Mtag
| Term of Term //TODO: fix ugly hack when you have brain, not like 2am me

// BTag = Begin Tag
// ETag = End Tag
// ITag = the insides between a BTag and a ETag

// returns the attributes of the tag:
let pBTag tagName  = between (sstr ("<" + tagName)) (sstr ">") (manyTill (noneOf ">") (lookAhead (sstr ">"))) <!> "pBTag" //TODO: watch out for '\>' before the actual end of the tag (>)
let pETag tagName  = between (sstr "</") (sstr ">") (pstr tagName) <!> "pETag"
let pTag tagName p = between (pBTag tagName) (pETag tagName) p <!> "pTag"


let mapIdentifier (identifier : string) =
    let trimmedIdent = identifier.Trim()
    match trimmedIdent with
    | "log" ->
        Mtag.Identifier "log"
    | "ln" ->
        Mtag.Identifier "ln"
    | _ ->
        Mtag.Identifier trimmedIdent
let pMi = pTag "mi" (charsTillString "</mi>" false 1000) |>> mapIdentifier <!> "pMi"//(many1Chars (lower <|> upper <|> digit <|> anyOf ['-'; '<'; '>'; '&'; '#'; ';'; ' '; '!'])) |>> mapIdentifier <!> "pMi"


let plusOperatorParser = choice [pstr "+"] |>> (fun _ -> Mtag.Operator Plus)
let minusOperatorParser = choice [pstr "-"] |>> (fun _ -> Mtag.Operator Minus)
let multiplyOperatorParser = choice [pstr "*"; pstr "&#x00B7; <!-- middle dot -->"; pstr "&#x00D7; <!-- multiplication sign -->"] |>> (fun _ -> Mtag.Operator Multiply)
let divideOperatorParser = choice [pstr "/"] |>> (fun _ -> Mtag.Operator Divide)
let exponentOperatorParser = choice [pstr "^"] |>> (fun _ -> Mtag.Operator Exponent)
let equalsOperatorParser = choice [pstr "="] |>> (fun _ -> Mtag.Operator Equals)


let operatorParsers = [plusOperatorParser; minusOperatorParser; multiplyOperatorParser; divideOperatorParser; exponentOperatorParser; equalsOperatorParser]

let pMo        = pTag "mo" (choice operatorParsers) <!> "pMo"
let pMn        = pTag "mn" (pfloat |>> Mtag.Number) <!> "pMn"

let pMathTag p = pTag "math" p <!> "pMathTag"

let pMrow, pMrowRef = createParserForwardedToRef()
let pMfenced, pMfencedRef = createParserForwardedToRef()
let pMsup, pMsupRef = createParserForwardedToRef()
let pMstyle, pMstyleRef = createParserForwardedToRef()
let pMfrac, pMfracRef = createParserForwardedToRef()
let pMsqrt, pMsqrtRef = createParserForwardedToRef()


let pMtag = choice [pMrow; pMo; pMi; pMn; pMfenced; pMsup; pMfrac; pMsqrt; pMstyle;]

do pMrowRef := pTag "mrow" (many pMtag) |>> Row <!> "pMrow"
do pMfencedRef := pTag "mfenced" (pMrow) |>> Fenced <!> "pMfenced"
do pMsupRef := pTag "msup" (tuple2 pMtag pMtag) |>> Sup <!> "pMsup"
do pMstyleRef := pTag "mstyle" pMtag <!> "pMstyle"
do pMfracRef := pTag "mfrac" (tuple2 pMtag pMtag) |>> Fraction <!> "pMfrac"
do pMsqrtRef := pTag "msqrt" (many pMtag) |>> (Row >> Sqrt) <!> "pMsqrt"

let pMathML = pMathTag (many pMtag) |>> (Row >> Root) <!> "pMathML"

let functionMap beforeParentheses : UnaryOp option = 
    match beforeParentheses with
    | Identifier str ->
        match str.Trim() with
        | "sin" -> Some <| Trig Sin
        | "cos" -> Some <| Trig Cos
        | "tan" -> Some <| Trig Tan
        | "cot" -> Some <| Trig Cot
        | "sec" -> Some <| Trig Sec
        | "csc" -> Some <| Trig Csc
        | _ -> None
    | _ -> None
    




let rec mtagToTerm (mtag : Mtag) : Term =
    match mtag with
    | Root mtag -> mtagToTerm mtag
    | Fraction (numerator, denominator) -> Term.BinaryTerm (mtagToTerm numerator, BinaryOp.Divide, mtagToTerm denominator)
    | Sub (above, below) -> mtagToTerm above //TODO: what even is this
    | Sup (base_, exponent) -> Term.BinaryTerm (mtagToTerm base_, BinaryOp.Exponent, mtagToTerm exponent)
    | Fenced mt -> Term.TFenced <| mtagToTerm mt //TODO: is this ok?
    // | Row mtagList when List.isEmpty mtagList -> 
    //     printfn "error"
    //     Term.TConstant (Real 0.0)
    | Row mtagList when List.length mtagList = 1 -> 
        mtagToTerm <| List.item 0 mtagList
    | Row mtagList ->
        let splitByEquals = split ((=) <| Operator Equals) mtagList
        if List.length splitByEquals > 1 then
            Term.AssociativeTerm (AssociativeOp.Equals, List.map (Row >> mtagToTerm) splitByEquals)
        else
            let splitByPlusMinus = split (fun x -> x = Operator Plus || x = Operator Minus) mtagList
            if List.length splitByPlusMinus > 1 then
                // gets an Mtag list of small expressions separated by plus & minus.
                // returns an Mtag list list, of lists of small expressions separated by Operator "+" (not included),
                // will negate relevant terms
                // then we List.concat (intersperse (Operator "+") lst)
                let rec negateRelevantParts (expression : Mtag list) (negFirstElem : bool) : Mtag list =
                    let splitByPlusMinus = split (fun x -> x = Operator Plus || x = Operator Minus) expression
                    match splitByPlusMinus with
                    | [] -> []
                    | [x] ->
                        match negFirstElem with
                        | true -> [negate x]
                        | false -> x
                    | x::xs -> 
                        let fixedX =
                            match negFirstElem with
                            | true -> [negate x]
                            | false -> x
                        let rest = 
                            negateRelevantParts (List.skip (List.length x + 1) expression) ((List.item (List.length x) expression) = Operator Minus)
                        if List.isEmpty fixedX then
                            rest
                        else
                            // if the operator after `x` is a minus, we'll negate the next section after it (calling recursively)
                            fixedX @ Operator Plus :: rest

                let noMinuses = negateRelevantParts mtagList false
                let lst = split ((=) <| Operator Plus) noMinuses
                if List.length lst = 1 then
                    mtagToTerm <| Mtag.Row (List.item 0 lst)
                else
                    Term.AssociativeTerm (AssociativeOp.Plus, List.map (Mtag.Row >> mtagToTerm) lst)
                
            else
                // no pluses or minuses in the expression
                let splitByMultiplyDivide = split (fun x -> x = Operator Multiply || x = Operator Divide) mtagList
                if List.length splitByMultiplyDivide > 1 then
                    // there are multiply and divide in the expression
                    let rec f lst =
                        let opIndex = List.tryFindIndexBack (fun x -> x = Operator Multiply || x = Operator Divide) lst
                        match opIndex with
                        | Some index ->
                            match List.item index lst with
                            | Operator Multiply ->
                                // keep looking for *
                                let notMultiplyIndex = List.tryFindIndexBack ((=) <| Operator Divide) lst
                                match notMultiplyIndex with
                                | Some ind -> 
                                    let x, y = List.splitAt ind lst
                                    Term.BinaryTerm (Term.AssociativeTerm(AssociativeOp.Multiply, List.rev (List.map mtagToTerm x)), BinaryOp.Divide, f y) 
                                | None ->
                                    let operands = split ((=) <| Operator Multiply) lst
                                    Term.AssociativeTerm(AssociativeOp.Multiply, List.map (Mtag.Row >> mtagToTerm) operands)
                            | Operator Divide ->
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
                    match mtagList with
                    | [] -> TConstant (Real 0.0)
                    | [x] -> mtagToTerm x
                    | _ ->
                        let isFenced x =
                            match x with
                            | Fenced _ -> true
                            | _ -> false
                        let rec f mtagList =
                            let parenIndex = List.tryFindIndex isFenced mtagList
                            match parenIndex with
                            | Some index when index > 0 ->
                                let funcOpt = functionMap <| List.item (index - 1) mtagList
                                let firstTerm =
                                    match funcOpt with
                                    | Some func ->
                                        Term.UnaryTerm (func, mtagToTerm <| List.item index mtagList)
                                    | None ->
                                        //printfn "no such function: %A" (List.item (index - 1) mtagList)
                                        //let item = mtagToTerm <| List.item (index - 1) mtagList
                                        mtagToTerm (Mtag.Row <| intersperse (Operator Multiply) mtagList)

                                firstTerm // TODO: TODOTOTDOTODTODTO see discord for problems with this shit (recursivity and wrongness)
                            | _ ->
                                mtagToTerm (Mtag.Row <| intersperse (Operator Multiply) mtagList) // if we have something like 3xy, make it 3*x*y, side effect: <mn>3</mn><mn>4</mn> -> 3*4, doesn't seem harmful
                        f mtagList
    | Identifier str ->
        match str.Trim() with
        | "sin" -> UnaryTerm (Trig Sin, TVariable "str")
        | "cos" -> UnaryTerm (Trig Sin, TVariable "str")
        | "&#x03C0; <!-- greek small letter pi -->" -> TConstant (Constant.Real Math.PI)
        | "e" -> TConstant (Constant.Real Math.E)
        | _ -> Term.TVariable str
    | Operator op ->
        printfn "shouldn't happen, operator str"
        Term.TVariable "Operator"
    | Number num -> 
        let constant = 
            if num = infinity then
                Constant.Infinity
            else if num = -infinity then
                Constant.NegativeInfinity
            else
                Constant.Real num
        Term.TConstant constant
    | Sqrt arg ->
        Term.UnaryTerm (UnaryOp.Sqrt, mtagToTerm arg)
    | Term t -> t
and negate (mt : Mtag list) : Mtag =
    match mt with
    | [x] ->
        match x with
        | Number n -> Mtag.Number -n
        | Identifier i -> Mtag.Term (Term.UnaryTerm (Negative, Term.TVariable i))
        | someMtag -> Mtag.Term (Term.UnaryTerm (Negative, mtagToTerm someMtag))
    | xs -> Mtag.Term <| Term.UnaryTerm (Negative, mtagToTerm (Mtag.Row xs))




let termToMtag (term : Term) =
    let rec termToMtagRec (term : Term) =
        match term with
        | TConstant tc ->
            match tc with
            | Infinity -> Mtag.Number System.Double.PositiveInfinity // TODO: verify infinities work like that in mathml
            | NegativeInfinity -> Mtag.Number System.Double.NegativeInfinity
            | Real r -> 
                if r >= 0.0 then
                    Mtag.Number r
                else
                    Mtag.Row [Mtag.Operator Minus; Mtag.Number (abs r)]
        | TVariable var -> Mtag.Identifier var
        | UnaryTerm (uop, t1) ->
            match uop with
            | Negative ->
                Row [Operator Minus; termToMtagRec t1]
            | NaturalLog ->
                Row [Identifier "ln"; termToMtagRec t1]
            | Log base_ ->
                Row [Sub (Identifier "log", Number (unboxConstant base_)); termToMtagRec t1]
            | UnaryOp.Sqrt ->
                Sqrt <| termToMtagRec t1
            | Trig tr ->
                Mtag.Row [Identifier <| trigToStr tr; Mtag.Fenced (termToMtagRec t1)];
            | InvTrig it -> // sin^-1
                let func =
                    match it with
                    | Arcsin -> "sin"
                    | Arccos -> "cos"
                    | Arctan -> "tan"
                    | Arccot -> "cot"
                    | Arcsec -> "sec"
                    | Arccsc -> "csc"
                Row [Sup (Identifier func, Number -1.0); Mtag.Fenced (termToMtagRec t1)]
        | BinaryTerm (t1, bop, t2) ->
            match bop with
            | BinaryOp.Exponent -> 
                Sup (termToMtagRec t1, termToMtagRec t2)
            | BinaryOp.Divide ->
                Fraction (termToMtagRec t1, termToMtagRec t2)
        | AssociativeTerm (aop, termList) ->
            match aop with
            | AssociativeOp.Plus ->
                Row <| Utils.intersperse (Mtag.Operator Plus) (List.map termToMtagRec termList)
            | AssociativeOp.Multiply ->
                Row <| Utils.intersperse (Mtag.Operator Multiply) (List.map termToMtagRec termList)
            | AssociativeOp.Equals ->
                Row <| Utils.intersperse (Mtag.Operator Equals) (List.map termToMtagRec termList)
        | TFenced term ->
            Fenced <| termToMtagRec term
        | _ ->
            Number 999999999999999.0
    let mtag = termToMtagRec term

    let isOperator (mtag: Mtag) =
        match mtag with
        | Operator _ ->
            true
        | _ -> false
    // some post-processing, like removing unnecessary operators (x+-3 -> x-3, (-3) -> - 3 (because that's how myscript wants it))
    let rec postProcessMtag (mtag:Mtag) : Mtag =
        match mtag with
        | Row mtList ->
            let rec removeUnnecessaryRows (mtagList: Mtag list) : Mtag list =
                match mtagList with
                | [] -> []
                | x::xs ->
                    match x with
                    | Row mList ->
                        removeUnnecessaryRows mList @ removeUnnecessaryRows xs
                    | _ ->
                        x :: removeUnnecessaryRows xs
            //printfn "before:\n %A" mtList
            let mtagList = removeUnnecessaryRows mtList
            //printfn "afterRows:\n %A" mtagList
            let operatorClusters = List.filter (List.isEmpty >> not) <| split (isOperator >> not) mtagList
            let rec fixClusters opClusters =
                let rec fixOpCluster opCluster (acc: Mtag list) =
                    match opCluster with
                    | [] -> acc
                    | x::xs ->
                        match x with
                        | Operator Minus ->
                            match List.tryLast acc with
                            | Some (Operator Plus) ->
                                fixOpCluster xs (List.take (List.length acc - 1) acc @ [Operator Minus])
                            | Some (Operator Minus) ->
                                fixOpCluster xs (List.take (List.length acc - 1) acc @ [Operator Plus])
                            | _ -> 
                                fixOpCluster xs (acc @ [x])
                        | Operator Plus ->
                            match List.tryLast acc with
                            | Some (Operator Plus) ->
                                fixOpCluster xs acc
                            | Some (Operator Minus) ->
                                fixOpCluster xs acc
                            | _ ->
                                fixOpCluster xs (acc @ [x])
                        | _ ->
                            fixOpCluster xs (acc @ [x])
                        
                               
                
                match opClusters with
                | [] -> []
                | x::xs ->
                    fixOpCluster x [] :: fixClusters xs
            let fixedClusters = fixClusters operatorClusters
            let notOpClusters = List.filter (List.isEmpty >> not) <| List.map (fun x -> List.map postProcessMtag x) (split isOperator mtagList)
            
            if isOperator (List.item 0 mtagList) then
                let fixedMtagList = List.concat (Utils.merge fixedClusters notOpClusters)
                //printfn "fixedMtagList:\n%A" fixedMtagList
                Mtag.Row fixedMtagList 
            else
                let fixedMtagList = List.concat (Utils.merge notOpClusters fixedClusters)
                //printfn "fixedMtagList:\n%A" fixedMtagList
                Mtag.Row fixedMtagList 

            
        | Number flt -> Number flt //termToMtagRec (Term.UnaryTerm (Negative, TConstant (Real (abs flt))))
        | Fraction (mt1, mt2) -> Fraction (postProcessMtag mt1, postProcessMtag mt2)
        | Sup (mt1, mt2) -> Sup (postProcessMtag mt1, postProcessMtag mt2)
        | Sub (mt1, mt2) -> Sub (postProcessMtag mt1, postProcessMtag mt2)
        | Sqrt mt -> Sqrt (postProcessMtag mt)
        | Root mt -> Root (postProcessMtag mt)
        | Fenced mt -> Fenced (postProcessMtag mt)
        | Term t -> postProcessMtag (termToMtagRec t)
        | _ -> mtag
    let fixedMtag = postProcessMtag <| termToMtagRec term    
    //printfn "fixedMtag:\n %A" fixedMtag
    Root <| fixedMtag

let mathMLtag tag insides =
    "<" + tag + ">" + insides + "</" + tag + ">"

let rec mtagToMathML (mtag : Mtag) =
    match mtag with
    | Identifier str ->
        mathMLtag "mi" str
    | Operator op ->
        let strOp = 
            match op with
            | Plus -> "+"
            | Minus -> "-"
            | Multiply -> "&#x00B7; <!-- middle dot -->" //TODO: middle-dot?
            | Divide -> "/"
            | Exponent -> "^"
            | Equals -> "="
        mathMLtag "mo" strOp
    | Number f ->
        mathMLtag "mn" <| string f
    | Fraction (x, y) ->
        mathMLtag "mfrac" <| mtagToMathML x + mtagToMathML y
    | Fenced mt ->
        mathMLtag "mfenced" <| mtagToMathML mt
    | Root mt ->
        "<math xmlns='http://www.w3.org/1998/Math/MathML'>" + mtagToMathML mt + "</math>"
    | Row mtList ->
        let list = List.fold (+) "" (List.map mtagToMathML mtList)
        mathMLtag "mrow" <| list
    | Sub (above, below) ->
        mathMLtag "msub" <| mtagToMathML above + mtagToMathML below
    | Sup (base_, exponent) ->
        mathMLtag "msup" <| mtagToMathML base_ + mtagToMathML exponent
    | Sqrt (arg) ->
        mathMLtag "msqrt" <| mtagToMathML arg
    | Term term ->
        mtagToMathML <| termToMtag term

let mathmlToMtag (mathML : string) : Mtag option =
    test pMathML mathML

let term (mathML : string) =
    let parsedResult = mathmlToMtag mathML
    match parsedResult with
    | Some mtag ->
        Some <| mtagToTerm mtag
    | None ->
        None


let parserTests =
    // printfn "Parser tests:"
    // printfn "----------------------------\n"

    let mathMLStrings = [
        //"<math xmlns='http://www.w3.org/1998/Math/MathML'>\n  <mi> x </mi>\n  <mo> + </mo>\n  <mn> 3 </mn>\n  <mo> - </mo>\n  <mn> 7 </mn>\n  <mo> - </mo>\n  <mn> 4 </mn>\n  <mo> + </mo>\n  <mn> 6 </mn>\n  <mo> - </mo>\n  <mn> 4 </mn>\n</math>\n"
        // "<math xmlns='http://www.w3.org/1998/Math/MathML'>\n  <mfenced>\n    <mrow>\n      <mi> x </mi>\n      <mo> - </mo>\n      <msup>\n        <mrow>\n          <mfenced>\n            <mrow>\n              <msup>\n                <mrow>\n                  <mi> y </mi>\n                </mrow>\n                <mrow>\n                  <mn> 2 </mn>\n                </mrow>\n              </msup>\n              <mo> + </mo>\n              <mn> 3 </mn>\n            </mrow>\n          </mfenced>\n        </mrow>\n        <mrow>\n          <mn> 2 </mn>\n        </mrow>\n      </msup>\n    </mrow>\n  </mfenced>\n  <mo> - </mo>\n  <mn> 7 </mn>\n  <mfenced>\n    <mrow>\n      <mi> x </mi>\n      <mo> + </mo>\n      <mn> 3 </mn>\n    </mrow>\n  </mfenced>\n</math>\n"
        //"<math xmlns='http://www.w3.org/1998/Math/MathML'>\n  <mfenced>\n    <mrow>\n      <msup>\n        <mrow>\n          <mi> x </mi>\n        </mrow>\n        <mrow>\n          <mn> 2 </mn>\n        </mrow>\n      </msup>\n      <mo> - </mo>\n      <mn> 3 </mn>\n    </mrow>\n  </mfenced>\n  <mo> + </mo>\n  <mfenced>\n    <mrow>\n      <mi> x </mi>\n      <mo> - </mo>\n      <mn> 3 </mn>\n    </mrow>\n  </mfenced>\n</math>\n"
        //"<math xmlns='http://www.w3.org/1998/Math/MathML'>\n  <mn> 3 </mn>\n  <mo> &#x00B7; <!-- middle dot --> </mo>\n  <mo> - </mo>\n  <mn> 3 </mn>\n  <msup>\n    <mrow>\n      <mi> x </mi>\n    </mrow>\n    <mrow>\n      <mn> 2 </mn>\n    </mrow>\n  </msup>\n  <mo> + </mo>\n  <mn> 7 </mn>\n  <msup>\n    <mrow>\n      <mi> x </mi>\n    </mrow>\n    <mrow>\n      <mn> 3 </mn>\n    </mrow>\n  </msup>\n  <mo> - </mo>\n  <mn> 3 </mn>\n  <mo> + </mo>\n  <msup>\n    <mrow>\n      <mi> e </mi>\n    </mrow>\n    <mrow>\n      <mn> 2 </mn>\n      <msup>\n        <mrow>\n          <mi> x </mi>\n        </mrow>\n        <mrow>\n          <mn> 2 </mn>\n        </mrow>\n      </msup>\n    </mrow>\n  </msup>\n</math>\n"
        // "<math xmlns='http://www.w3.org/1998/Math/MathML'>\n  <msup>\n    <mrow>\n      <mi> x </mi>\n    </mrow>\n    <mrow>\n      <mn> 2 </mn>\n    </mrow>\n  </msup>\n  <mo> - </mo>\n  <mn> 3 </mn>\n  <msup>\n    <mrow>\n      <mi> x </mi>\n    </mrow>\n    <mrow>\n      <mn> 2 </mn>\n    </mrow>\n  </msup>\n  <mo> + </mo>\n  <mn> 7 </mn>\n  <msup>\n    <mrow>\n      <mi> x </mi>\n    </mrow>\n    <mrow>\n      <mn> 3 </mn>\n    </mrow>\n  </msup>\n  <mo> - </mo>\n  <mn> 3 </mn>\n</math>\n"
        // "<math xmlns='http://www.w3.org/1998/Math/MathML'>\n  <mn> 5 </mn>\n  <mi> x </mi>\n  <mo> - </mo>\n  <mn> 3 </mn>\n  <mo> - </mo>\n  <mn> 4 </mn>\n  <mi> x </mi>\n  <mo> - </mo>\n  <mn> 2 </mn>\n  <mo> - </mo>\n  <mn> 3 </mn>\n  <mo> + </mo>\n  <mn> 7 </mn>\n</math>\n"
        // "<math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n  <mrow>  <mn>2</mn><mi>z</mi><mi> r </mi><mo>-</mo><mn>3</mn><mo>-</mo><mi>b</mi><mo>/</mo><mi>b</mi><mo>/</mo><mi>b</mi><mo>-</mo><mi>b</mi>\n </mrow> \n</math>"; 
        // "<math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n  <mstyle displaystyle=\"true\">\n  <mrow><mi>x</mi><mo>-</mo><mi>y</mi> </mrow> </mstyle>\n</math>"
    ]

    let results = List.map term mathMLStrings

    List.map (printfn "Parsed Term: %A") results
    let mtags = List.map (Option.map termToMtag) results
    List.map (printfn "To Mtag: %A") mtags
    let mathmls = List.map (Option.map mtagToMathML) mtags
    List.map (printfn "To MathML: %A") mathmls

    // printfn "\n----------------------------\n\n"
    results


///////////////////////////////// TREGEX PARSER

// A # B
//    A dominates B
// A < B 
//    A immediately dominates B
// A $ B 
//    A is a sister of B (and not equal to B)

// A precedes B if they are both children of the same node and A is leftmost

// A , B 
//    A precedes B 
// A . B 
//    A immediately precedes B

let pTRelation = choice [attempt <| pstr "#" >>% Relation.Descendant <!> "#";
                         attempt <| pstr "<" >>% Relation.DirectDescendant <!> "<";
                         attempt <| pstr "$" >>% Relation.Sibling <!> "$";
                         attempt <| pstr "," >>% Relation.Precedent <!> ",";
                         attempt <| pstr "." >>% Relation.ImmediatePrecedent <!> "."] <!> "pTRelation"



let pTConstant = choice [pfloat |>> Constant.Real;
                         sstr "e" >>% Constant.Real System.Math.E;
                         sstr "pi" >>% Constant.Real System.Math.PI;
                         sstr "inf" >>% Constant.Infinity;
                         sstr "ninf" >>% Constant.NegativeInfinity] <!> "pTConstant"
let pTVariable = identifier (IdentifierOptions (normalizeBeforeValidation = true))  <!> "pTVariable"

let pTrig = choice [ pstr "sin" >>% Trig.Sin; pstr "cos" >>% Trig.Cos; pstr "tan" >>% Trig.Tan; pstr "cot" >>% Trig.Cot; pstr "sec" >>% Trig.Sec; pstr "csc" >>% Trig.Csc] <!> "pTrig"
let pInvTrig = choice [ pstr "asin" >>% InvTrig.Arcsin; pstr "acos" >>% InvTrig.Arccos; pstr "atan" >>% InvTrig.Arctan; pstr "acot" >>% InvTrig.Arccot; pstr "asec" >>% InvTrig.Arcsec; pstr "acsc" >>% InvTrig.Arccsc] <!> "pInvTrig"
let pLog = pstr "log" >>. pTConstant |>> UnaryOp.Log <!> "pLog"
let pUnaryOp = choice [pstr "-" >>% UnaryOp.Negative <!> "negative"; pstr "ln" >>% UnaryOp.NaturalLog <!> "ln"; pLog; pstr "sqrt" >>% UnaryOp.Sqrt <!> "sqrt"; pTrig |>> UnaryOp.Trig; pInvTrig |>> UnaryOp.InvTrig] <!> "pUnaryOp"

let pBinaryOp = choice [pstr "/" >>% BinaryOp.Divide <!> "divide"; pstr "^" >>% BinaryOp.Exponent <!> "exponent"] <!> "pBinaryOp"
let pAssociativeOp = choice [pstr "+" >>% AssociativeOp.Plus <!> "plus"; pstr "*" >>% AssociativeOp.Multiply <!> "multiply"; pstr "=" >>% AssociativeOp.Equals <!> "equals"] <!> "pAssociativeOp"
let pFenced = pstr "[]"
let pNodeValue = choice [pUnaryOp |>> NodeValue.UnaryOp; pFenced >>% NodeValue.Fenced; pBinaryOp |>> NodeValue.BinaryOp; pAssociativeOp |>> NodeValue.AssociativeOp; pTConstant |>> NodeValue.Constant; pTVariable |>> NodeValue.Variable] <!> "pNodeValue"

let (pTRegex : Parser<TRegex, unit>), pTRegexRef = createParserForwardedToRef()

do pTRegexRef := pipe2
                    pNodeValue
                    (opt (many1 (
                                    tuple2 pTRelation (
                                                        (between (pstr "(") (pstr ")") pTRegex <!> "fenced tregex") <|> (pTRegex <!> "normal tregex")
                                                      )
                                )
                         )
                    )
                    (
                    fun dominant optionalListOfRelationAndTRegex ->
                        {
                            dominant = dominant;
                            subjects = optionalListOfRelationAndTRegex
                        }
                    ) <!> "pTRegex"