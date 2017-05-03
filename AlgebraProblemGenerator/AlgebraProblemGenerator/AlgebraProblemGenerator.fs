
open FParsec

module AlgebraProblemGenerator = 
    type BinaryOp =
    | Plus
    | Minus
    | Multiply
    | Divide
    | Exponent


    type Trig =
    | Sin
    | Cos
    | Tan
    | Cot
    | Sec
    | Csc

    type InvTrig =
    | Arcsin
    | Arccos
    | Arctan
    | Arccot
    | Arcsec
    | Arccsc


    type UnaryOp =
    | Negative
    | Square
    | NaturalLog
    | Sqrt
    | Trig of Trig
    | InvTrig of InvTrig


    type Constant =
    | Infinity
    | Real of float

    type Variable = string

    type Term =
    | Constant of Constant
    | Variable of Variable
    | UnaryTerm of UnaryOp * Term
    | BinaryTerm of Term * BinaryOp * Term
    | Differential of Variable * Term // Term differentiated by variable
    | IndefiniteIntegral of Variable * Term // Term integrated by variable
    | DefiniteIntegral of Variable * Term * Term * Term // Term integrated by variable from term to term
    | Summation of Variable * Term * Term * Term // Term summed with index of name Variable starting from term up to term
    | Limit of Variable * Term * Term // Limit of term when Variable tends to Term
    | Ncr of Term * Term // "term" choose "term" (n above r)
    | Matrix of (Term * int * int) list // list of terms with index pair TODO: better representation of matrix
    | Determinant of Term // Determinant of a Matrix TODO: better representation? can we give it a Matrix term?

    type VariableDomain =
    | IntegerDomain of Variable * int * int // Variable domain of Variable from int1 to int2
    | RealDomain of Variable * Constant * Constant // Variable domain of Variable from const1 to const2


    type Problem = Term * Term * VariableDomain list
    
    type Id = int

type ChoiceU =Id * UnaryOp of UnaryOp


type ChoiceB =Id  * BinaryOp of BinaryOp


type ChoiceT =Id * Term of Term

type OpTerm =
|UnaryOp of UnaryOp
|BinaryOp of BinaryOp
|Term of Term

type QUnaryOp =
| UnaryOp of UnaryOp
| ChoiceU of ChoiceU

type QBinaryOp =
| BinaryOp of BinaryOp
| ChoiceB of ChoiceB

type QTerm =
| Term of Term
| ChoiceT of ChoiceT

type QProblem  =
| QTerm of QTerm *QTerm of QTerm * VariableDomain list of VariableDomain

type QConstraint = 
| FunctionalConstraint of Id *(Id list -> OpTerm of OpTerm)
| RelationalConstraint of Id list -> bool


type Query =QProblem of QProblem * QConstraint list of QConstraint
    

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code
