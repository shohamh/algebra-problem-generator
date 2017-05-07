module AlgebraProblemGenerator

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

[<CustomEquality>]
[<CustomComparison>]
type Constant =
| Infinity
| NegativeInfinity
| Real of float
with
    static member (-) (x : Constant, y : Constant) =
        match (x, y) with
        | (Infinity, _) -> Infinity
        | (NegativeInfinity, _) -> NegativeInfinity
        | (Real r1, Real r2) -> Real (r1 - r2)
        | (Real r1, Infinity) -> NegativeInfinity
        | (Real r1, NegativeInfinity) -> Infinity

    static member (+) (x : Constant, y : Constant) =
        match (x, y) with
        | (Infinity, _) -> Infinity
        | (NegativeInfinity, _) -> NegativeInfinity
        | (Real r1, Real r2) -> Real (r1 + r2)
        | (Real r1, a) -> a

    static member (~-) (x: Constant) =
        match x with
        | Infinity -> NegativeInfinity
        | NegativeInfinity -> Infinity
        | Real r -> Real (-r)
    
    override x.Equals(y) =
        match y with
        | :? Constant as c ->
            match (x, c) with
            | (Infinity, Infinity) -> true
            | (NegativeInfinity, NegativeInfinity) -> true
            | (Real r1, Real r2) -> r1 = r2
            | (_, _) -> false
        | _ -> false

    override x.GetHashCode() = 
        match x with
        | Infinity -> System.Int32.MaxValue
        | NegativeInfinity -> System.Int32.MinValue
        | Real r -> hash r

    interface System.IComparable with
        member x.CompareTo y =
            match y with
            | :? Constant as c ->
                match (x, c) with
                | (Real r1, Real r2) -> compare r1 r2
                | (Infinity, Infinity) -> 0
                | (NegativeInfinity, NegativeInfinity) -> 0
                | (NegativeInfinity, Infinity) -> -1
                | (Infinity, NegativeInfinity) -> 1
                | (Infinity, Real r2) -> 1
                | (NegativeInfinity, Real r2) -> -1
                | (Real r1, Infinity) -> -1
                | (Real r1, NegativeInfinity) -> 1
            | _ -> -1

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
//| Matrix of (Term * int * int) list // list of terms with index pair TODO: better representation of matrix
//| Determinant of Term // Determinant of a Matrix TODO: better representation? can we give it a Matrix term?


type Domain = Constant * Constant

type VariableDomain = Variable * Domain

type Problem = Term * Term * VariableDomain list
   
type Id = int

type ChoiceU = Id * UnaryOp list

type ChoiceB = Id  * BinaryOp list

type ChoiceT = Id * Term list

type ChoiceConst = Id * Domain list


type OpTerm =
| UnaryOp of UnaryOp
| BinaryOp of BinaryOp
| Term of Term

type QUnaryOp =
| UnaryOp of UnaryOp
| ChoiceU of ChoiceU

type QBinaryOp =
| BinaryOp of BinaryOp
| ChoiceB of ChoiceB

type QConstant =
| Constant of Constant
| ChoiceConst of ChoiceConst

type QTerm =
| QConstant of QConstant
| Variable of Variable
| QUnaryTerm of QUnaryOp * QTerm
| QBinaryTerm of QTerm * QBinaryOp * QTerm
| QDifferential of Variable * QTerm // Term differentiated by variable
| QIndefiniteIntegral of Variable * QTerm // Term integrated by variable
| QDefiniteIntegral of Variable * QTerm * QTerm * QTerm // Term integrated by variable from term to term
| QSummation of Variable * QTerm * QTerm * QTerm // Term summed with index of name Variable starting from term up to term
| QLimit of Variable * QTerm * QTerm // Limit of term when Variable tends to Term
| QNcr of QTerm * QTerm // "term" choose "term" (n above r)
//| QMatrix of (QTerm * int * int) list // list of terms with index pair TODO: better representation of matrix
//| QDeterminant of QTerm // Determinant of a Matrix TODO: better representation? can we give it a Matrix term?
| ChoiceT of ChoiceT

type QProblem  = QTerm * QTerm  * VariableDomain list

type QConstraint = 
| FunctionalConstraint of Id *(Id list -> OpTerm)
| RelationalConstraint of (Id list -> bool)


type Query = QProblem * QConstraint list


let constantInDomain constant domain =
    constant >= fst domain && constant <= snd domain

let numberInDomainList x domainList = 
    List.exists (constantInDomain x) domainList

let domainLength domain =
    snd domain - fst domain

let roundConstant constant =
    match constant with
    | Real x -> Real (System.Math.Round x)
    | Infinity -> Infinity
    | NegativeInfinity -> NegativeInfinity

let unboxConstant constant =
    match constant with
    | Real x -> x
    | Infinity -> System.Double.PositiveInfinity
    | NegativeInfinity -> System.Double.NegativeInfinity

let lengthOfDomainListRange domainList =
    let ranges = List.map domainLength domainList
    List.fold (+) 0 ranges
