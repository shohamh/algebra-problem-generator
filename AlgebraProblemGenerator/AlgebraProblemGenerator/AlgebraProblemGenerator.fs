
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

    type ChoiceU = Id * UnaryOp Set

    type ChoiceB = Id  * BinaryOp Set

    type ChoiceT = Id * Term Set

    type ChoiceConst = Id * Constant Set
    

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
    | QMatrix of (QTerm * int * int) list // list of terms with index pair TODO: better representation of matrix
    | QDeterminant of QTerm // Determinant of a Matrix TODO: better representation? can we give it a Matrix term?
    | ChoiceT of ChoiceT

    type QProblem  = QTerm * QTerm  * VariableDomain list

    type QConstraint = 
    | FunctionalConstraint of Id *(Id list -> OpTerm)
    | RelationalConstraint of (Id list -> bool)


    type Query = QProblem * QConstraint list



    let executeTerm (qterm : QTerm) =
        // return set of terms
        let rand = System.Random()
            match qterm with
            | QConstant qc -> 
                match qc with
                | Constant c -> c
                | ChoiceConst cc -> rand.Next(1,101)//TODO generate reasonable number and return Constant [that number] 
            | Variable v -> v
            | QUnaryTerm (qUnaryOp, tempQTerm) ->
                let newTerm = executeTerm(tempQTerm)
                let newUnaryOp =
                    match qUnaryOp with
                    | UnaryOp u -> u
                    | ChoiceU (id, setUnaryOps) -> setUnaryOps[rand.Next(1,101)] // TODO generate random index and choose it from the set
                (newUnaryOp, newTerm)
            | QBinaryTerm (tempQTerm1, qbinaryop, tempQTerm2) ->
                let newTerm1 = executeTerm(tempQTerm1)
                let newTerm2 = executeTerm(tempQTerm2)
                let newBinaryOp=
                    match qbinaryop with
                    | BinaryOp b -> b
                    | choiceB(id,setBinaryOps) -> setBinaryOps[rand.Next(1,101)] //what is the exactly range??
                (newTerm1,qbinaryop,newTerm2)
            | QDifferential(variable1,tempDQTerm) ->
                  let newDTerm = executeTerm(tempDQTerm)
                  (variable1,newDTerm)
            | QIndefiniteIntegral(variable2,tempIQTerm) ->
                  let newIterm=executeTerm(tempIQTerm)
                  (variable2,newIterm)    
            | QDefiniteIntegral(variable3,tempIQTerm1,tempIQTerm2,tempIQTerm3) ->
                let term1=executeTerm(tempIQTerm1)
                let term2=executeTerm(tempIQTerm2)
                let term3=executeTerm(tempIQTerm3)
                (variable3,term1,term2,term3)
            | QSummation(variable4,tempSQTerm1,tempSQTerm2,tempSQTerm3) ->
                let Sterm1=executeTerm(tempSQTerm1)
                let Sterm2=executeTerm(tempSQTerm2)
                let Sterm3=executeTerm(tempSQTerm3)
                (variable4,Sterm1,Sterm2,Sterm3)
            | QLimit(variable5,tempLQTerm1,tempLQTerm2) ->
                let Lterm1=executeTerm(tempLQTerm1)
                let Lterm2=executeTerm(tempLQTerm2)
                (variable5,Lterm1,Lterm2)
            | QNcr(tempNQterm1,tempNQTerm2) ->
                let Nterm1=executeTerm(tempNQterm1)
                let Nterm2=executeTerm(tempNQterm2)
                (Nterm1,Nterm2)

   //         | QMatrix((tempMQTerm,int1,int2)list) ->
 
            | QDeterminant(tmpQTerm) ->
                 let dterm=executeTerm(tmpQTerm)
                 dterm
            | ChoiceT(id,setTerms) -> setTerms[rand.Next(1,101)] //what is the exactly range?

   

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code
