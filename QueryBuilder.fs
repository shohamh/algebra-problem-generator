module QueryBuilder

open AlgebraProblemGenerator

let rec build (root:Term) (constraints:Constraint list) : QTerm = 
    match root with
    | TConstant constant ->
        match constant with
        | Real _ ->
            match constraints.Head with
            | Free -> 
                QConstant <| ChoiceC (0, [(NegativeInfinity,Infinity)])
            | Constant constConstraint ->
                match constConstraint with
                | Identical ->  
                    QConstant <| ChoiceC (0, [(constant,constant)])
                | Range domainList ->
                     QConstant <| ChoiceC (0, domainList)
        | _ -> QConstant <| JustC constant
    | TVariable var -> QVariable var
    | UnaryTerm (op,term) ->
        match op with
        | Negative | NaturalLog | Sqrt ->
            QUnaryTerm (JustU op, build term [Free]) 
        | Log base_ ->
            QUnaryTerm (JustU (Log base_), build term [Free])
        | Trig _ ->
            let qop = ChoiceU (0, List.map Trig [Sin; Cos; Tan; Cot; Sec; Csc])
            QUnaryTerm (qop,build term [Free])
        | InvTrig _ -> 
            let qop = ChoiceU (0, List.map InvTrig [Arcsin; Arccos; Arctan; Arccot; Arcsec; Arccsc])
            QUnaryTerm (qop, build term [Free])
    | BinaryTerm (term1,op,term2) ->
        match op with
        | Exponent ->
            QBinaryTerm (build term1 [Free], JustB op , build term2 [Constant Identical])
        | _ ->
            QBinaryTerm (build term1 [Free], JustB op , build term2 [Free])
    | AssociativeTerm (op,lst) -> 
        let qlst = List.map (fun a -> build a [Free]) lst
        match op with
        | Plus ->
            let qop = JustA (Plus)
            QAssociativeTerm (qop, qlst)
        | Multiply ->
            let qop = JustA (Multiply)
            QAssociativeTerm (qop, qlst)
        | Equals ->
            let qop = JustA (Equals)
            QAssociativeTerm (qop, qlst)
    | Differential (var, term) ->
        QDifferential (var, build term [Free])
    | IndefiniteIntegral (var, term) ->
        QIndefiniteIntegral (var, build term [Free])
    | DefiniteIntegral (var, term, term1, term2)->         
        QDefiniteIntegral (var, build term [Free], build term1 [Free],build term2 [Free])
    | Summation (var, term, term1, term2)->
        QSummation (var, build term [Free], build term1 [Free], build term2 [Free])
    | Limit (var, term1, term2)->
        QLimit (var, build term1 [Free], build term2 [Free])
    | Ncr (term1, term2)->
        QNcr (build term1 [Free], build term2 [Free])
    | TFenced t ->
        QTFenced (build t [Free])
        