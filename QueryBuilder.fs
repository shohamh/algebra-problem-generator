module QueryBuilder

open AlgebraProblemGenerator

let rec Build (root:Term) = 
    match root with
    | Term.TConstant con ->
        match con with
        | Real f -> //FIX THIS
            let max = f + 100.0
            let min = f - 100.0
            let range = (Real min, Real max)
            QTerm.QConstant <| QConstant.ChoiceC (0, [range]) 
        | _ -> 
            QTerm.QConstant <| QConstant.JustC con
    | Term.TVariable var -> QVariable var
    | UnaryTerm (op,term) ->
        match op with
        | UnaryOp.Negative | UnaryOp.NaturalLog | UnaryOp.Sqrt -> //should i change the negative?
            QTerm.QUnaryTerm (QUnaryOp.JustU op, Build term) 
        | UnaryOp.Trig _ ->
            let qop = QUnaryOp.ChoiceU (0, List.map UnaryOp.Trig [Trig.Sin; Trig.Cos; Trig.Tan; Trig.Cot; Trig.Sec; Trig.Csc])
            QTerm.QUnaryTerm (qop,Build term)
        | UnaryOp.InvTrig _ -> 
            let qop = QUnaryOp.ChoiceU (0, List.map UnaryOp.InvTrig [InvTrig.Arcsin; InvTrig.Arccos; InvTrig.Arctan; InvTrig.Arccot; InvTrig.Arcsec; InvTrig.Arccsc])
            QTerm.QUnaryTerm (qop, Build term)
    | BinaryTerm (term1,op,term2) ->
        QTerm.QBinaryTerm (Build term1, QBinaryOp.JustB op , Build term2)
    | AssociativeTerm (op,lst) -> 
        let qlst = List.map Build lst
        match op with
        | AssociativeOp.Plus ->
            let qop = QAssociativeOp.JustA (AssociativeOp.Plus)
            QTerm.QAssociativeTerm (qop, qlst)
        | AssociativeOp.Multiply ->
            let qop = QAssociativeOp.JustA (AssociativeOp.Multiply)
            QTerm.QAssociativeTerm (qop, qlst)
    | Differential (var, term) ->
        QTerm.QDifferential (var, Build term)
    | IndefiniteIntegral (var, term) ->
        QTerm.QIndefiniteIntegral (var, Build term)
    | DefiniteIntegral (var, term, term1, term2)->         
        QTerm.QDefiniteIntegral (var, Build term, Build term1,Build term2)
    | Summation (var, term, term1, term2)->
        QTerm.QSummation (var, Build term, Build term1, Build term2)
    | Limit (var, term1, term2)->
        QTerm.QLimit (var, Build term1, Build term2)
    | Ncr (term1, term2)->
        QTerm.QNcr (Build term1, Build term2) 
        
