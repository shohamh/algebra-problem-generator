module QueryBuilder

open AlgebraProblemGenerator

let rec Build (root:Term) = 
    match root with
    | Constant con ->
        match con with
        | float f -> //FIX THIS
            let max = f+100
            let min = f-100
            let range = (min,max)
            QTerm.QConstant (0,range) 
        | _ -> 
            QTerm.QConstant con
    | Variable var -> var
    | UnaryTerm (op,term) ->
        match op with
        | UnaryOp.Negative | UnaryOp.NaturalLog | UnaryOp.Sqrt -> //should i change the negative?
            let op = UnaryOp op
            QTerm.QUnaryTerm (op, Build term) 
        | UnaryOp.Trig ->
            let qop = QBinaryOp (0,[Trig.Sin;Trig.Cos;Trig.Tan;Trig.Cot;Trig.Sec;Trig.Csc])
            QTerm.QUnaryTerm (qop,Build term)
        | UnaryOp.InvTrig -> 
            let qop = QBinaryOp (0,[InvTrig.Arcsin;InvTrig.Arccos;InvTrig.Arctan;InvTrig.Arccot;InvTrig.Arcsec;InvTrig.Arccsc])
            QTerm.QUnaryTerm (qob, Build term)
    | BinaryTerm (term1,op,term2) ->
        match op with
        | BinaryOp.Plus | BinaryOp.Minus -> 
            QTerm.QBinaryTerm (Build term1, [BinaryOp.Plus;BinaryOp.Minus], Build term2)
        | BinaryOp.Multiply | BinaryOp.Divide -> 
            QTerm.QBinaryTerm (Build term1, [BinaryOp.Multiply;BinaryOp.Divide], Build term2)
        | BinaryOp.Exponent -> 
            QTerm.QBinaryTerm (Build term1, [BinaryOp.Exponent] , Build term2)
    | AssociativeTerm (op,lst) -> 
        let qlst = [for t in lst -> Build t]
        match op with
        | AssociativeOp.Plus ->
            let qop = QAssociativeOp (AssociativeOp.Plus)
            QTerm.QAssociativeTerm (qop,qlst)
        | AssociativeOp.Multiply ->
            let qop = QAssociativeOp (AssociativeOp.Multiply)
            QTerm.QAssociativeTerm (qop,qlst)
    | Differential (var,term) ->
        QTerm.QDifferential (var,term)
    | IndefiniteIntegral (var, term) ->
        QTerm.QIndefiniteIntegral (var,Build term)
    | DefiniteIntegral (var,term,term1,term2)->         
        QTerm.QDefiniteIntegral (var, Build term, Build term1,Build term2)
    | Summation (var,term,term1,term2)->
        QTerm.QSummation (var, Build term, Build term1,Build term2)
    | Limit (var,term1,term2)->
        QTerm.QLimit (var,Build term1,Build term2)
    | Ncr (term1, term2)->
       QTerm.QNcr (Build term1, Build term2) 
        
