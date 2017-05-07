module QueryExecutor

open AlgebraProblemGenerator

let rec executeTerm (qterm : QTerm) =
    // return list of terms
    let rand = System.Random()
    match qterm with
    | QConstant qc -> 
        match qc with
        | Constant c -> Term.Constant c
        | ChoiceConst (id, domainlist) -> 
            let maxRange = List.max (List.map snd domainlist)
            let minRange = List.min (List.map fst domainlist)
            let max = 
                match maxRange with
                | Infinity -> Real (float 50)
                | a -> a
            let min = 
                match minRange with
                | NegativeInfinity -> Real (float -50)
                | a -> a
            
            if min = roundConstant min && max = roundConstant max then
                Term.Constant (Real (float (rand.Next ((int (unboxConstant min)), int (unboxConstant max)))))
            else
                Term.Constant (Real (rand.NextDouble() * unboxConstant (max - min) + unboxConstant min)) //TODO: try round it if the eval passes after.
            
    | Variable v -> Term.Variable v
    | QUnaryTerm (qUnaryOp, tempQTerm) ->
        let newTerm = executeTerm tempQTerm
        let newUnaryOp =
            match qUnaryOp with
            | UnaryOp u -> u
            | ChoiceU (id, listUnaryOps) -> List.item (rand.Next (0, List.length listUnaryOps)) listUnaryOps
        Term.UnaryTerm (newUnaryOp, newTerm)
    | QBinaryTerm (tempQTerm1, qBinaryOp, tempQTerm2) ->
        let term1 = executeTerm tempQTerm1
        let term2 = executeTerm tempQTerm2
        let binaryOp =
            match qBinaryOp with
            | BinaryOp b -> b
            | ChoiceB(id, listBinaryOps) -> List.item (rand.Next (0, List.length listBinaryOps)) listBinaryOps
        Term.BinaryTerm (term1, binaryOp, term2)
    | QDifferential(variable, tempQTerm) ->
          let term = executeTerm tempQTerm
          Term.Differential (variable, term)
    | QIndefiniteIntegral(variable ,tempQTerm) ->
          let term = executeTerm tempQTerm
          Term.IndefiniteIntegral (variable, term)    
    | QDefiniteIntegral(variable, tempQTerm1, tempQTerm2, tempQTerm3) ->
        let term1 = executeTerm tempQTerm1
        let term2 = executeTerm tempQTerm2 
        let term3 = executeTerm tempQTerm3
        Term.DefiniteIntegral (variable, term1, term2, term3)
    | QSummation(variable, tempQTerm1, tempQTerm2, tempQTerm3) ->
        let term1 = executeTerm tempQTerm1
        let term2 = executeTerm tempQTerm2
        let term3 = executeTerm tempQTerm3
        Term.Summation (variable, term1, term2, term3)
    | QLimit(variable, tempQTerm1, tempQTerm2) ->
        let term1 = executeTerm tempQTerm1
        let term2 = executeTerm tempQTerm2
        Term.Limit (variable, term1, term2)
    | QNcr(tempQTerm1, tempQTerm2) ->
        let term1 = executeTerm tempQTerm1
        let term2 = executeTerm tempQTerm2
        Term.Ncr (term1, term2)

    //| QMatrix((tempMQTerm,int1,int2)list) ->

//    | QDeterminant(tmpQTerm) ->
  //       executeTerm(tmpQTerm)
    | ChoiceT(id, listTerms) -> List.item (rand.Next (0, List.length listTerms)) listTerms