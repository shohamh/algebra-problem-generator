module QueryExecutor

open AlgebraProblemGenerator

let rec executeTerm (qterm : QTerm) =
    // return list of terms
    let rand = System.Random()
    match qterm with
    | QConstant qc -> 
        match qc with
        | JustC c -> Term.TConstant c
        | ChoiceC (id, domainlist) -> 
            let maxRange = List.max (List.map snd domainlist)
            let minRange = List.min (List.map fst domainlist)
            let max = 
                match maxRange with
                | Infinity -> Real (float 25)
                | a -> a
            let min = 
                match minRange with
                | NegativeInfinity -> Real (float -25)
                | a -> a
            
            if min = roundConstant min && max = roundConstant max then
                Term.TConstant (Real (float (rand.Next ((int (unboxConstant min)), int (unboxConstant max)))))
            else
                Term.TConstant (Real (rand.NextDouble() * unboxConstant (max - min) + unboxConstant min)) //TODO: try round it if the eval passes after.
            
    | QVariable v -> Term.TVariable v
    | QUnaryTerm (qUnaryOp, qTerm) ->
        let newTerm = executeTerm qTerm
        let newUnaryOp =
            match qUnaryOp with
            | JustU u -> u
            | ChoiceU (id, listUnaryOps) -> List.item (rand.Next (0, List.length listUnaryOps)) listUnaryOps
        Term.UnaryTerm (newUnaryOp, newTerm)
    | QBinaryTerm (qTerm1, qBinaryOp, qTerm2) ->
        let term1 = executeTerm qTerm1
        let term2 = executeTerm qTerm2
        let binaryOp =
            match qBinaryOp with
            | JustB b -> b
            | ChoiceB(id, listBinaryOps) -> List.item (rand.Next (0, List.length listBinaryOps)) listBinaryOps
        Term.BinaryTerm (term1, binaryOp, term2)
    //|

    | QAssociativeTerm (qAssociativeOp, qTermList) ->
        let termList = List.map executeTerm qTermList
        let associativeOp =
            match qAssociativeOp with
            | JustA a -> a
            | ChoiceA(id, listAssociativeOps) -> List.item (rand.Next (0, List.length listAssociativeOps)) listAssociativeOps
        Term.AssociativeTerm (associativeOp, termList)
    | QDifferential(variable, qTerm) ->
          let term = executeTerm qTerm
          Term.Differential (variable, term)
    | QIndefiniteIntegral(variable ,qTerm) ->
          let term = executeTerm qTerm
          Term.IndefiniteIntegral (variable, term)    
    | QDefiniteIntegral(variable, qTerm1, qTerm2, qTerm3) ->
        let term1 = executeTerm qTerm1
        let term2 = executeTerm qTerm2 
        let term3 = executeTerm qTerm3
        Term.DefiniteIntegral (variable, term1, term2, term3)
    | QSummation(variable, qTerm1, qTerm2, qTerm3) ->
        let term1 = executeTerm qTerm1
        let term2 = executeTerm qTerm2
        let term3 = executeTerm qTerm3
        Term.Summation (variable, term1, term2, term3)
    | QLimit(variable, qTerm1, qTerm2) ->
        let term1 = executeTerm qTerm1
        let term2 = executeTerm qTerm2
        Term.Limit (variable, term1, term2)
    | QNcr(qTerm1, qTerm2) ->
        let term1 = executeTerm qTerm1
        let term2 = executeTerm qTerm2
        Term.Ncr (term1, term2)
    //| QMatrix((tempMQTerm,int1,int2)list) ->
    //| QDeterminant(tmpQTerm) ->
    //executeTerm(tmpQTerm)
    | QChoice(listTerms) -> List.item (rand.Next (0, List.length listTerms)) listTerms |> executeTerm