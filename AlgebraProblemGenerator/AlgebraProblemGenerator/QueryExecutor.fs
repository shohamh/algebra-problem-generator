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
    | QBinaryTerm (tempQTerm1, qbinaryop, tempQTerm2) ->
        let newTerm1 = executeTerm tempQTerm1
        let newTerm2 = executeTerm tempQTerm2
        let newBinaryOp =
            match qbinaryop with
            | BinaryOp b -> b
            | ChoiceB(id,listBinaryOps) -> List.item (rand.Next (0, List.length listBinaryOps)) listBinaryOps
        Term.BinaryTerm (newTerm1, newBinaryOp, newTerm2)
    | QDifferential(variable1,tempDQTerm) ->
          let newDTerm = executeTerm tempDQTerm
          Term.Differential (variable1,newDTerm)
    | QIndefiniteIntegral(variable2,tempIQTerm) ->
          let newIterm = executeTerm tempIQTerm
          Term.IndefiniteIntegral (variable2,newIterm)    
    | QDefiniteIntegral(variable3,tempIQTerm1,tempIQTerm2,tempIQTerm3) ->
        let term1=executeTerm tempIQTerm1
        let term2=executeTerm tempIQTerm2 
        let term3=executeTerm tempIQTerm3
        Term.DefiniteIntegral (variable3,term1,term2,term3)
    | QSummation(variable4,tempSQTerm1,tempSQTerm2,tempSQTerm3) ->
        let Sterm1=executeTerm tempSQTerm1
        let Sterm2=executeTerm tempSQTerm2
        let Sterm3=executeTerm tempSQTerm3
        Term.Summation (variable4,Sterm1,Sterm2,Sterm3)
    | QLimit(variable5,tempLQTerm1,tempLQTerm2) ->
        let Lterm1=executeTerm tempLQTerm1
        let Lterm2=executeTerm tempLQTerm2
        Term.Limit (variable5,Lterm1,Lterm2)
    | QNcr(tempNQTerm1,tempNQTerm2) ->
        let Nterm1=executeTerm tempNQTerm1
        let Nterm2=executeTerm tempNQTerm2
        Term.Ncr (Nterm1,Nterm2)

    //| QMatrix((tempMQTerm,int1,int2)list) ->

//    | QDeterminant(tmpQTerm) ->
  //       executeTerm(tmpQTerm)
    | ChoiceT(id,listTerms) -> List.item (rand.Next (0, List.length listTerms)) listTerms