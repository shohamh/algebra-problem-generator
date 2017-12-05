module TRegex

open AlgebraProblemGenerator
open Utils
open FParsec.CharParsers

type NodeValue =
| Constant of Constant
| Variable of Variable
| UnaryOp of UnaryOp
| BinaryOp of BinaryOp
| AssociativeOp of AssociativeOp


type Node = {
    parent : Node option;
    mutable children : Node list;
    value : NodeValue;
}

type Relation =
| Descendant
| DirectDescendant
| Sibling
| Precedent
| ImmediatePrecedent

type TRegex = {
    dominant : NodeValue;
    subjects : (Relation * TRegex) list option;
}

let nodeValueToString (value:NodeValue) : string =
    match value with
    | Constant c ->
        match c with
        | Infinity -> "inf"
        | NegativeInfinity -> "ninf"
        | Real r -> string r 
    | Variable v-> v
    | UnaryOp op ->
        match op with
        | Negative -> "-"
        | NaturalLog -> "log"
        | Log c -> 
            match c with
                | Infinity -> "loginf"
                | NegativeInfinity -> "logninf"
                | Real r -> String.concat "" ["log";(string r)]
        | Sqrt -> "sqrt"
        | Trig t ->
                match t with
                | Sin -> "sin"
                | Cos -> "cos"
                | Tan -> "tan"
                | Cot -> "cot"
                | Sec -> "sec"
                | Csc -> "csc"
        | InvTrig t ->
                match t with
                | Arcsin -> "arcsin"
                | Arccos -> "arccos"
                | Arctan -> "arctan"
                | Arccot -> "arccot"
                | Arcsec -> "arcsec"
                | Arccsc -> "arccsc" 
    | BinaryOp op ->
        match op with
        | Divide -> "/"
        | Exponent -> "^"
    | AssociativeOp op ->
        match op with
        | Plus -> "+"
        | Multiply -> "*"
        | Equals -> "="        

let relationToString (relation:Relation) : string =
    match relation with
    | Descendant -> "<<"
    | DirectDescendant -> "<"
    | Sibling -> "$"
    | Precedent -> ","
    | ImmediatePrecedent -> "."

let rec tregexToString (tregex:TRegex) : string =
    let mutable res = nodeValueToString tregex.dominant
    match tregex.subjects with
    | None ->
        nodeValueToString tregex.dominant
    | Some subjects ->
        let dominant = nodeValueToString tregex.dominant
        let perSubject (relation : Relation, texpr : TRegex) =
            [relationToString relation;
            (if texpr.subjects.IsNone || List.isEmpty texpr.subjects.Value then
                tregexToString texpr
            else
                "(" + tregexToString texpr + ")")
            ]
        String.concat " " <| dominant::List.collect perSubject subjects

let termToNode (root : Term) : Node =
    let rec termToNodeHelper (root:Term) (parent : Node option): Node =
        match root with
        | TConstant constant ->  {parent=parent;children=[];value=Constant constant} 
        | TVariable variable -> {parent=parent;children=[];value=Variable variable}
        | UnaryTerm (op,term)->
            let res = {parent=parent;children=[];value=UnaryOp op}
            res.children <- [termToNodeHelper term (Some res)]
            res
        | BinaryTerm (term1,op,term2) -> 
            let res = {parent=parent;children=[];value=BinaryOp op}
            res.children <- [termToNodeHelper term1 (Some res);termToNodeHelper term2 (Some res)]
            res
        | AssociativeTerm (op,terms) ->
            let res = {parent=parent;children=[];value=AssociativeOp op}
            res.children <- List.map (fun term-> termToNodeHelper term parent) terms
            res
    termToNodeHelper root None

let rec find (root:Node) (target:NodeValue) : Node list =
    let res=List.collect (fun child -> find child target) root.children
    if root.value = target then
        List.append [root] res
    else
        res

let rec getAll (root:Node) : Node list =
    let res=List.collect getAll root.children
    List.append [root] res

let checkDescendant (root:Node) (ancestor:NodeValue) (descendant:NodeValue) : Node list = 
    let ancestors = find root ancestor
    List.collect (fun ancestorRoot -> List.collect (fun x-> if x.parent.IsSome then [x.parent.Value] else []) (find ancestorRoot descendant)) ancestors
         
let checkDirectDescendant (root:Node) (parent:NodeValue) (child:NodeValue) : Node list = 
    let parents= find root parent
    List.filter (fun parent -> List.exists (fun x-> x.value = child) parent.children) parents

let checkSibling (root:Node) (siblings:NodeValue list) : Node list list = 
    let childrenOfNodes=List.map (fun x-> x.children) (getAll root)
    List.filter (fun x-> containsList (List.map (fun y-> y.value) x) siblings) childrenOfNodes 

let checkPrecedent (root:Node) (siblings:NodeValue list) : Node list list = 
    let childrenOfNodes=List.map (fun x-> x.children) (getAll root)
    List.filter (fun x-> containsListInOrder (List.map (fun y-> y.value) x) siblings) childrenOfNodes

let checkImmediatePrecedent (root:Node)  (siblings:NodeValue list): Node list list = 
    let childrenOfNodes=List.map (fun x-> x.children) (getAll root)
    List.filter (fun x-> containsExactList (List.map (fun y-> y.value) x) siblings) childrenOfNodes

let rec checkDescendantReal (ancestor:Node) (descendant:Node) : bool=
    if ancestor=descendant then true
    else List.exists (checkDescendantReal descendant) ancestor.children

let checkDirectDescendantReal (parent:Node) (descendant:Node) : bool =
    List.contains descendant parent.children

let checkSiblingReal (node1:Node) (node2:Node) : bool = 
    if node1.parent.IsSome then List.contains node2 node1.parent.Value.children
    else false

let checkPrecedentReal (node1: Node) (node2: Node) : bool=
    if node1.parent.IsNone then false
    else 
        let idx1=List.findIndex (fun child -> child=node1) node1.parent.Value.children
        let idx2=List.findIndex (fun child -> child=node2) node1.parent.Value.children
        idx1<idx2
let checkImmediatePrecedentReal (node1: Node) (node2: Node) : bool =
    if node1.parent.IsNone then false
    else 
        let idx1=List.findIndex (fun child -> child=node1) node1.parent.Value.children
        let idx2=List.findIndex (fun child -> child=node2) node1.parent.Value.children
        idx1=idx2-1

let rec checkRegex (root:Node) (exp:TRegex) : Node list =
    match exp.subjects with
    | Some subjects ->
        let checkPerSubject (relation, sexp) : Node list =
            let isGoodDominantsFunction =
                match relation with
                | Descendant -> checkDescendant
                | DirectDescendant -> checkDirectDescendant   
                // | Sibling -> checkSibling 
                // | Precedent -> checkPrecedent
                // | ImmediatePrecedent -> checkImmediatePrecedent
            let isGoodCandidateFunction =
                match relation with
                | Descendant -> checkDescendantReal    
                | DirectDescendant -> checkDirectDescendantReal
                | Sibling -> checkSiblingReal
                | Precedent -> checkPrecedentReal
                | ImmediatePrecedent -> checkImmediatePrecedentReal
            let candidates = checkRegex root sexp            
            match candidates with
            | [] -> []
            | _ ->
                let possibleDominantsOfCandidate = isGoodDominantsFunction root exp.dominant (List.item 0 candidates).value
                List.filter (fun dominant -> List.contains true <| List.map (isGoodCandidateFunction dominant) candidates) possibleDominantsOfCandidate
        List.ofSeq (Set.intersectMany <| List.map (checkPerSubject >> set) subjects)
    | None ->
        find root exp.dominant

let collectDomains (term : Term) (baseExpressions: TRegex list) : TRegex list =
    let rec collectDomainsHelper (term:Term) (exp: TRegex) (baseExpressions: TRegex list) : TRegex list = 
        printfn "checkRegex: %A" exp
        let tree = termToNode term
        let matches = checkRegex tree exp
        match matches with
        | [] ->
            printfn "%b" false 
            []
        | _ ->
            let perExpression (expression: TRegex) =
                let tregex = {
                    dominant = exp.dominant;
                    subjects =  
                        match exp.subjects with
                        | Some listOfSubjects ->
                            Some (List.append listOfSubjects [(Relation.Descendant, expression)])
                        | None ->
                            Some [(Relation.Descendant, expression)]
                }
                collectDomainsHelper term tregex baseExpressions
            List.collect perExpression baseExpressions
    List.collect (fun baseExpression -> collectDomainsHelper term baseExpression baseExpressions) baseExpressions
