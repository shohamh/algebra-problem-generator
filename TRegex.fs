module TRegex

open AlgebraProblemGenerator
open Utils


type NodeValue =
| Constant of Constant
| Variable of Variable
| UnaryOp of UnaryOp
| BinaryOp of BinaryOp
| AssociativeOp of AssociativeOp

[<CustomEquality; CustomComparison>]
type Node =
    {
        parent : Node option;
        mutable children : Node list;
        value : NodeValue;
    }
    override x.Equals(yobj) =
        match yobj with
        | :? Node as y -> (x.value = y.value && x.children = y.children && System.Object.ReferenceEquals(x.parent, y.parent))
        | _ -> false
    override x.GetHashCode() = 31 * hash x.children + 37 * hash x.value    
    interface System.IComparable with
      member x.CompareTo yobj =
          match yobj with
          | :? Node as y -> 
            if x.children = y.children && x.value = y.value then
                0
            else 1
          | _ -> invalidArg "yobj" "cannot compare values of different types"

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
        | Real r ->
            if r = System.Math.PI then
                string "pi"
            elif r = System.Math.E then
                string "e"
            else
                string r 
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
            res.children <- List.map (fun term-> termToNodeHelper term (Some res)) terms
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
    List.collect (fun ancestorRoot -> List.collect (fun x-> if x.parent.IsSome then [x.parent.Value] else []) (find ancestorRoot descendant)) (List.collect (fun x -> x.children) ancestors)
         
let checkDirectDescendant (root:Node) (parent:NodeValue) (child:NodeValue) : Node list = 
    let parents= find root parent
    List.filter (fun parent -> List.exists (fun x-> x.value = child) parent.children) parents

// let checkSibling (root:Node) (siblings:NodeValue list) : Node list list = 
//     let childrenOfNodes=List.map (fun x-> x.children) (getAll root)
//     List.filter (fun x-> containsList (List.map (fun y-> y.value) x) siblings) childrenOfNodes 
let checkSibling (root:Node) (node1:NodeValue) (node2: NodeValue) : (Node * Node) list = 
    let childrenOfNodes=List.map (fun x-> x.children) (getAll root)
    let filter = List.filter (fun x -> containsList (List.map (fun y-> y.value) x) [node1; node2]) childrenOfNodes
    if List.isEmpty filter then
        []
    else
        List.map (fun siblings -> (List.find (fun x -> x.value = node1) siblings, List.find (fun x -> x.value = node2) siblings)) filter


// let checkPrecedent (root:Node) (siblings:NodeValue list) : Node list list = 
//     let childrenOfNodes=List.map (fun x-> x.children) (getAll root)
//     List.filter (fun x-> containsListInOrder (List.map (fun y-> y.value) x) siblings) childrenOfNodes

let checkPrecedent (root:Node) (node1:NodeValue) (node2: NodeValue) : (Node * Node) list = 
    let childrenOfNodes=List.map (fun x-> x.children) (getAll root)
    let filter = List.filter (fun x -> containsListInOrder (List.map (fun y-> y.value) x) [node1; node2]) childrenOfNodes
    if List.isEmpty filter then
        []
    else
        List.map (fun siblings -> (List.find (fun x -> x.value = node1) siblings, List.find (fun x -> x.value = node2) siblings)) filter

let checkImmediatePrecedent (root:Node) (node1:NodeValue) (node2: NodeValue) : (Node * Node) list = 
    let childrenOfNodes=List.map (fun x-> x.children) (getAll root)
    let filter = List.filter (fun x -> containsExactList (List.map (fun y-> y.value) x) [node1; node2]) childrenOfNodes
    if List.isEmpty filter then
        []
    else
        List.map (fun siblings -> (List.find (fun x -> x.value = node1) siblings, List.find (fun x -> x.value = node2) siblings)) filter

// let checkImmediatePrecedent (root:Node)  (siblings:NodeValue list): Node list list = 
//     let childrenOfNodes=List.map (fun x-> x.children) (getAll root)
//     List.filter (fun x-> containsExactList (List.map (fun y-> y.value) x) siblings) childrenOfNodes

let rec checkDescendantReal (ancestor:Node) (descendant:Node) : bool=
    //printfn "ancestor______________________________________________________________\n%A" ancestor 
    //printfn "descendant________________________________________________________________\n%A" descendant

    if ancestor = descendant then true
    else if List.isEmpty ancestor.children then false
    else List.exists (fun x-> checkDescendantReal x descendant) ancestor.children

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
            let candidates = checkRegex root sexp            
            match candidates with
            | [] -> []
            | _ ->
                match relation with
                | Descendant | DirectDescendant ->
                    let isGoodDominantsFunction =
                        match relation with
                        | Descendant -> checkDescendant
                        | DirectDescendant -> checkDirectDescendant   
                    let isGoodCandidateFunction =
                        match relation with
                        | Descendant -> checkDescendantReal    
                        | DirectDescendant -> checkDirectDescendantReal
                    let possibleDominantsOfCandidate = isGoodDominantsFunction root exp.dominant (List.item 0 candidates).value
                    List.filter (fun dominant -> List.contains true <| List.map (isGoodCandidateFunction dominant) candidates) possibleDominantsOfCandidate
                | Sibling | Precedent | ImmediatePrecedent ->
                    let isGoodDominantsFunction =
                        match relation with
                        | Sibling -> checkSibling
                        | Precedent -> checkPrecedent
                        | ImmediatePrecedent -> checkImmediatePrecedent
                    let isGoodCandidateFunction =
                        match relation with
                        | Sibling -> checkSiblingReal
                        | Precedent -> checkPrecedentReal
                        | ImmediatePrecedent -> checkImmediatePrecedentReal
                    let possibleDominantsOfCandidate = List.map fst <| isGoodDominantsFunction root exp.dominant (List.item 0 candidates).value
                    List.filter (fun dom -> (List.contains true <| List.map (fun cand -> isGoodCandidateFunction dom cand) candidates)) possibleDominantsOfCandidate
                // printfn "we have candidates, after filter"
        List.ofSeq (Set.intersectMany <| List.map (checkPerSubject >> set) subjects)
    | None ->
        find root exp.dominant

type IndexPath = int list
let getTRegexThroughPath (tregex: TRegex) (indexPath : IndexPath) (map : TRegex -> TRegex) : TRegex option =
    let rec helper tregex indexPath : TRegex option =
        match indexPath with
        | [] ->
            Some (map tregex)
        | index::xs ->
            Some {
                dominant = tregex.dominant;
                subjects = 
                    match tregex.subjects with
                    | None ->
                        None
                    | Some subjectList ->
                        let (relation, treg) = List.item index subjectList
                        let optionItem = helper treg xs
                        match optionItem with
                        | None ->
                            // TODO: CHECK
                            Some subjectList
                        | Some item ->
                            Some <| List.concat [List.take index subjectList; [(relation, item)]; List.skip (index + 1) subjectList]
            }
    helper tregex indexPath

// returns different versions of root, all with the deepest elements changed to have "expression" added to their subjects
let addToDeepestElements (root: TRegex) (expression: TRegex): TRegex list =
    let rec deepestElementsHelper (root:TRegex) (depth : int) (pathSoFar : IndexPath) : int * IndexPath list =
        match root.subjects with
        | None ->
            (depth, [pathSoFar])
        | Some subjectList ->
            let results = List.mapi (fun index (relation, subject) -> deepestElementsHelper subject (depth + 1) (List.append pathSoFar [index])) subjectList
            let maxDepth = List.max <| List.map fst results
            let rec f (results : (int * IndexPath list) list) : IndexPath list =
                match results with
                | [] -> []
                | (depth, items)::xs ->
                    if depth = maxDepth then
                        List.append items (f xs)
                    else
                        f xs
            (depth, f results)

    let addExpressionToTRegex tregex =
        {
            dominant = tregex.dominant;
            subjects =  
                match tregex.subjects with
                | Some listOfSubjects ->
                    let (relation, firstTregex) = List.item 0 listOfSubjects
                    let updatedFirst =
                        match firstTregex.subjects with
                        | None ->
                            {
                                dominant = firstTregex.dominant;
                                subjects = Some [(Relation.Descendant, expression)]
                            }
                        | Some firstSubjects ->
                            {
                                dominant = firstTregex.dominant;
                                subjects = Some <| List.append firstSubjects [(Relation.Descendant, expression)]
                            }
                    Some ((relation, updatedFirst)::(List.skip 1 listOfSubjects))
                | None ->
                    Some [(Relation.Descendant, expression)]
        }
    let indexPaths = snd (deepestElementsHelper root 0 [])
    if List.isEmpty indexPaths || List.length indexPaths = 1 && List.isEmpty (List.head indexPaths) then
        [addExpressionToTRegex root]
    else
        List.choose id <| List.map (fun indexPath -> getTRegexThroughPath root indexPath addExpressionToTRegex) indexPaths
    


let collectDomains (term : Term) (baseExpressions: TRegex list) : TRegex list =
    let rec collectDomainsHelper (term:Term) (exp: TRegex) (baseExpressions: TRegex list) : TRegex list = 
        // printfn "collectDomains: %A" exp
        let tree = termToNode term
        let matches = checkRegex tree exp
        match matches with
        | [] ->
            // printfn "   no matches for checkRegex" 
            []
        | _ ->
            // printfn "   matches for checkRegex"
            let perExpression (expression: TRegex) : TRegex list =
                let variations = addToDeepestElements exp expression
                List.collect (fun variation -> collectDomainsHelper term variation baseExpressions) variations
            exp::List.collect perExpression baseExpressions
    List.collect (fun baseExpression -> collectDomainsHelper term baseExpression baseExpressions) baseExpressions
