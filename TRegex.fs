module TRegex

open AlgebraProblemGenerator
open Utils
open System.Collections.Generic

type NodeValue=
| Constant of Constant
| Variable of Variable
| Trig of Trig
| InvTrig of InvTrig
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

let rec term2Node (root:Term) (parent : Node option): Node =
    match root with
    | TConstant constant ->  {parent=parent;children=[];value=Constant constant} 
    | TVariable variable -> {parent=parent;children=[];value=Variable variable}
    | UnaryTerm (op,term)->
        let res = {parent=parent;children=[];value=UnaryOp op}
        res.children <- [term2Node term (Some res)]
        res
    | BinaryTerm (term1,op,term2) -> 
        let res = {parent=parent;children=[];value=BinaryOp op}
        res.children <- [term2Node term1 (Some res);term2Node term2 (Some res)]
        res
    | AssociativeTerm (op,terms) ->
        let res = {parent=parent;children=[];value=AssociativeOp op}
        res.children <- List.map (fun term-> term2Node term parent) terms
        res

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

let rec checkRegex (root:Node) (exp:TRegex) : Node list =
    if exp.subjects.IsSome then find root exp.dominant
    else 
        for (relation, sexp) in exp.subjects do
            let candidates = checkRegex root sexp
            for candidate in candidates do
                match relation with
                | Descendant ->
                    if not (checkDescendantReal exp.dominant candidate) then false
                    else    
             
             


