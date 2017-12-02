module TRegex

open AlgebraProblemGenerator
open Utils

type Node=
| Constant of Constant
| Variable of Variable
| Trig of Trig
| InvTrig of InvTrig
| UnaryOp of UnaryOp
| BinaryOp of BinaryOp
| AssociativeOp of AssociativeOp
| TNode of Node * Node list

type Relation =
| Descendant
| DirectDescendant
| Sibling
| Precedent
| ImmediatePrecedent

type TregRelation = 
| Plain of Node * Relation * Node
| Complex of TregRelation * Relation * TregRelation

let rec find (root:Node) (target:Node) : Node list =
    match root with
    | TNode (node,children) -> 
        let res=List.collect (fun child -> find child target) children
        if node.Equals(target) then
            List.append [node] res
        else
            res
    | _ ->
        if target.Equals(root) then [root]
        else []

let rec getAll (root:Node) : Node list =
    match root with
    | TNode (node,children) ->
        let res=List.collect getAll children
        List.append [node] res
    | _ -> [root]    

let getChildren (node:Node) : Node list=
    match node with
    | TNode (node,children) -> children
    | _ -> [] 

let checkDescendant (root:Node) (ancestor:Node) (descendant:Node) : bool = 
    let ancestors = find root ancestor
    List.collect (fun ancestor -> find ancestor descendant) ancestors |> List.isEmpty |> not
         
let checkDirectDescendant (root:Node) (parent:Node) (child:Node) : bool = 
    let parents=find root parent
    List.collect getChildren parents |> List.isEmpty |> not

let checkSibling (root:Node) (siblings:Node list) : bool = 
    (List.map (getChildren >> (containsList siblings)) (getAll root)) |> List.isEmpty |> not

let checkPrecedent (root:Node) (siblings:Node list) : bool = 
    (List.map (getChildren >> (containsListInOrder siblings)) (getAll root)) |>  List.isEmpty |> not

let checkImmediatePrecedent (root:Node)  (siblings:Node list): bool = 
    (List.map (getChildren >> (containsExactList siblings)) (getAll root)) |>  List.isEmpty |> not
