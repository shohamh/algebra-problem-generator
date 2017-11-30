module TRegex

open AlgebraProblemGenerator

type Node=
| Constant
| Variable
| Trig
| InvTrig
| UnaryOp
| BinaryOp
| AssociativeOp
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

let rec find (root:Node) (target:Node) : int =
    match target with
    | root -> 1
    | _ ->
        match root with
        | TNode (node,children) ->
            for child in children do



//let rec checkDescendant (root:Node) (ancestor:Node) (descendant:Node) : int = 1
//let rec checkDirectDescendant (root:Node) (parent:Node) (child:Node) : int = 1
//let rec checkSibling (root:Node) (sib1:Node) (sib2:Node) : int = 1
//let rec checkPrecedent (root:Node) (first:Node) (second:Node) : int = 1
//let rec checkImmediatePrecedent (root:Node) (first:Node) (second:Node) : int = 1

