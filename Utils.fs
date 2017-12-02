module Utils

let rec insert v i l =
    match i, l with
    | 0, xs -> v::xs
    | i, x::xs -> x::insert v (i - 1) xs
    | i, [] -> failwith "index out of range"

let rec remove i l =
    match i, l with
    | 0, x::xs -> xs
    | i, x::xs -> x::remove (i - 1) xs
    | i, [] -> failwith "index out of range"

// intersperse an element between each pair of elements in a list
//TODO: test this shit
let rec intersperse (elem: 'T) (lst: 'T list) =
    match lst with
    | [] -> []
    | [x] -> [x]
    | x::xs -> x::elem::(intersperse elem xs)

let split (predicate: 'T -> bool) (lst: 'T list) : 'T list list =
    let rec splitUtil acc lst =
        let beforePredicate = List.takeWhile (predicate >> not) lst
        let afterPredicate = List.skip beforePredicate.Length lst
        match afterPredicate with
        | x::rest when predicate x -> splitUtil (beforePredicate::acc) rest // x is the element to split with
        | _ -> beforePredicate::acc
    List.rev <| splitUtil [] lst

let rec merge (list1: 'T list) (list2: 'T list) : 'T list =
    match list1, list2 with
    | xs, [] -> xs
    | [], ys -> ys
    | x::xs, y::ys -> x::y::(merge xs ys)

let containsList (list1:'T list) (list2:'T list) : bool =
    List.exists ((=) false) <| List.map (fun x -> List.contains x list1) list2

let containsListInOrder (list1:'T list) (list2:'T list) : bool =
    let rec containsListInOrderHelper (list1:'T list) (list2:'T list) (idx1:int) (idx2:int) : bool =    
        if idx2 > list2.Length then true
        else if idx1 > list1.Length then false
        else if (list1.Item idx1)=(list2.Item idx2) then containsListInOrderHelper list1 list2 (idx1+1) (idx2+2)
        else containsListInOrderHelper list1 list2 (idx1+1) idx2
    containsListInOrderHelper list1 list2 0 0

let containsExactList (list1:'T list) (list2:'T list) : bool =
    let rec containsListInOrderHelper (list1:'T list) (list2:'T list) (idx1:int) (idx2:int) : bool =    
        if idx2 > list2.Length then true
        else if idx1 > list1.Length then false
        else if (list1.Item idx1)=(list2.Item idx2) then containsListInOrderHelper list1 list2 (idx1+1) (idx2+2)
        else containsListInOrderHelper list1 list2 (idx1+1) idx2
    containsListInOrderHelper list1 list2 0 0