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
