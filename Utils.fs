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