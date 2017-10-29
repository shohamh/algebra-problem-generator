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

/// The intersperse function takes an element and a list and
/// 'intersperses' that element between the elements of the list.
let intersperse sep ls =
    List.foldBack (fun x -> function
        | [] -> [x]
        | xs -> x::sep::xs) ls []
