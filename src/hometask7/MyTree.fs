module MyTree

open MyList
open System

type MyTree<'t> =
    | Leaf of 't
    | Node of 't * MyList<MyTree<'t>>

let rec fold folder acc tree =
    match tree with
    | Leaf x -> folder acc x
    | Node (x, tail) -> MyList.fold (fun acc t -> fold folder acc t) (folder acc x) tail

let max tree =
    fold (fun max x -> if x > max then x else max) Int32.MinValue tree

let average tree =
    let x, y = fold (fun (sum, count) x -> (sum + x, count + 1)) (0, 0) tree
    float (x/y)
