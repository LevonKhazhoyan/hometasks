module QuadTree

open System
open SparseMatrix
open System.Collections.Generic
open AlgebraicStructure

let nearestPowerOfTwo (x: int, y: int) =
    let mutable power = 1
    let maximum = max x y
    while maximum > power do
        power <- power * 2
    power

type QuadTree<'t when 't : equality> =
    | None
    | Leaf of 't
    | Node of QuadTree<'t> * QuadTree<'t> * QuadTree<'t> * QuadTree<'t>

    static member nodeCheckForNone (x1:QuadTree<'t>, x2:QuadTree<'t>, x3:QuadTree<'t>, x4:QuadTree<'t>) =
        if x1 = None && x2 = None && x3 = None && x4 = None then None
        else Node (x1, x2, x3, x4)

    static member LeafCheckForNone (x:'t) (monoid: Monoid<'t>) =
        if x = monoid.Neutral then None
        else Leaf x

type QuadTreeMtx<'t when 't : equality> =
    val Rows: int
    val Cols: int
    val Tree: QuadTree<'t>
    new(r,c,t) = {Rows = r; Cols = c; Tree = t}
        
    static member sum (x: QuadTreeMtx<'t>) (y: QuadTreeMtx<'t>) (monoid: Monoid<'t>) =
        if x.Rows = y.Rows && x.Cols = y.Cols then
            let rec _go x y =
                match x, y with
                | Leaf a, Leaf b ->
                    QuadTree.LeafCheckForNone (monoid.Sum a b) monoid
                | None, a -> a
                | a, None -> a
                | Node (v1, v2, v3, v4), Node (v5, v6, v7, v8) ->
                    QuadTree.nodeCheckForNone (_go v1 v5, _go v2 v6, _go v3 v7, _go v4 v8 )
                | _, _ -> failwith "can't sum matrices of diff size" // just to complete pattern matching
            QuadTreeMtx(x.Rows, y.Cols, _go x.Tree y.Tree)
        else failwith "can't sum matrices of diff sizes"

    static member convert (xs: QuadTreeMtx<'t>) = 
        let size = nearestPowerOfTwo (xs.Rows, xs.Cols)
        let hash = new HashSet<_>()
        let rec _go a b c d xs = 
            match xs with
            | Leaf p ->
                hash.Add(Coordinate(a, c, p)) |> ignore
            | Node (dir1, dir2, dir3, dir4) ->
                let rowHalf = a + (b - a) / 2
                let colHalf = c + (d - c) / 2
                _go a rowHalf c colHalf dir1
                _go a rowHalf (colHalf + 1) d dir2
                _go (rowHalf + 1) b c colHalf dir3
                _go (rowHalf + 1) b (colHalf + 1) d dir4
            | None -> ()
        _go 0 (size - 1) 0 (size - 1) xs.Tree
        SparseMatrix(xs.Rows, xs.Cols, List.ofSeq hash)

    static member create (xs: SparseMatrix<'t>) =
        let rec _go rowBorder colBorder =
            if SparseMatrix.isEmptySparseMatrix xs rowBorder colBorder then None
            else match rowBorder, colBorder with
                 | (a, b), (c, d) when a = b -> Leaf(SparseMatrix.get xs a c)
                 | (a, b), (c, d) ->
                    let rowHalf = a + (b - a) / 2
                    let colHalf = c + (d - c) / 2
                    Node (_go (a, rowHalf) (c, colHalf),
                         _go (a, rowHalf) (colHalf + 1, d),
                         _go (rowHalf + 1, b) (c, colHalf),
                         _go (rowHalf + 1, b) (colHalf + 1, d))
        let border = nearestPowerOfTwo (xs.NumOfCols,xs.NumOfRows)
        QuadTreeMtx(xs.NumOfRows, xs.NumOfCols, (_go (0, border - 1) (0, border - 1)))

    static member multiplyByScalar (xs: QuadTreeMtx<'t>) (semiRing: SemiRing<'t>) (a: 't) =
        if a = semiRing.Monoid.Neutral then QuadTreeMtx(xs.Rows, xs.Cols, None)
        else
            let rec _go (x: QuadTree<'t>) =
                match x with
                | Leaf v -> Leaf (semiRing.Mul a v)
                | Node (v1, v2, v3, v4) -> Node(_go v1, _go v2, _go v3, _go v4)
                | None -> None
            QuadTreeMtx(xs.Rows, xs.Cols, _go xs.Tree)

    static member tensorMultiply (x: QuadTreeMtx<'t>) (y: QuadTreeMtx<'t>) (semiRing: SemiRing<'t>) =
        let rec _go (xs: QuadTree<'t>) =
            match xs with
            | Leaf v -> (QuadTreeMtx.multiplyByScalar y semiRing v).Tree
            | Node (v1, v2, v3, v4) -> QuadTree.nodeCheckForNone (_go v1, _go v2, _go v3, _go v4)
            | None -> None
        QuadTreeMtx(x.Rows * y.Rows, x.Cols * y.Cols, _go x.Tree)
