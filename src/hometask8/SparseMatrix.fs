module SparseMatrix

open System
open System.Collections.Generic

type Coordinate<'t> =
    val Row: int
    val Col: int
    val Value: 't
    new(n, m, value) = {Row = n; Col = m; Value = value} 

type SparseMatrix<'t> =
    val NumOfRows: int
    val NumOfCols: int
    val Coordinates: list<Coordinate<'t>>
    new(rows, cols, lst) = {NumOfRows = rows; NumOfCols = cols; Coordinates = lst}

    static member isEmptySparseMatrix (xs: SparseMatrix<'t>) (rowBorder: int * int) (colBorder: int * int) =
        let (a, b) = rowBorder
        let (c, d) = colBorder
        let pred (coord: Coordinate<'t>) =
            coord.Row <= b && coord.Row >= a && coord.Col <= d && coord.Col >= c 
        not (List.exists pred xs.Coordinates)

    static member get (sparseMatrix: SparseMatrix<'t>) (row: int) (col: int) =
        let pred (coord: Coordinate<'t>) =
            row = coord.Row && col = coord.Col
        (List.head (List.filter pred sparseMatrix.Coordinates)).Value

    static member randomIntSparseMatrix rows cols =
        SparseMatrix(rows, cols, [for i in 0..(rows - 1) do
                                    for j in 0..(cols - 1) do
                                        if Random().Next(0, 2) = 1
                                        then Coordinate(i, j, Random().Next(1, 100))])

    static member sortListOfCoords (xs: list<int * int * 't>) =
        xs |> List.sortBy (fun (a, b, _) -> a, b)

    static member coordinatesToListOfTriples (xs: SparseMatrix<'t>) :list<int * int * 't> =
        List.map (fun (coord: Coordinate<'t>) -> (coord.Row, coord.Col, coord.Value)) xs.Coordinates



