module hometask6 

    [<Measure>]
    type _row

    [<Measure>]
    type _column

    [<Struct>]
    type Coordinate =
        val I: int<_row>
        val J: int<_column>
        new(i, j) = {I = i; J = j}

    [<Struct>]
    type Matrix =
        val Rows: int
        val Cols: int
        val Coordinates: list<Coordinate>
        new(rows, cols, lst) = {Rows = rows; Cols = cols; Coordinates = lst}


    let checkMatrixSize rows = 
        let lengths = rows |> Array.map String.length |> Array.distinct
        if lengths.Length > 1 then failwith "Expected valid matrix size"


    let readBoolMatrix file =
        let matrixToListOfCoords (str: string) (i, lst) =
            str.Split ' ' |> Array.fold (fun (j, lst) c ->
                match c with
                | "0" -> (j + 1, lst)
                | "1" -> (j + 1, Coordinate (i * 1<_row>, j * 1<_column>) :: lst)
                | _ -> failwith "Expected bool matrix") (0, lst)
        let rows = System.IO.File.ReadAllLines file
        checkMatrixSize rows
        let (n, (m, acc)) = rows |> Array.fold (fun (i, lst) str -> (i + 1, matrixToListOfCoords str (i, snd lst))) (0, (0, []))
        Matrix (n, m, acc)
   


    let multiplyBoolMatrices (matrix1: Matrix) (matrix2: Matrix) =
        if matrix1.Rows <> matrix2.Cols then failwith "Matrices aren't matched"
        else
            let lst =
                [for i in matrix1.Coordinates do
                    for j in matrix2.Coordinates do
                    if int i.J = int j.I then Coordinate (i.I, j.J)]
            Matrix (matrix1.Rows, matrix2.Cols, List.distinct lst)


    let boolSparseMatrixToArrayOfStrings (matrix: Matrix) =
        let array2d = Array.init matrix.Rows (fun _ -> Array.create matrix.Cols "0")
        for i in matrix.Coordinates do
            array2d.[int i.J].[int i.I] <- "1"
        Array.map (String.concat " ") array2d


    let writeMatrix (matrix: Matrix) outputPath =
        System.IO.File.WriteAllLines (outputPath, boolSparseMatrixToArrayOfStrings matrix)
