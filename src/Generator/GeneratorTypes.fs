module GeneratorTypes
open System

type GeneratorType =
    | Int
    | Float
    | Bool

[<Struct>]
type GeneratorStats =
    val Rows: int
    val Cols: int
    val Amount: int
    val Sparsity: float // percentage of neutral elements
    val Path: string
    val DataType: GeneratorType
    new (rows, cols, amount, sparsity, path, dataType) =
        { Rows = rows; Cols = cols; Amount = amount;
        Sparsity = sparsity; Path = path; DataType = dataType}

    static member printMatrix (xs: string [,]) path =
        let mutable text = ""
        for i = 0 to (Array2D.length1 xs) - 1 do
            for j = 0 to (Array2D.length2 xs) - 1 do
                text <- text + xs.[i, j] + " "
            text <- text + "\n"
        IO.File.WriteAllText (path, text)

    static member generateSparseMatrix (xs: GeneratorStats) =
        for i = 1 to xs.Amount do
            let output = Array2D.zeroCreate xs.Rows xs.Cols
            for j = 0 to xs.Rows - 1 do
                for k = 0 to xs.Cols - 1 do             
                    if Random().NextDouble() > xs.Sparsity
                    then
                        output.[j, k] <- (match xs.DataType with
                                         | Int -> string (Random().Next())
                                         | Float -> string (float(Random().Next()) + Random().NextDouble())
                                         | Bool -> "1")
                    else output.[j, k] <- "0"
            GeneratorStats.printMatrix output (IO.Path.Combine (xs.Path, "Matrix" + string i + ".txt"))

