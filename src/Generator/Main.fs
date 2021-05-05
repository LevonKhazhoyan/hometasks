module Main
open System.IO
open GeneratorTypes
open Argu

type CliArguments =
    | Rows of Rows: int
    | Cols of Cols: int
    | Amount of Amount: int
    | Sparsity of Sparsity: float
    | Path of Path: string
    | Type of DataType: GeneratorType
    
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Rows _ -> "Indicate the number of rows"
            | Cols _ -> "Indicate the number of cols"
            | Amount _ -> "Indicate the number of matrices"
            | Sparsity _ -> "Indicate the sparsity"
            | Path _ -> "Indicate the directory"
            | Type _ -> "Indicate the type of matrices"      

[<EntryPoint>]
let main (argv: string array) =
    let parser = ArgumentParser.Create<CliArguments> (programName = "Generator")
    try
    let x = parser.Parse argv
    if x.GetResult Rows <= 0 || x.GetResult Cols <= 0
    then failwith "Expected positive ints"
    elif x.GetResult Amount <= 0
    then failwith "Expected positive int"
    elif x.GetResult Sparsity < 0.0 || x.GetResult Sparsity > 1.0
    then failwith "Expected a number between 0.0 and 1.0"
    elif not (Directory.Exists (x.GetResult Path))
    then failwith "Expected existing path"
    else
         GeneratorStats.generateSparseMatrix <|
         (GeneratorStats
         (x.GetResult Rows, x.GetResult Cols, x.GetResult Amount,
          x.GetResult Sparsity, x.GetResult Path, x.GetResult Type))
    0    
    with e ->
        printfn "%s" e.Message
        1
