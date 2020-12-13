module Main
open Argu
open System
open hometask6

    type CLIArguments =
        | MultiplyMatrices
        interface IArgParserTemplate with
               member s.Usage =
                   match s with
                   | MultiplyMatrices _ -> "multiplies"

    [<EntryPoint>]
    let main (argv: string array) =
        let parser = ArgumentParser.Create<CLIArguments>(programName = "hometask6")
        let results = parser.Parse(argv)
        if results.Contains MultiplyMatrices
        then
            printfn "Enter paths to 2 matrices"
            let inputPath1 = Console.ReadLine()
            let matrix1 = readBoolMatrix inputPath1
            let inputPath2 = Console.ReadLine()
            let matrix2 = readBoolMatrix inputPath2
            printfn "Enter path to output file"
            let outputPath = Console.ReadLine()
            writeMatrix (multiplyBoolMatrices matrix1 matrix2) outputPath                              
        else           
            parser.PrintUsage() |> printfn "%s"
        0
       
        
