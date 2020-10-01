namespace hometasks

open System.Reflection

module AssemblyInfo =

    let metaDataValue (mda: AssemblyMetadataAttribute) = mda.Value

    let getMetaDataAttribute (assembly: Assembly) key =
        assembly.GetCustomAttributes(typedefof<AssemblyMetadataAttribute>)
        |> Seq.cast<AssemblyMetadataAttribute>
        |> Seq.find (fun x -> x.Key = key)

    let getReleaseDate assembly =
        "ReleaseDate"
        |> getMetaDataAttribute assembly
        |> metaDataValue

    let getGitHash assembly =
        "GitHash"
        |> getMetaDataAttribute assembly
        |> metaDataValue

    let getVersion assembly =
        "AssemblyVersion"
        |> getMetaDataAttribute assembly
        |> metaDataValue

    let assembly = lazy (Assembly.GetEntryAssembly())

    let printVersion() =
        let version = assembly.Force().GetName().Version
        printfn "%A" version

    let printInfo() =
        let assembly = assembly.Force()
        let name = assembly.GetName()
        let version = assembly.GetName().Version
        let releaseDate = getReleaseDate assembly
        let githash = getGitHash assembly
        printfn "%s - %A - %s - %s" name.Name version releaseDate githash

module Say =
    open System

    let nothing name = name |> ignore

    let hello name = sprintf "Hello %s" name

    let colorizeIn color str =
        let oldColor = Console.ForegroundColor
        Console.ForegroundColor <- (Enum.Parse(typedefof<ConsoleColor>, color) :?> ConsoleColor)
        printfn "%s" str
        Console.ForegroundColor <- oldColor

module Main =
    open Argu
    open System

        type CLIArguments =
            | Subtask1 
            | Subtask2
            | Subtask3
            | Subtask4
            | Subtask5
            | Subtask6
            interface IArgParserTemplate with
                member s.Usage =
                    match s with
                    | Subtask1 -> "run Subtask1"
                    | Subtask2 -> "run Subtask2"
                    | Subtask3 -> "run Subtask3"
                    | Subtask4 -> "run Subtask4"
                    | Subtask5 -> "run Subtask5"
                    | Subtask6 -> "run Subtask6"
    
        [<EntryPoint>]
        let main (argv: string array) =
            let parser = ArgumentParser.Create<CLIArguments>(programName = "hometasks")
    
            let results = parser.Parse(argv)
            if results.Contains Subtask1 then
    
                printf "Enter the number: "
                let number = Console.ReadLine() |> float
                let current = hometasks.Hometask2.subtask1 number
                printfn "The result of doing first subtask. = %A" current
    
            elif results.Contains Subtask2 then
    
                printf "Enter the number: "     
                let number = Console.ReadLine() |> float
                let current = hometasks.Hometask2.subtask2 number
                printfn "The result of doing second subtask. = %A" current
    
            elif results.Contains Subtask3 then
    
                printf "Enter a number of array elements: "
                let amountOfElements = Console.ReadLine() |> int
                let randomArray: array<int> = hometasks.Hometask2.createArray amountOfElements
                printf "Enter a number that the array elements must not be larger than: "
                let maximum = Console.ReadLine() |> int
                let result = hometasks.Hometask2.subtask3 randomArray maximum
                printfn "%A" result
    
            elif results.Contains Subtask4 then
    
                printf "Enter amount of array elements: "
                let amountOfElements = Console.ReadLine() |> int
                let randomArray: array<int> = hometasks.Hometask2.createArray amountOfElements
                printf "Enter left limit of the range:"
                let leftLimit =  Console.ReadLine() |> int
                printf "Enter right limit of the range:"
                let rightLimit =  Console.ReadLine() |> int
                let result = hometasks.Hometask2.subtask4 randomArray leftLimit rightLimit
                printf "Indices of array elements that out-of-range: %A" result

            elif results.Contains Subtask5 then

                let amountOfElements = 2
                let randomArray: array<int> = hometasks.Hometask2.createArray amountOfElements
                let result = hometasks.Hometask2.subtask5 randomArray
                printf "Changed array: "
                printfn "%A" result

            elif results.Contains Subtask6 then

                printf "Enter amount of array elements: "
                let amountOfElements = Console.ReadLine() |> int
                let randomArray: array<int> = hometasks.Hometask2.createArray amountOfElements
                printf "Enter indices of array elements that needs to be replaced: "
                let i =  Console.ReadLine() |> int
                let j =  Console.ReadLine() |> int
                let result = hometasks.Hometask2.subtask6 randomArray i j
                printfn "Changed array: "
                printfn "%A" result

            else
                parser.PrintUsage() |> printfn "%s"
            0
