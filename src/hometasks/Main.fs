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
            | Subtask_1 
            | Subtask_2
            | Subtask_3
            | Subtask_4
            | Subtask_5
            | Subtask_6
            interface IArgParserTemplate with
                member s.Usage =
                    match s with
                    | Subtask_1 -> "run Subtask_1"
                    | Subtask_2 -> "run Subtask_2"
                    | Subtask_3 -> "run Subtask_3"
                    | Subtask_4 -> "run Subtask_4"
                    | Subtask_5 -> "run Subtask_5"
                    | Subtask_6 -> "run Subtask_6"
    
        [<EntryPoint>]
        let main (argv: string array) =
            let parser = ArgumentParser.Create<CLIArguments>(programName = "hometasks")
    
            let results = parser.Parse(argv)
            if results.Contains Subtask_1 then
    
                printf "Enter the number: "
                let number = Console.ReadLine() |> float
                let current = hometasks.Hometask_2.Subtask_1 number
                printfn "The result of doing first subtask. = %A" current
    
            elif results.Contains Subtask_2 then
    
                printf "Enter the number: "     
                let number = Console.ReadLine() |> float
                let current = hometasks.Hometask_2.Subtask_2 number
                printfn "The result of doing second subtask. = %A" current
    
            elif results.Contains Subtask_3 then
    
                printf "Enter a number of array elements: "
                let amount_of_elements = Console.ReadLine() |> int
                let random_array: int array = hometasks.Hometask_2.create_array amount_of_elements
                printf "Enter a number that the array elements must not be larger than: "
                let maximum = Console.ReadLine() |> int
                let result = hometasks.Hometask_2.Subtask_3 random_array maximum
                printfn "%A" result
    
            elif results.Contains Subtask_4 then
    
                printf "Enter amount of array elements: "
                let amount_of_elements = Console.ReadLine() |> int
                let random_array: int array = hometasks.Hometask_2.create_array amount_of_elements
                printf "Enter left limit of the range:"
                let left_limit =  Console.ReadLine() |> int
                printf "Enter right limit of the range:"
                let right_limit =  Console.ReadLine() |> int
                let result = hometasks.Hometask_2.Subtask_4 random_array left_limit right_limit
                printf "Indices of array elements that out-of-range: "
                printfn "%A" result

            elif results.Contains Subtask_5 then

                let amount_of_elements = 2
                let random_array: int array = hometasks.Hometask_2.create_array amount_of_elements
                let result = hometasks.Hometask_2.Subtask_5 random_array
                printf "Changed array: "
                printfn "%A" result

            elif results.Contains Subtask_6 then

                printf "Enter amount of array elements: "
                let amount_of_elements = Console.ReadLine() |> int
                let random_array: int array = hometasks.Hometask_2.create_array amount_of_elements
                printf "Enter indices of array elements that needs to be replaced: "
                let i =  Console.ReadLine() |> int
                let j =  Console.ReadLine() |> int
                if (i > -1) && (i < amount_of_elements) && (j > -1) && (j < amount_of_elements) && (i <> j) then
                    let result = hometasks.Hometask_2.Subtask_6 random_array i j
                    printfn "Changed array: "
                    printfn "%A" result
                else printf "You've entered incorrect indices"

            else
                parser.PrintUsage() |> printfn "%s"
            0
