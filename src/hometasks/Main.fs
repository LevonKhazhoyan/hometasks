namespace hometasks



module Main =
    open Argu
    open System

        type CLIArguments =
            | Subtask1_1 
            | Subtask2_1
            | Subtask3_1
            | Subtask4_1
            | Subtask5_1
            | Subtask6_1
            | Subtask1_2 of x: int
            | Subtask2_2 of x: int
            | Subtask3_2 of x: int
            | Subtask4_2 of x: int
            | Subtask5_2 of x: int
            | Subtask6_2 of x: int
            interface IArgParserTemplate with
                member s.Usage =
                    match s with
                    | Subtask1_1 -> "run Subtask1_1"
                    | Subtask2_1 -> "run Subtask2_1"
                    | Subtask3_1 -> "run Subtask3_1"
                    | Subtask4_1 -> "run Subtask4_1"
                    | Subtask5_1 -> "run Subtask5_1"
                    | Subtask6_1 -> "run Subtask6_1"
                    | Subtask1_2 _ -> "run Subtask1_2"
                    | Subtask2_2 _ -> "run Subtask2_2"
                    | Subtask3_2 _ -> "run Subtask3_2"
                    | Subtask4_2 _ -> "run Subtask4_2"
                    | Subtask5_2 _ -> "run Subtask5_2"
                    | Subtask6_2 _ -> "run Subtask6_2"
    
        [<EntryPoint>]
        let main (argv: string array) =
            let parser = ArgumentParser.Create<CLIArguments>(programName = "Hometasks")
            try 
            let results = parser.Parse argv

            let readFuncs f =
                printf "enter the number: "
                let n = Console.ReadLine() |> int
                let result = f n
                printfn "the result of doing ex. = %A" result

            if results.Contains Subtask1_1 then
    
                printf "Enter the number: "
                let number = Console.ReadLine() |> float
                let current = hometasks.Hometask2.subtask1 number
                printfn "The result of doing first subtask. = %A" current
    
            elif results.Contains Subtask2_1 then
    
                printf "Enter the number: "     
                let number = Console.ReadLine() |> float
                let current = hometasks.Hometask2.subtask2 number
                printfn "The result of doing second subtask. = %A" current
    
            elif results.Contains Subtask3_1 then
    
                printf "Enter a number of array elements: "
                let amountOfElements = Console.ReadLine() |> int
                let randomArray: array<int> = hometasks.Hometask2.createArray amountOfElements
                printf "Enter a number that the array elements must not be larger than: "
                let maximum = Console.ReadLine() |> int
                let result = hometasks.Hometask2.subtask3 randomArray maximum
                printfn "%A" result
    
            elif results.Contains Subtask4_1 then
    
                printf "Enter amount of array elements: "
                let amountOfElements = Console.ReadLine() |> int
                let randomArray: array<int> = hometasks.Hometask2.createArray amountOfElements
                printf "Enter left limit of the range:"
                let leftLimit =  Console.ReadLine() |> int
                printf "Enter right limit of the range:"
                let rightLimit =  Console.ReadLine() |> int
                let result = hometasks.Hometask2.subtask4 randomArray leftLimit rightLimit
                printf "Indices of array elements that out-of-range: %A" result

            elif results.Contains Subtask5_1 then

                let amountOfElements = 2
                let randomArray: array<int> = hometasks.Hometask2.createArray amountOfElements
                let result = hometasks.Hometask2.subtask5 randomArray
                printf "Changed array: "
                printfn "%A" result

            elif results.Contains Subtask6_1 then

                printf "Enter amount of array elements: "
                let amountOfElements = Console.ReadLine() |> int
                let randomArray: array<int> = hometasks.Hometask2.createArray amountOfElements
                printf "Enter indices of array elements that needs to be replaced: "
                let i =  Console.ReadLine() |> int
                let j =  Console.ReadLine() |> int
                let result = hometasks.Hometask2.subtask6 randomArray i j
                printfn "Changed array: "
                printfn "%A" result

            elif results.Contains Subtask1_2 then readFuncs Hometask3.fibIter

            elif results.Contains Subtask2_2 then readFuncs Hometask3.fibRec
             
            elif results.Contains Subtask3_2 then readFuncs Hometask3.fibTailRec

            elif results.Contains Subtask4_2 then readFuncs Hometask3.fibByMultMatrices

            elif results.Contains Subtask5_2 then readFuncs Hometask3.fibMultMatricesFaster

            elif results.Contains Subtask6_2 then readFuncs Hometask3.fibNumbersToN
               
            else
                parser.PrintUsage() |> printfn "%s"
            0
            with
            | :? ArguParseException as ex ->
                printfn "%s" ex.Message
                1
            | ex ->
                printfn "Internal Error:"
                printfn "%s" ex.Message
                2
