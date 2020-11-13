namespace hometask4

module Main =
    open Argu
    open System

    type CLIArguments =
        | BubbleSortArray
        | BubbleSortList
        | QuickSortArray
        | QuickSortList
        | QuickSortListWithFilters
        | Pack32Into64
        | Pack16Into64
        | Unpack64Into32
        | Unpack64Into16
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | BubbleSortArray -> "bubbleSortArray"
                | BubbleSortList -> "BubbleSortList"
                | QuickSortArray -> "QuickSortArray"
                | QuickSortList -> "QuickSortList"
                | QuickSortListWithFilters -> "QuickSortListWithFilters"
                | Pack32Into64 -> "Packing two 32bit numbers"
                | Pack16Into64 -> "Packing four 16bit numbers"
                | Unpack64Into32 -> "Unpacking 64bit number into two 32bit numbers"
                | Unpack64Into16 -> "Unpacking 64bit number into four 16bit numbers"


    [<EntryPoint>]
    let main (argv: string array) =
        let parser = ArgumentParser.Create<CLIArguments>(programName = "hometask4")
        try
        let results = parser.Parse argv

        let readFuncs f1 f2 f3 =
            printfn "Enter path to input file"
            let input = Console.ReadLine () 
            printfn "Enter path to output file"
            let output = Console.ReadLine () 
            let result = f1 (f2 input)
            f3 output result

        if results.Contains BubbleSortArray
        then readFuncs hometask4.bubbleSortArray hometask4.readArray hometask4.writeArray

        elif results.Contains BubbleSortList
        then readFuncs hometask4.bubbleSortList hometask4.readList hometask4.writeList

        elif results.Contains QuickSortArray
        then readFuncs hometask4.quickSortArray hometask4.readArray hometask4.writeArray

        elif results.Contains QuickSortList
        then readFuncs hometask4.quickSortList hometask4.readList hometask4.writeList

        elif results.Contains QuickSortListWithFilters
        then readFuncs hometask4.quickSortListWithFilters hometask4.readList hometask4.writeList

        elif results.Contains Pack32Into64 then
            printfn "Enter two int32 numbers: "
            let x = Console.ReadLine () |> int32
            let y = Console.ReadLine () |> int32
            let result = hometask4.packing32To64 (x, y)
            printfn "%A" result

        elif results.Contains Unpack64Into32 then
            printfn "Enter int64 number: "
            let x = Console.ReadLine () |> int64
            let result = hometask4.unpacking64To32 x
            printfn "%A" result

        elif results.Contains Pack16Into64 then
            printfn "Enter four int16 numbers:"
            let x = Console.ReadLine () |> int16
            let y = Console.ReadLine () |> int16
            let z = Console.ReadLine () |> int16
            let a = Console.ReadLine () |> int16
            let result = hometask4.packing16To64 (x, y, z, a)
            printfn "%A" result

        elif results.Contains Unpack64Into16 then
            printfn "Enter int64 number: "
            let x = Console.ReadLine () |> int64
            let result = hometask4.unpacking64To16 x
            printfn "%A" result

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
