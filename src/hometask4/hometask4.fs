namespace hometask4

module hometask4 =

    let arrayOfIntIntoArrayOfStrings (array: array<int>) =
        Array.map string array

    let readArray file =
        let lines = System.IO.File.ReadAllLines file
        lines
        |> Array.map (fun x -> int (x.Trim())) 

    let readList file =
        readArray file |> Array.toList

    let writeArray file array = System.IO.File.WriteAllLines (file, arrayOfIntIntoArrayOfStrings array)
    let writeList file list = System.IO.File.WriteAllLines (file, arrayOfIntIntoArrayOfStrings (List.toArray list))

    let bubbleSortArray (xs: array<_>) =
        if xs.Length >= 2 then
            let mutable swapped = true
            let mutable temp = 0
            while swapped = true do
                swapped <- false
                for i = 0 to xs.Length - 2 do
                    if xs.[i + 1] < xs.[i] 
                    then
                        temp <- xs.[i]
                        xs.[i] <- xs.[i + 1]
                        xs.[i + 1] <- temp
                        swapped <- true
        xs

    let bubbleSortList (xs: list<_>) =
        let rec loop xs = 
            match xs with
            | first :: second :: rest -> 
                if first > second
                then second :: loop (first :: rest)
                else first :: loop (second :: rest)
            | x -> x 
     
        let rec tailRec (xs: list<_>) a =
            if a = xs.Length
            then xs
            else tailRec (loop xs) (a + 1)

        tailRec xs 0


    let quickSortArray (xs: array<_>) =

        let partition (xs: array<_>) l r =
            let pivot = xs.[r]
            let mutable m = l - 1
            let mutable temp = 0
            for j = l to r - 1 do
                if xs.[j] <= pivot
                then
                    m <- m + 1
                    temp <- xs.[m]
                    xs.[m] <- xs.[j]
                    xs.[j] <- temp
            temp <- xs.[m + 1]
            xs.[m + 1] <- xs.[r]
            xs.[r] <- temp
            m + 1
    
        let rec loop (xs: array<_>) l r =
            if l < r
            then
                let m = partition xs l r
                loop xs l (m - 1)
                loop xs (m + 1) r
    
        loop xs 0 (xs.Length - 1)

        xs


    let quickSortList (xs: list<_>)  =

        let partition compare xs =
            let rec loop lst smaller larger =
                match lst with
                | [] -> smaller, larger
                | first :: rest ->
                    if compare first
                    then loop rest (first :: smaller) larger
                    else loop rest smaller (first :: larger)

            loop xs [] []

        let rec score lst =
            match lst with
            | [] -> []
            | [x] -> [x]
            | first :: rest ->
                let left, right = partition ((>) first) rest
                (score left) @ first :: (score right)

        score xs

    let quickSortListWithFilters (xs: list<_>) =
        let rec qsort xs =
            match xs with
            | [] -> []
            | xs ->
                let pivot = System.Random().Next(0, xs.Length)
                let smaller = qsort (xs |> List.filter(fun e -> e < xs.[pivot]))
                let equal = xs |> List.filter(fun e -> e = xs.[pivot])
                let larger  = qsort (xs |> List.filter(fun e -> e > xs.[pivot]))
                smaller @ equal @ larger
                
        qsort xs

    let packing32To64 (a, b) =
        if b >= 0
        then (a |> int64 <<< 32) + (b |> int64)
        else (a |> int64 <<< 32) + 4294967296L + (b |> int64)
    
    let packing16To32 (a: int16, b) =
        if b >= 0s
        then (int32 a <<< 16) + (int32 b)
        else (int32 a <<< 16) + 65536 + (int32 b)
    
    let packing16To64 (a, b, c, d) =
        packing32To64 (packing16To32 (a, b), packing16To32 (c, d))
    
    let unpacking64To32 (a: int64) =
        (a >>> 32 |> int32, (a <<< 32) >>> 32 |> int32)
    
    let unpacking32To16 (a) =
        (a >>> 16 |> int16, (a <<< 16) >>> 16 |> int16)

    let unpacking64To16 (a) =
        let ab, cd = unpacking64To32 a
        let res1, res2 = unpacking32To16 ab
        let res3, res4 = unpacking32To16 cd
        (res1, res2, res3, res4)


