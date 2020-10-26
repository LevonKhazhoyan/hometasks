namespace hometasks


module Hometask3 =
    let rec fibRec n =
        if n < 0 then failwith "expected n >= 0"
        match n with
        | 0 -> 0
        | 1 -> 1
        | n -> fibRec (n - 1) + fibRec (n - 2)


    let fibIter n =
        if n < 0 then failwith "expected n >= 0"
        elif n = 0 then 0
        else
            let mutable fibPrevPrev = 0
            let mutable fibPrev = 1
            let mutable fibCurr = fibPrev + fibPrevPrev
            let mutable i = 1
            while i <> n do
                fibCurr <- fibPrevPrev + fibPrev
                fibPrevPrev <- fibPrev
                fibPrev <- fibCurr
                i <- i + 1
            fibCurr


    let fibTailRec n =
        if n < 0 then failwith "expected n >= 0"
        let rec loop acc1 acc2 n =
            match n with
            | 0 -> acc1
            | 1 -> acc2
            | n -> loop acc2 (acc1 + acc2) (n - 1)
        loop 0 1 n


    let createIdentityMatrix size =
        let identityMatrix = Array2D.zeroCreate size size
        for i = 0 to size - 1 do
            for j = 0 to size - 1 do
                if i = j then identityMatrix.[i,j] <- 1
        identityMatrix


    let matrixMultiply (matrix1: int[,]) (matrix2: int[,]) =
        let rowsMatrix1 = matrix1.[0, *].Length
        let rowsMatrix2 = matrix1.[*, 0].Length
        let columnsMatrix1 = matrix2.[0, *].Length
        let columnsMatrix2 = matrix2.[*, 0].Length
        let result = Array2D.zeroCreate rowsMatrix1 columnsMatrix2
        if rowsMatrix2 = columnsMatrix1 || rowsMatrix1 = columnsMatrix2
        then
            for i = 0 to rowsMatrix1 - 1 do
                for k = 0 to columnsMatrix2 - 1 do
                    for r = 0 to rowsMatrix1 - 1 do
                            result.[i, k] <- result.[i, k] + matrix1.[i, r] * matrix2.[r, k]
            result
        else failwith "Matrices don't match"


    let matrixPow (matrix: int[,]) power =
        if power < 0 then failwith "Matrix can't have negative power"
        elif power = 0
        then
            let rows = matrix.[0,*].Length
            let columns = matrix.[*,0].Length
            if rows = columns
            then createIdentityMatrix rows
            else failwith "Matrix can't have zero power if it isn't square"
        else
            let mutable result = createIdentityMatrix matrix.[0,*].Length
            for i = 1 to power do
                result <- matrixMultiply result matrix
            result
             
    // factorizing n to a powers of 2
    let factorization n =
        if n <= 0
        then failwith "Expected n >= 0"
        let mutable result = []
        let mutable i = n
        while i > 0 do
            let mutable power = 0     
            while (1 <<< power) <= i do
                power <- power + 1
            power <- power - 1
            result <- power :: result
            i <- i - (1 <<< power)
        result 


    // evals matrix ^ (2 ^ pow) 
    let rec matrixBinPow (matrix: int[,]) pow =
        if pow = 0 then matrix
        else matrixPow (matrixBinPow matrix (pow - 1)) 2

    // creating an array of differences between adjacent elements in order not to raise the next matrix to a pow again in matrixBinPow,
    // but only to multiply by the matrix ^ difference between the calculated and the new one
    let diffBetweenPairs (xs: list<int>) =
        if xs.Length < 2 then failwith "expected length >= 2" 
        let pairs = Seq.zip xs.[1..] xs
        Seq.map (fun (x, y) -> x - y) pairs |> Seq.toArray


    let fastMatrixPow n (matrix: int[,]) =
        let factors = factorization n
        if factors.Length = 1 then matrixBinPow matrix factors.[0]
        else
            let mutable x = matrixBinPow matrix factors.[0]
            let difference = diffBetweenPairs factors
            let mutable result = createIdentityMatrix 2
            for i = 0 to (difference.Length - 1) do
                result <- matrixMultiply result x
                x <- matrixBinPow x (difference.[i])
            result <- matrixMultiply result x
            result
            

    let fibByMultMatrices n =
        if n < 0
        then failwith "expected n >= 0"
        else
            let fibMatrix = array2D [ [0; 1]; [1; 1] ]
            let result = matrixPow fibMatrix n
            result.[0, 1]


    let fibMultMatricesFaster n =
        if n < 0 then failwith "expected n >= 0"
        elif n = 0 then 0
        else
             let fibMatrix = array2D [ [0; 1]; [1; 1] ]
             let result = fastMatrixPow n fibMatrix
             result.[0, 1]


    let fibNumbersToN n =
        if n < 0 then failwith "expected n >= 0"
        let rec loop acc1 acc2 n =
            match n with
            | 0 -> []
            | n -> acc1 :: loop acc2 (acc1 + acc2) (n - 1)
        
        loop 0 1 (n+1)


