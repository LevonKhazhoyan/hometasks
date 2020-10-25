module TestHometask3

open Expecto
open hometasks

[<Tests>]
let tests = testList "Hometask3" [
    testList "Subtask 1: fib number rec" [

        testCase "evals 2nd fib number" <| fun _ ->
            let subject = Hometask3.fibRec 2
            Expect.equal subject 1 "result must be equal to 1"
        testCase "evals 1st fib number" <| fun _ ->
            let subject = Hometask3.fibRec 1
            Expect.equal subject 1 "result must be equal to 1"
        testCase "evals 0 fib number" <| fun _ ->
            let subject = Hometask3.fibRec 0
            Expect.equal subject 0 "result must be equal to 0"
        testCase "evals 10th fib number" <| fun _ ->
            let subject = Hometask3.fibRec 10
            Expect.equal subject 55 "result must be equal to 55"
        testCase "evals 11th fib number" <| fun _ ->
            let subject = Hometask3.fibRec 11
            Expect.equal subject 89 "result must be equal to 89"
        testCase "n is negative fib number" <| fun _ ->
            Expect.throws (fun _ -> Hometask3.fibRec -3 |> ignore) "expected n >= 0"


    ]
    testList "Subtask 2: fib number iterative" [

        testCase "evals 1st fib number" <| fun _ ->
            let subject = Hometask3.fibIter 1
            Expect.equal subject 1 "result must be equal to 1"

        testCase "evals 0 fib number" <| fun _ ->
            let subject = Hometask3.fibIter 0
            Expect.equal subject 0 "result must be equal to 0"

        testCase "evals 5th fib number" <| fun _ ->
            let subject = Hometask3.fibIter 5
            Expect.equal subject 5 "result must be equal to 5"

        testCase "evals 2nd fib number" <| fun _ ->
            let subject = Hometask3.fibIter 2
            Expect.equal subject 1 "result must be equal to 1"

        testCase "evals 10th fib number" <| fun _ ->
            let subject = Hometask3.fibIter 10
            Expect.equal subject 55 "result must be equal to 55"

        testCase "n is negative fib number" <| fun _ ->
            Expect.throws (fun _ -> Hometask3.fibIter -3 |> ignore) "expected n >= 0"
    ]
    testList "Subtask 3: fib number iterative" [

        testCase "evals 1st fib number" <| fun _ ->
            let subject = Hometask3.fibTailRec 1
            Expect.equal subject 1 "result must be equal to 1"

        testCase "evals 2nd fib number" <| fun _ ->
            let subject = Hometask3.fibTailRec 2
            Expect.equal subject 1 "result must be equal to 1"

        testCase "evals 0 fib number" <| fun _ ->
            let subject = Hometask3.fibTailRec 0
            Expect.equal subject 0 "result must be equal to 1"

        testCase "evals 6th fib number" <| fun _ ->
            let subject = Hometask3.fibTailRec 6
            Expect.equal subject 8 "result must be equal to 8"

        testCase "evals 1th fib number" <| fun _ ->
            let subject = Hometask3.fibTailRec 11
            Expect.equal subject 89 "result must be equal to 89"
        testCase "n is negative fib number" <| fun _ ->
            Expect.throws (fun _ -> Hometask3.fibTailRec -3 |> ignore) "expected n >= 0"
    ]
    testList "Subtask 4: fib number matrix multiplication" [

        testCase "evals 0 fib number" <| fun _ ->
            let subject = Hometask3.fibByMultMatrices 0
            Expect.equal subject 0 "result must be equal to 1"

        testCase "evals 1st fib number" <| fun _ ->
            let subject = Hometask3.fibByMultMatrices 1
            Expect.equal subject 1 "result must be equal to 1"

        testCase "evals 5th fib number" <| fun _ ->
            let subject = Hometask3.fibByMultMatrices 5
            Expect.equal subject 5 "result must be equal to 5"

        testCase "evals 10th fib number" <| fun _ ->
            let subject = Hometask3.fibByMultMatrices 10
            Expect.equal subject 55 "result must be equal to 55"

        testCase "evals 32th fib number" <| fun _ ->
            let subject = Hometask3.fibByMultMatrices 32
            Expect.equal subject 2178309 "result must be equal to 2178309"

        testCase "n is negative fib number" <| fun _ ->
            Expect.throws (fun _ -> Hometask3.fibByMultMatrices -3 |> ignore) "expected n >= 0"
    ]
    testList "Side functions" [

        testCase "n <= 0" <| fun _ ->
            Expect.throws (fun _ -> Hometask3.matrixMultiply (array2D [[1; 0; 0]; [0; 1; 0]; [0; 0; 1]]) (array2D [[1; 0]; [0; 1]]) |> ignore) "matrices don't match"

        testCase "creating identity matrix 3x3" <| fun _ ->
            let subject = Hometask3.createIdentityMatrix 3
            Expect.equal subject (array2D [[1; 0; 0]; [0; 1; 0]; [0; 0; 1]])"must return identity matrix 3x3"

        testCase "creating identity matrix 2x2" <| fun _ ->
            let subject = Hometask3.createIdentityMatrix 2
            Expect.equal subject (array2D [[1; 0]; [0; 1]]) "must return identity matrix 2x2"

        testCase "factorization first fib number" <| fun _ ->
            let subject = Hometask3.factorization 33
            Expect.equal subject [0; 5] "result must be equal to 1"

        testCase "factorization  4th number" <| fun _ ->
            let subject = Hometask3.factorization 4
            Expect.equal subject [2] "result must be equal to 1"

        testCase "factorization  3th number" <| fun _ ->
            let subject = Hometask3.factorization 3
            Expect.equal subject [0;1] "result must be equal to 1"

        testCase "factorization  2th number" <| fun _ ->
            let subject = Hometask3.factorization 2
            Expect.equal subject [1] "result must be equal to 1"

        testCase "factorization n <= 0" <| fun _ ->
            Expect.throws (fun _ -> Hometask3.factorization 0 |> ignore) "expected n > 0"

        testCase "raise to the 2^4 power" <| fun _ ->
            let subject = Hometask3.matrixBinPow (array2D [ [ 0; 1]; [1; 1] ]) 4 
            Expect.equal subject (array2D [[ 610; 987]; [987; 1597]]) "must return matrix with 16 fib number on [0,1] and [1,1] places"

        testCase "raise to the 2^0 power" <| fun _ ->
            let subject = Hometask3.matrixBinPow (array2D [ [ 0; 1]; [1; 1] ]) 0
            Expect.equal subject (array2D [ [0; 1]; [1;1] ]) "must return same matrix"

        testCase "raise to the 2^5 power" <| fun _ ->
            let subject = Hometask3.matrixBinPow (array2D [ [ 0; 1]; [1; 1] ]) 5
            Expect.equal subject (array2D [[1346269; 2178309]; [2178309; 3524578]]) "must return matrix with 32 fib number in [0,1] and [1,1] elements"

        testCase "Diff 4 element list" <| fun _ ->
            let subject = Hometask3.diffBetweenPairs [1;3;4;5] 
            Expect.equal subject [|2; 1; 1|] "must return 3 element list"

        testCase "Diff 2 element list" <| fun _ ->
            let subject = Hometask3.diffBetweenPairs [3;5] 
            Expect.equal subject [|2|] "must return 1 element list"

        testCase "Diff 5 element list" <| fun _ ->
            let subject = Hometask3.diffBetweenPairs [2;3;15;20;25] 
            Expect.equal subject [|1;12;5;5|] "must return 4 element list"
    ]
    testList "Subtask 5: fib number log matrix multiplication" [

        testCase "evals 18th fib number" <| fun _ ->
            let subject = Hometask3.fibMultMatricesFaster 18
            Expect.equal subject 2584 "result must be equal to 2584"

        testCase "evals 10th fib number" <| fun _ ->
            let subject = Hometask3.fibMultMatricesFaster 10
            Expect.equal subject 55 "result must be equal to 55"

        testCase "evals 9th fib number" <| fun _ ->
            let subject = Hometask3.fibMultMatricesFaster 9
            Expect.equal subject 34 "result must be equal to 34"

        testCase "evals 5th fib number" <| fun _ ->
            let subject = Hometask3.fibMultMatricesFaster 5
            Expect.equal subject 5 "result must be equal to 5"

        testCase "evals 4th fib number" <| fun _ ->
            let subject = Hometask3.fibMultMatricesFaster 4
            Expect.equal subject 3 "result must be equal to 3"

        testCase "evals 1st fib number" <| fun _ ->
            let subject = Hometask3.fibMultMatricesFaster 1
            Expect.equal subject 1 "result must be equal to 1"
            
        testCase "evals zero fib number" <| fun _ ->
            let subject = Hometask3.fibMultMatricesFaster 0
            Expect.equal subject 0 "result must be equal to 0"

        testCase "n is negative fib number" <| fun _ ->
            Expect.throws (fun _ -> Hometask3.fibByMultMatrices -3 |> ignore) "expected n >= 0"
    ]
    testList "Subtask 6: evals all fib numbers to n" [

        testCase "evals 0-3 fib number" <| fun _ ->
            let subject = Hometask3.puttingAllFibNumbersInListToN 3
            Expect.equal subject [0;1;1;2] "result must be equal to list with fib numbers from 0 to 3"

        testCase "evals 0-5 fib number" <| fun _ ->
            let subject = Hometask3.puttingAllFibNumbersInListToN 5
            Expect.equal subject [0;1;1;2;3;5] "result must be equal to list with fib numbers from 0 to 5"

        testCase "evals 0-9 fib number" <| fun _ ->
            let subject = Hometask3.puttingAllFibNumbersInListToN 9
            Expect.equal subject [0;1;1;2;3;5;8;13;21;34] "result must be equal to list with fib numbers from 0 to 9"

        testCase "evals 0-13 fib number" <| fun _ ->
            let subject = Hometask3.puttingAllFibNumbersInListToN 13
            Expect.equal subject [0;1;1;2;3;5;8;13;21;34;55;89;144;233] "result must be equal to list with fib numbers from 0 to 13"

        testCase "n is negative fib number" <| fun _ ->
            Expect.throws (fun _ -> Hometask3.puttingAllFibNumbersInListToN -3 |> ignore) "expected n >= 0"
    ]
]
[<Tests>]
let propertytests =
    testList "Property tests"
        [           
            testProperty "Subtask1 = Subtask2" 
                <| fun (n: int) -> Expect.equal (Hometask3.fibRec (abs n % 40)) (Hometask3.fibIter (abs n % 40)) "Fib1 must be equal to Fib4"
                
            testProperty "Subtask2 = Subtask3" 
                <| fun (n: int) -> Expect.equal (Hometask3.fibIter (abs n % 40)) (Hometask3.fibTailRec (abs n % 40)) "Fib2 must be equal to Fib3"

            testProperty "Subtask3 = Subtask4" 
                <| fun (n: int) -> Expect.equal (Hometask3.fibTailRec (abs n % 40)) (Hometask3.fibByMultMatrices (abs n % 40)) "Fib3 must be equal to Fib4"

            testProperty "Subtask4 = Subtask5" 
                <| fun (n: int) -> Expect.equal (Hometask3.fibByMultMatrices (abs n % 40)) (Hometask3.fibMultMatricesFaster (abs n % 40)) "Fib4 must be equal to Fib5"
        ]
