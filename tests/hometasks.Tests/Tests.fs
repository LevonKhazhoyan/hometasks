module Tests


open Expecto
open hometasks

[<Tests>]
let tests = testList "all" [
    testList "Subtask 1: eval polynome" [
        testCase "-1 to an odd power is -1, and an even power to 1" <| fun _ ->
            let subject = Hometask2.subtask1 -1.0
            Expect.equal subject 1.0 "result must be equal 1"
    ]

    testList "Subtask 2: eval polynome effeciently" [
        testCase "-1 to an odd power is -1, and an even power to 1" <| fun _ ->
            let subject = Hometask2.subtask2 -1.0
            Expect.equal subject 1.0 "result must be equal 1"
    ]

    testList "Subtask 3: find indices" [
        testCase "indices of all array numbers that lower than 5" <| fun _ ->
            let subject = Hometask2.subtask3 [| -2; 0; -3; 15 |] 5
            Expect.equal subject [| 0; 1; 2 |] "should return indices of all array numbers that lower than 5 "
        testCase "all numbers are higher" <| fun _ ->
            let subject = Hometask2.subtask3 [| -9; -6; 3; 6 |] -722
            Expect.equal subject [||] "numbers is too big"
    ]

    testList "Subtask 4: find indices" [
        testCase "among positive numbers and zero there can be no negative" <| fun _ ->
            let subject = Hometask2.subtask4 [| -9; -6; 3; 6 |] 0 7
            Expect.equal subject [| 0; 1 |] "should write indices of negative array elements"


        testCase "among negative numbers and zero there can be no positive" <| fun _ ->
            let subject = Hometask2.subtask4 [| -9; -6; 3; 6 |] -10 0
            Expect.equal subject [| 2; 3|] "should write indices of positive array elements"

    ]

    testList "Subtask 5: swap array elements" [
        testCase "should swap 0 and 1" <| fun _ ->
            let subject = Hometask2.subtask5 [| 0; 1|]
            Expect.equal subject [| 1; 0|]  "should swap"

        testCase "should swap big numbers without overflow" <| fun _ ->
            let subject = Hometask2.subtask5 [| 2147483647; 2147483640|]
            Expect.equal subject [| 2147483640; 2147483647|]  "should swap"
            
        testCase "should swap big negative numbers without overflow" <| fun _ ->
            let subject = Hometask2.subtask5 [| -2147483647; -2147483640|]
            Expect.equal subject [| -2147483640; -2147483647|]  "should swap"

        testCase "should fail when array length is less than 2" <| fun _ ->
            Expect.throws (fun _ -> Hometask2.subtask5 [| 0 |] |> ignore) "expected array of size 2"

        testCase "should fail when array length is greater than 2" <| fun _ ->
            Expect.throws (fun _ -> Hometask2.subtask5 [| 0; 1; 2 |] |> ignore) "expected array of size 2"
    ]

    testList "Subtask 6: swap array elements" [
        testCase "should swap big numbers without overflow" <| fun _ ->
            let subject = Hometask2.subtask6 [| 3; 4;2147483647; 2147483640; 10; 42 |] 2 3
            Expect.equal subject [| 3; 4; 2147483640; 2147483647; 10; 42 |] "should swap"

        testCase "should swap big negative numbers without overflow" <| fun _ ->
            let subject = Hometask2.subtask6 [| 3; 4; -2147483647; -2147483640; 10; 42 |] 2 3
            Expect.equal subject [| 3; 4; -2147483640; -2147483647; 10; 42 |] "should swap"

        testCase "should fail when an index is out of bounds" <| fun _ ->
            Expect.throws (fun _ -> Hometask2.subtask6 [| 3; 4; 5; 10; 10; 42 |] 2 73  |> ignore) "index is out of bounds"

        testCase "should fail when an index is negative" <| fun _ ->
            Expect.throws (fun _ -> Hometask2.subtask6 [|0; 3; 15|] -4 2  |> ignore) "you entered a negative index"

        testCase "should return same list when indices are equal" <| fun _ ->
            let subject = Hometask2.subtask6 [| 3; 4; 10; 30; 10; 42 |] 2 2
            Expect.equal subject [| 3; 4; 10; 30; 10; 42 |] "should return same list"
    ]

 ]




