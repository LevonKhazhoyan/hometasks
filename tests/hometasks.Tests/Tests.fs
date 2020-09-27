module Tests


open Expecto
open hometasks

[<Tests>]
let tests = testList "all" [
    testList "Subtask 1: eval polynome" [
        testCase "-1 to an odd power is -1, and an even power to 1" <| fun _ ->
            let subject = Hometask_2.Subtask_1 -1.0
            Expect.equal subject 1.0 "result must be equal 1"
    ]

    testList "Subtask 2: eval polynome effeciently" [
        testCase "-1 to an odd power is -1, and an even power to 1" <| fun _ ->
            let subject = Hometask_2.Subtask_2 -1.0
            Expect.equal subject 1.0 "result must be equal 1"
    ]

    testList "Subtask 3: find indices" [
        testCase "indices of all array numbers that lower than 5" <| fun _ ->
            let subject = Hometask_2.Subtask_3 [| -2; 0; -3; 15 |] 5
            Expect.equal subject [| 0; 1; 2 |] "should return indices of all array numbers that lower than 5 "
        testCase "all numbers are higher" <| fun _ ->
            let subject = Hometask_2.Subtask_3 [| -2; -1; -3; -5 |] -100
            Expect.equal subject [||] "should return empty array"
    ]

    testList "Subtask 4: find indices" [
        testCase "among positive numbers and zero there can be no negative" <| fun _ ->
            let subject = Hometask_2.Subtask_4 [| -9; -6; 3; 6 |] 0 7
            Expect.equal subject [| 0; 1 |] "should write indices of negative array elements"


        testCase "among negative numbers and zero there can be no positive" <| fun _ ->
            let subject = Hometask_2.Subtask_4 [| -9; -6; 3; 6 |] -10 0
            Expect.equal subject [| 2; 3|] "should write indices of positive array elements"

    ]

    testList "Subtask 5: swap array elements" [
        testCase "should swap 0 and 1" <| fun _ ->
            let subject = Hometask_2.Subtask_5 [| 0; 1|]
            Expect.equal subject [| 1; 0|]  "should swap"

        testCase "should swap big numbers without overflow" <| fun _ ->
            let subject = Hometask_2.Subtask_5 [| 2147483647; 2147483640|]
            Expect.equal subject [| 2147483640; 2147483647|]  "should swap"
            
        testCase "should swap big negative numbers without overflow" <| fun _ ->
            let subject = Hometask_2.Subtask_5 [| -2147483647; -2147483640|]
            Expect.equal subject [| -2147483640; -2147483647|]  "should swap"

        testCase "should fail when array length is less than 2" <| fun _ ->
            let subject = Hometask_2.Subtask_5 [| 0 |]
            Expect.equal subject [||]  "should return empty array"

        testCase "should fail when array length is greater than 2" <| fun _ ->
            let subject = Hometask_2.Subtask_5 [| 0; 1; 2 |]
            Expect.equal subject [||]  "should return empty array"
    ]

    testList "Subtask 6: swap array elements" [
        testCase "should swap big numbers without overflow" <| fun _ ->
            let subject = Hometask_2.Subtask_6 [| 3; 4;2147483647; 2147483640; 10; 42 |] 2 3
            Expect.equal subject [| 3; 4; 2147483640; 2147483647; 10; 42 |] "should swap"

        testCase "should swap big negative numbers without overflow" <| fun _ ->
            let subject = Hometask_2.Subtask_6 [| 3; 4; -2147483647; -2147483640; 10; 42 |] 2 3
            Expect.equal subject [| 3; 4; -2147483640; -2147483647; 10; 42 |] "should swap"

        testCase "should fail when an index is out of bounds" <| fun _ ->
            let subject = Hometask_2.Subtask_6 [| 3; 4; 5; 10; 10; 42 |] 2 73
            Expect.equal subject [||] "should return empty list"

        testCase "should fail when an index is negative" <| fun _ ->
            let subject = Hometask_2.Subtask_6 [| 3; 4; 20; 40; 10; 42 |] -2 3
            Expect.equal subject [||] "should return empty list"

        testCase "should return same list when indices are equal" <| fun _ ->
            let subject = Hometask_2.Subtask_6 [| 3; 4; 10; 30; 10; 42 |] 2 2
            Expect.equal subject [| 3; 4; 10; 30; 10; 42 |] "should return same list"
    ]

 ]




