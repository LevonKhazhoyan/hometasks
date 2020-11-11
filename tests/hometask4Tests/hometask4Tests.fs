module Tests

open Expecto
open hometask4

[<Tests>]
let hometask4Tests =
    testList "hometask4 property tests" [

        let compareSorts func1 func2 x msg = Expect.sequenceEqual (func1 x) (func2 x) msg

        testProperty "bubbleSortArray is like system sort"
        <| fun (arr: array<int>) -> compareSorts Array.sort hometask4.bubbleSortArray arr "bubbleSortArray is like system sort"
        testProperty "bubbleSortList is like system sort"
        <| fun (lst: List<int>) -> compareSorts List.sort hometask4.bubbleSortList lst "bubbleSortList is like system sort"
        testProperty "quickSortList is like system sort"
        <| fun (lst: List<int>) -> compareSorts List.sort hometask4.quickSortList lst "quickSortList is like system sort"
        testProperty "quickSortListWithFilters is like system sort"
        <| fun (lst: List<int>) -> compareSorts List.sort hometask4.quickSortListWithFilters lst "quickSortListWithFilters is like system sort"
        testProperty "quickSortArray is like system sort"
        <| fun (arr: array<int>) -> compareSorts Array.sort hometask4.quickSortArray arr "quickSortArray is like system sort"

        testProperty "bubbleSortArray is like quickSortArray"
        <| fun (arr: array<int>) -> compareSorts hometask4.bubbleSortArray hometask4.quickSortArray arr "bubbleSortArray is like quickSortArray"
        testProperty "bubbleSortList is like quickSortList"
        <| fun (lst: List<int>) -> compareSorts hometask4.bubbleSortList hometask4.quickSortList lst "bubbleSortList is like quickSortList"
        testProperty "quickSortList is like quickSortListWithFilters"
        <| fun (lst: List<int>) -> compareSorts hometask4.quickSortList hometask4.quickSortListWithFilters lst "quickSortList is like quickSortListWithFilters"

        testProperty "Pack + Unpack = Original 32-bit"
        <| fun a b -> Expect.equal (a, b) (hometask4.unpacking64To32 (hometask4.packing32To64 (a, b))) "Result must be equal to original"
        testProperty "Pack + Unpack = Original 16-bit"
        <| fun a b c d -> Expect.equal (a, b, c, d) (hometask4.unpacking64To16 (hometask4.packing16To64 (a, b, c, d))) "Result must be equal to original"
    ]


[<Tests>]
let tests = testList "Special cases for sorts" [
    testList "Special cases for lists and arrays of length 1 or 0(QuickSort)" [

        testCase "returns array of length 0" <| fun _ ->
            let subject = hometask4.quickSortArray [||]
            Expect.equal subject [||] "must return list of length 0"

        testCase "returns array of length 1" <| fun _ ->
            let subject = hometask4.quickSortArray [|1|]
            Expect.equal subject [|1|] "must return list of length 1"

        testCase "returns list of length 0" <| fun _ ->
            let subject = hometask4.quickSortList []
            Expect.equal subject [] "must return list of length 0"

        testCase "returns list of length 1" <| fun _ ->
            let subject = hometask4.quickSortList [1]
            Expect.equal subject [1] "must return list of length 1"

        testCase "returns list of length 0(QSort with filters)" <| fun _ ->
            let subject = hometask4.quickSortListWithFilters []
            Expect.equal subject [] "must return list of length 0"

        testCase "returns list of length 1(QSort with filters)" <| fun _ ->
            let subject = hometask4.quickSortListWithFilters [1]
            Expect.equal subject [1] "must return list of length 1"
    ]
    testList "Special cases for lists and arrays of length 1 or 0(BubbleSort)" [

        testCase "returns array of length 0" <| fun _ ->
            let subject = hometask4.bubbleSortArray [||]
            Expect.equal subject [||] "result must be equal to 1"

        testCase "returns array of length 1" <| fun _ ->
            let subject = hometask4.bubbleSortArray [|1|]
            Expect.equal subject [|1|] "result must be equal to 1"

        testCase "returns list of length 0" <| fun _ ->
            let subject = hometask4.bubbleSortList []
            Expect.equal subject [] "must return list of length 1"

        testCase "returns list of length 1" <| fun _ ->
            let subject = hometask4.bubbleSortList [1]
            Expect.equal subject [1] "must return list of length 1"
    ]
]
