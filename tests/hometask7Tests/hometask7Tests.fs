module TestsHW7

open MyList
open MyTree
open Expecto

let genRandomList count =
    let rnd = System.Random()
    let Count = if count = 0 then rnd.Next(1, 1000) else abs count
    List.init Count (fun _ -> rnd.Next(1000))

[<Tests>]
let propTestsForMyList =
    testList "Property tests for myList"
        [
            testProperty "Fold test" <| fun x ->
                let lst = genRandomList x |> List.map (fun y -> string y)
                let str = List.fold (fun w z -> w + z) "" lst
                Expect.sequenceEqual (lst |> listToMyList |> MyList.fold (fun w z -> w + z) "") str "Expected Folded List"
            testProperty "Length test" <| fun x ->
                let lst = genRandomList x
                Expect.equal (List.length lst) (lst |> listToMyList |> len) "Expected Length of list"
            testProperty "Sort test" <| fun x ->
                let lst = genRandomList x
                Expect.equal (List.sort lst |> listToMyList) (bubbleSort (lst |> listToMyList)) "Expected Sorted list"
            testProperty "Iter test" <| fun x ->
                let lst = genRandomList x
                let a = Array.zeroCreate lst.Length
                let b = Array.ofList lst
                let mutable c = 0
                iter (fun x ->
                    a.[c] <- x
                    c <- c + 1) (listToMyList lst)
                Expect.sequenceEqual a b "Expected Iter"
            testProperty "Map test" <| fun x ->
                let lst = genRandomList x
                Expect.equal (List.map (fun i -> i * 7) lst |> listToMyList) (map (fun i -> i * 7) (listToMyList lst)) "Expected Mapped list"
            testProperty "Concatenate test" <| fun (x, y) ->
                let lst1 = genRandomList x
                let lst2 = genRandomList y
                Expect.equal (lst1 @ lst2 |> listToMyList) (concat (lst1 |> listToMyList) (lst2 |> listToMyList)) "Expected Concatenated lists"
        ]

[<Tests>]
let MyListTests =
    testList "MyListTests"
        [
            testCase "Length 2" <| fun _ ->
                Expect.equal 2 (len (Cons (5, One 4))) "Expected 2"
            testCase "Length 3" <| fun _ ->
                Expect.equal 3 (len (Cons (12, Cons (5, One 4)))) "Expected 3"

            testCase "map" <| fun _ ->
                Expect.equal (Cons (6, One 5)) (map ((+) 1) (Cons (5, One 4))) "Expected [6,5]"
            testCase "map second" <| fun _ ->
                Expect.equal (Cons (13, Cons (6, One 5))) (map ((+) 2) (Cons (11, Cons (4, One 3)))) "Expected [13,6,5]"

            testCase "System list to MyList" <| fun _ ->
                Expect.equal (Cons (6, One 5)) (listToMyList [6; 5]) "Expected [6; 5]"
            testCase "System list to MyList second" <| fun _ ->
                Expect.equal (Cons (13, Cons (6, One 5))) (listToMyList [13; 6; 5]) "Expected [13; 6; 5]"

            testCase "concat myLists" <| fun _ ->
                Expect.equal (Cons (6, One 5)) (concat (One 6) (One 5)) "Expected [6,5]"
            testCase "concat myLists second" <| fun _ ->
                Expect.equal (Cons (13, Cons (6, One 5))) (concat (Cons (13, One 6)) (One 5)) "Expected[13,6,5]"

            testCase "sort myList" <| fun _ ->
                Expect.equal (Cons (5, One 6)) (bubbleSort (Cons (6, One 5))) "Expected [5,6]"
            testCase "sort myList second" <| fun _ ->
                Expect.equal (Cons (5, Cons (6, One 14))) (bubbleSort (Cons (14, Cons (6, One 5)))) "Expected [5,6,13]"
        ]
[<Tests>]
let MyStringTests =
    testList "MyStringTests"
        [
            testCase "System string to MyString Re" <| fun _ ->
                Expect.equal (Cons ('R', One 'e')) (stringToMyString "Re") ""
            testCase "System string to MyString TE" <| fun _ ->
                Expect.equal (Cons ('T', Cons (' ', One 'E'))) (stringToMyString "T E") ""

            testCase "Concatenate myStrings Re" <| fun _ ->
                Expect.equal (Cons ('R', One 'e')) (concat (One 'R') (One 'e')) ""
            testCase "Concatenate myStrings TE" <| fun _ ->
                Expect.equal (Cons ('T', Cons (' ', One 'E'))) (concat (Cons('T', One ' ')) (One 'E')) ""
        ]

[<Tests>]
let MyTreeTests =
    testList "MyTreeTests"
        [
            testCase "maxElem myTree" <| fun _ ->
                Expect.equal (max (Node ((29), Cons ((Leaf (1337)), One (Leaf (29)))))) 1337 "Expected 1337"
            testCase "maxElem myTree second" <| fun _ ->
                Expect.equal (max (Node ((-9), Cons ((Leaf (4)), One (Leaf (-2)))))) 4 "Expected 4"

            testCase "average myTree" <| fun _ ->
                Expect.equal (average (Node ((14), Cons ((Leaf (16)), One (Leaf (18)))))) 16.0 "Expected 16"
            testCase "average myTree second" <| fun _ ->
                Expect.equal (average (Node ((10), Cons ((Leaf (12)), One (Leaf (11)))))) 11.0 "Expected 11"

        ]
