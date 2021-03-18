module Tests

open Expecto
open SparseMatrix
open QuadTree
open System
open AlgebraicStructure

let randomIntSparseMatrix rows cols =
    let xs = Array2D.init rows cols (fun _ _ -> Random().Next(0,2))
    SparseMatrix(rows,cols,[for i in 0..(rows-1) do
                                for j in 0..(cols-1) do
                                    if xs.[i, j] = 1 then Coordinate(i,j,Random().Next(1,100))])

let monoidForTests = new Monoid<int>((+), 0)
let ringForTests = new SemiRing<int>(new Monoid<int>((+), 0), (*))

[<Tests>]
let tests =
  testList "samples" [

    testProperty "creating quad tree from sparse matrix and back"
    <| fun (x1: int) (x2: int) ->
      let mtx = randomIntSparseMatrix (abs(x1 % 64)) (abs(x2 % 64))
      Expect.equal

          (SparseMatrix.sortListOfCoords
          (SparseMatrix.coordinatesToListOfTriples
          (QuadTreeMtx.convert
          (QuadTreeMtx.create (mtx)))))

          (SparseMatrix.coordinatesToListOfTriples mtx) ""

    testCase "sum 4х4" <| fun _ ->
        let mtx1 = SparseMatrix(3, 3, [Coordinate(0, 0, 3); Coordinate(1, 1, -5)])
        let mtx2 = SparseMatrix(3, 3, [Coordinate(0, 1, 2); Coordinate(1, 1, 5)])
        Expect.equal

            (SparseMatrix.sortListOfCoords
            (SparseMatrix.coordinatesToListOfTriples
            (QuadTreeMtx.convert
            ((QuadTreeMtx.sum (QuadTreeMtx.create (mtx1)) (QuadTreeMtx.create (mtx2)) monoidForTests)))))

            [(0,0,3);(0,1,2)] ""

    testCase "None + _" <| fun _ ->
        let mtx1 = SparseMatrix(4, 4, [])
        let mtx2 = SparseMatrix(4, 4, [Coordinate(0, 2, 3465); Coordinate(0, 3, 4851);
                                      Coordinate(1, 2, 6853); Coordinate(1, 3, 2079); Coordinate(3, 1, 2484)])
        Expect.equal

            (SparseMatrix.sortListOfCoords
            (SparseMatrix.coordinatesToListOfTriples
            (QuadTreeMtx.convert
            ((QuadTreeMtx.sum (QuadTreeMtx.create (mtx1)) (QuadTreeMtx.create (mtx2)) monoidForTests)))))

            [(0, 2, 3465); (0, 3, 4851); (1, 2, 6853); (1, 3, 2079); (3, 1, 2484)]  ""
            
    testCase "tensor multiply" <| fun _ ->
        let mtx1 = SparseMatrix(2,2,[Coordinate(0, 1, 77); Coordinate(1, 0, 92)])
        let mtx2 = SparseMatrix(2,2,[Coordinate(0, 0, 45); Coordinate(0, 1, 63); Coordinate(1, 0, 89); Coordinate(1, 1, 27)])
        Expect.equal

            (SparseMatrix.sortListOfCoords
            (SparseMatrix.coordinatesToListOfTriples
            (QuadTreeMtx.convert (QuadTreeMtx.tensorMultiply (QuadTreeMtx.create (mtx1)) (QuadTreeMtx.create (mtx2)) ringForTests))))

            [(0, 2, 3465); (0, 3, 4851); (1, 2, 6853); (1, 3, 2079); (2, 0, 4140);
            (2, 1, 5796); (3, 0, 8188); (3, 1, 2484)] ""

    testCase "tensor multiply by None" <| fun _ ->
        let mtx1 = SparseMatrix(2,2,[Coordinate(0, 1, 77); Coordinate(1, 0, 92)])
        let mtx2 = SparseMatrix(2,2,[])
        Expect.equal

            (SparseMatrix.sortListOfCoords
            (SparseMatrix.coordinatesToListOfTriples
            (QuadTreeMtx.convert 
            (QuadTreeMtx.tensorMultiply (QuadTreeMtx.create (mtx1)) (QuadTreeMtx.create (mtx2)) ringForTests))))

            [] ""
    ]
