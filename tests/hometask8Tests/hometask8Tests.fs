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

let ringForTests = SemiRing(new SemiRing<int>(new Monoid<int>((+), 0), (*), 1))


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

    testCase "sum" <| fun _ ->
        let mtx1 = SparseMatrix(3,3,[Coordinate(0, 0, 3); Coordinate(1, 1, -5)])
        let mtx2 = SparseMatrix(3,3,[Coordinate(0, 1, 2); Coordinate(1, 1, 5)])
        Expect.equal

            (SparseMatrix.sortListOfCoords
            (SparseMatrix.coordinatesToListOfTriples
            (QuadTreeMtx.convert
            (QuadTreeMtx (3, 3, ((QuadTree.sum (QuadTreeMtx.create (mtx1)).Tree) (QuadTreeMtx.create (mtx2)).Tree) ringForTests)))))

            [(0,0,3);(0,1,2)] ""
    ]
