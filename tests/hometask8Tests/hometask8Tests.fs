module Tests

open Expecto
open SparseMatrix
open QuadTree
open System
open AlgebraicStructure
open System.Collections.Generic

let randomIntSparseMatrix rows cols =
    let xs = Array2D.init rows cols (fun _ _ -> Random().Next(0,2))
    SparseMatrix(rows,cols,[for i in 0..(rows-1) do
                                for j in 0..(cols-1) do
                                    if xs.[i, j] = 1 then Coordinate(i,j,Random().Next(1,100))])

let array2DFromSparseMatrix (xs: SparseMatrix<int>) =
    let res = Array2D.zeroCreate xs.NumOfRows xs.NumOfCols
    for x in xs.Coordinates do
        res.[x.Row, x.Col] <- x.Value
    res

let sumSparse (x1: SparseMatrix<int>) (y1: SparseMatrix<int>) =
    let x = array2DFromSparseMatrix x1
    let y = array2DFromSparseMatrix y1
    let rowsX, rowsY = Array2D.length1 x, Array2D.length1 y
    let colsX, colsY = Array2D.length2 x, Array2D.length2 y
    let res = Array2D.create rowsX colsY 0
    if rowsX = rowsY && colsX = colsY then 
        for i in 0 .. (rowsX - 1) do
            for j in 0 .. (colsX - 1) do
                res.[i, j] <- x.[i, j] + y.[i, j]
        res
    else failwith "can't sum matrices of diff sizes"
    
let fromArray2DToSparse (xs: int[,]) =
    let rows, cols = xs.[0, *].Length, xs.[*, 0].Length
    SparseMatrix(rows, cols, [for i in 0..(rows - 1) do
                                for j in 0..(cols - 1) do
                                    if xs.[i, j] <> 0 then Coordinate(i, j, xs.[i, j])])

let nearestPowerOfTwo (x: int, y: int) =
    let mutable power = 1
    let maximum = max x y
    while maximum > power do
        power <- power * 2
    power

let tensor (x1: SparseMatrix<int>) (y1: SparseMatrix<int>) =
    let x = array2DFromSparseMatrix x1
    let y = array2DFromSparseMatrix y1
    let rowsX, rowsY = x.[0, *].Length, y.[0, *].Length
    let colsX, colsY = x.[*, 0].Length, y.[*, 0].Length
    let rows, cols = rowsX * rowsY, colsX * colsY
    let res = Array2D.zeroCreate rows cols
    for i in 0 .. (rows - 1) do
        for j in 0 .. (cols - 1) do
            res.[i, j] <- x.[i / rowsY, j / colsY] * y.[i % rowsY, j % rowsY]
    res

let monoidForTests = new Monoid<int>((+), 0)
let ringForTests = new SemiRing<int>(new Monoid<int>((+), 0), (*))

[<Tests>]   
let tests =
  testList "quad tree funs test" [

    testProperty "creating quad tree from sparse matrix and back"
    <| fun (x1: int) (x2: int) ->

      let mtx = randomIntSparseMatrix (abs(x1 % 64)) (abs(x2 % 64))

      Expect.equal

          (SparseMatrix.sortListOfCoords
          (SparseMatrix.coordinatesToListOfTriples
          (QuadTreeMtx.convert
          (QuadTreeMtx.create (mtx)))))

          (SparseMatrix.coordinatesToListOfTriples mtx) ""

    testProperty "tensor multiply"
    <| fun (x1: int) (x2: int) (x3: int) (x4: int) ->

        let rows1 = abs(x1 % 10) 
        let rows2 = abs(x3 % 10) 
        let cols1 = abs(x2 % 10) 
        let cols2 = abs(x4 % 10) 

        let mtx1 = randomIntSparseMatrix (nearestPowerOfTwo (rows1, cols1)) (nearestPowerOfTwo (rows1, cols1))
        let mtx2 = randomIntSparseMatrix (nearestPowerOfTwo (rows2, cols2)) (nearestPowerOfTwo (rows2, cols2))

        Expect.equal

            
            (tensor mtx1 mtx2)

            (array2DFromSparseMatrix
            (QuadTreeMtx.convert
            (QuadTreeMtx.tensorMultiply (QuadTreeMtx.create (mtx1)) (QuadTreeMtx.create (mtx2)) ringForTests))) ""

    testProperty "sum"
    <| fun (x1: int) (x2: int) ->

        let mtx1 = randomIntSparseMatrix (abs(x1 % 10)) (abs(x2 % 10))
        let mtx2 = randomIntSparseMatrix (abs(x1 % 10)) (abs(x2 % 10))

        Expect.equal

            
            (sumSparse mtx1 mtx2)

            (array2DFromSparseMatrix
            (QuadTreeMtx.convert
            (QuadTreeMtx.sum (QuadTreeMtx.create (mtx1)) (QuadTreeMtx.create (mtx2)) monoidForTests))) ""


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
            
    testCase "tensor multiply case" <| fun _ ->
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
