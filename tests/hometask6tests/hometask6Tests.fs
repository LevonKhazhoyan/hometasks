module Tests

    open Expecto
    open hometask6

    [<Tests>]
    let boolMatricesMultiply =
        testList "Matrix multiply"
            [
                testCase "Multiply 3x3 and 3x3 matrices with all zeroes" <| fun _ ->
                    let matrix1 = Matrix (3, 3, [])
                    let matrix2 = Matrix (3, 3, [])
                    let subject = multiplyBoolMatrices matrix1 matrix2
                    Expect.equal subject (Matrix (3, 3, [])) ""

                testCase "Multiply 1x1 and 1x1 matrices" <| fun _ ->
                    let matrix1 = Matrix (1, 1, [Coordinate(0<_row>, 0<_column>)])
                    let matrix2 = Matrix (1, 1, [Coordinate(0<_row>, 0<_column>)])
                    let subject = multiplyBoolMatrices matrix1 matrix2
                    Expect.equal subject (Matrix (1, 1, [Coordinate(0<_row>, 0<_column>)])) ""

                testCase "Multiply 2x3 and 3x2 matrices" <| fun _ ->
                    let matrix1 = Matrix (2, 3, [Coordinate(0<_row>, 0<_column>); Coordinate(2<_row>, 1<_column>)])
                    let matrix2 = Matrix (3, 2, [Coordinate(0<_row>, 2<_column>); Coordinate(2<_row>, 1<_column>)])
                    let subject = multiplyBoolMatrices matrix1 matrix2
                    Expect.equal subject (Matrix (2, 2, [Coordinate(0<_row>, 2<_column>)])) ""

                testCase "Multiply 3x3 and 3x3 matrices" <| fun _ ->
                    let matrix1 = Matrix (3, 3, [Coordinate(0<_row>, 1<_column>)])
                    let matrix2 = Matrix (3, 3, [Coordinate(1<_row>, 0<_column>)])
                    let subject = multiplyBoolMatrices matrix1 matrix2
                    Expect.equal subject (Matrix (3, 3, [Coordinate(0<_row>, 0<_column>)])) ""


                testCase "Multiply 3x4 and 4x3 matrices" <| fun _ ->
                    let matrix1 = Matrix (3, 4, [Coordinate(0<_row>, 1<_column>); Coordinate(2<_row>, 2<_column>)])
                    let matrix2 = Matrix (4, 3, [Coordinate(1<_row>, 0<_column>)])
                    let subject = multiplyBoolMatrices matrix1 matrix2
                    Expect.equal subject (Matrix (3, 3, [Coordinate(0<_row>, 0<_column>)])) ""

                testCase "Multiply 4x5 and 5x4 matrices" <| fun _ ->
                    let matrix1 = Matrix (4, 5, [Coordinate(1<_row>, 0<_column>); Coordinate(5<_row>, 2<_column>)])
                    let matrix2 = Matrix (5, 4, [Coordinate(0<_row>, 3<_column>)])
                    let subject = multiplyBoolMatrices matrix1 matrix2
                    Expect.equal subject (Matrix (4, 4, [Coordinate(1<_row>, 3<_column>)])) ""

                testCase "Multiply 3x5 and 5x3 matrices" <| fun _ ->
                    let matrix1 = Matrix (3, 5, [Coordinate(0<_row>, 1<_column>)])
                    let matrix2 = Matrix (5, 3, [Coordinate(4<_row>, 1<_column>)])
                    let subject = multiplyBoolMatrices matrix1 matrix2
                    Expect.equal subject (Matrix (3, 3, [])) ""

                testCase "Multiply 4x4 and 4x4 matrices" <| fun _ ->
                    let matrix1 = Matrix (4, 4, [Coordinate(0<_row>, 1<_column>)])
                    let matrix2 = Matrix (4, 4, [Coordinate(1<_row>, 1<_column>); Coordinate(3<_row>, 2<_column>)])
                    let subject = multiplyBoolMatrices matrix1 matrix2
                    Expect.equal subject (Matrix (4, 4, [Coordinate(0<_row>, 1<_column>)])) ""

                testCase "Multiply 5x5 and 5x5 matrices" <| fun _ ->
                    let matrix1 = Matrix (5, 5, [Coordinate(0<_row>, 0<_column>)])
                    let matrix2 = Matrix (5, 5, [Coordinate(0<_row>, 0<_column>); Coordinate(1<_row>, 1<_column>)])
                    let subject = multiplyBoolMatrices matrix1 matrix2
                    Expect.equal subject (Matrix (5, 5, [Coordinate(0<_row>, 0<_column>)])) ""

                testCase "Multiply 6x7 and 7x6 matrices" <| fun _ ->
                    let matrix1 = Matrix (6, 7, [Coordinate(0<_row>, 0<_column>); Coordinate(4<_row>, 4<_column>)])
                    let matrix2 = Matrix (7, 6, [Coordinate(0<_row>, 5<_column>); Coordinate(3<_row>, 1<_column>)])
                    let subject = multiplyBoolMatrices matrix1 matrix2
                    Expect.equal subject (Matrix (6, 6, [Coordinate(0<_row>, 5<_column>)])) ""
            ]
