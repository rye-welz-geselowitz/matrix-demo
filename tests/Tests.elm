module Tests exposing (..)

import Expect
import Matrix
import Test exposing (..)


all : Test
all =
    describe "Matrix tests"
        [ describe "List conversions"
            [ test "toList and fromList maintain data integrity" <|
                \_ ->
                    let
                        l1 =
                            [ [ 1, 2, 3 ]
                            , [ 4, 5, 6 ]
                            , [ 7, 8, 9 ]
                            ]

                        transformed =
                            Matrix.fromList
                                l1
                                |> Maybe.withDefault Matrix.empty
                                |> Matrix.toList
                    in
                    Expect.equal l1 transformed
            ]
        , describe "Transposing a matrix"
            [ test "transposes an n x n matrix" <|
                \_ ->
                    let
                        a =
                            Matrix.fromList
                                [ [ 1, 2, 3 ]
                                , [ 4, 5, 6 ]
                                , [ 7, 8, 9 ]
                                ]
                                |> Maybe.withDefault Matrix.empty

                        transposed =
                            [ [ 1, 4, 7 ], [ 2, 5, 8 ], [ 3, 6, 9 ] ]
                    in
                    Expect.equal transposed (Matrix.transpose a |> Matrix.toList)
            , test "transposes an n x n+1 matrix" <|
                \_ ->
                    let
                        a =
                            Matrix.fromList
                                [ [ 1, 2, 3, 0 ]
                                , [ 4, 5, 6, 10 ]
                                , [ 7, 8, 9, 100 ]
                                ]
                                |> Maybe.withDefault Matrix.empty

                        transposed =
                            [ [ 1, 4, 7 ], [ 2, 5, 8 ], [ 3, 6, 9 ], [ 0, 10, 100 ] ]
                    in
                    Expect.equal transposed (Matrix.transpose a |> Matrix.toList)
            ]
        , describe "Multiplication"
            [ test "Multiplying empty matrices yields Nothing" <|
                \_ ->
                    let
                        actual =
                            Matrix.multiply Matrix.empty Matrix.empty

                        expected =
                            Nothing
                    in
                    Expect.equal expected (actual |> Maybe.map Matrix.toList)
            , test "Multiplying two n x n matrix yields Just n x n matrix" <|
                \_ ->
                    let
                        a =
                            Matrix.fromList
                                [ [ 1, 2, 3 ]
                                , [ 0, 1, 0 ]
                                , [ 2, 2, 3 ]
                                ]
                                |> Maybe.withDefault Matrix.empty

                        b =
                            Matrix.fromList
                                [ [ 1, 3, 0 ]
                                , [ 2, 2, 1 ]
                                , [ 0, 0, 3 ]
                                ]
                                |> Maybe.withDefault Matrix.empty

                        actual =
                            Matrix.multiply a b

                        expected =
                            [ [ 5, 7, 11 ], [ 2, 2, 1 ], [ 6, 10, 11 ] ]
                    in
                    Expect.equal (Just expected) (actual |> Maybe.map Matrix.toList)
            , test "Multiply m x n and n x m yields Just n x n matrix" <|
                \_ ->
                    let
                        a =
                            Matrix.fromList
                                [ [ 1, 10, 1, 1 ]
                                , [ 5, 1, 5, 1 ]
                                ]
                                |> Maybe.withDefault Matrix.empty

                        b =
                            Matrix.fromList
                                [ [ 8, 9 ]
                                , [ 1, 2 ]
                                , [ 3, 4 ]
                                , [ 0, 10 ]
                                ]
                                |> Maybe.withDefault Matrix.empty

                        actual =
                            Matrix.multiply a b

                        expected =
                            [ [ 21, 43 ], [ 56, 77 ] ]
                    in
                    Expect.equal (Just expected) (actual |> Maybe.map Matrix.toList)
            , test "Multiplying m x n matrix by n+1 x m matrix yeilds Nothing" <|
                \_ ->
                    let
                        a =
                            Matrix.fromList
                                [ [ 1, 10, 1, 1 ]
                                , [ 5, 1, 5, 1 ]
                                ]
                                |> Maybe.withDefault Matrix.empty

                        b =
                            Matrix.fromList
                                [ [ 8, 9 ]
                                , [ 1, 2 ]
                                , [ 3, 4 ]
                                , [ 0, 10 ]
                                , [ 0, 0 ]
                                ]
                                |> Maybe.withDefault Matrix.empty
                    in
                    Expect.equal Nothing (Matrix.multiply a b)
            , test "Multiplying m x n matrix by n x m+1 matrix yields Just m x m+1 matrix" <|
                \_ ->
                    let
                        a =
                            Matrix.fromList
                                [ [ 1, 10, 1, 1 ]
                                , [ 5, 1, 5, 1 ]
                                ]
                                |> Maybe.withDefault Matrix.empty

                        b =
                            Matrix.fromList
                                [ [ 8, 9, 0 ]
                                , [ 1, 2, 1 ]
                                , [ 3, 4, 0 ]
                                , [ 0, 10, 1 ]
                                ]
                                |> Maybe.withDefault Matrix.empty

                        expected =
                            [ [ 21, 43, 11 ]
                            , [ 56, 77, 2 ]
                            ]
                    in
                    Expect.equal (Just expected) (Matrix.multiply a b |> Maybe.map Matrix.toList)
            ]
        ]
