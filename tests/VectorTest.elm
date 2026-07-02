module VectorTest exposing (..)

import Expect
import Test exposing (..)
import Vector exposing (..)

suite : Test
suite =
    describe "basic vector operations"
        [ test "add two vectors" <|
            \_ ->
                Expect.equal (add [1, 2, 3] [4, 5, 6]) [5, 7, 9]

        , test "sub two vectors" <|
            \_ ->
                Expect.equal (sub [5, 7, 9] [4, 5, 6]) [1, 2, 3]

        , test "scale multiplies every element" <|
            \_ ->
                Expect.equal (scale 2 [1, 2, 3]) [2, 4, 6]

        , test "dot computes the dot product" <|
            \_ ->
                Expect.equal (dot [1, 2, 3] [4, 5, 6]) 32

        , test "sum returns the total of a float list" <|
            \_ ->
                Expect.equal (sum [1.5, 2.5, 3.0]) 7.0

        , test "roundFloat rounds a float to nearest int" <|
            \_ ->
                Expect.equal (roundFloat 2.4) 2
        ]
