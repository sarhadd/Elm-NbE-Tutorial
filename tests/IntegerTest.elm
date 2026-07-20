module IntegerTest exposing (..)

import Test exposing (..)
import Expect
import Integer as Integer exposing (Integer)

suite : Test
suite =
    describe "Integer literal and addition"
        [ test "Integer literals type check and evaluate" <|
            \_ ->
                let
                    z = Integer.fromSafeInt 3
                    m = Integer.fromSafeInt -2
                    sum = Integer.add z m
                in
                Expect.equal (Integer.toInt sum) 1
        ]
