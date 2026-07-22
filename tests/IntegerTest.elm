module IntegerTest exposing (..)

import Test exposing (..)
import Expect
import Integer as Integer exposing (Integer)

suite : Test
suite =
    describe "Integer literal and arithmetic"
        [ test "Integer literals type check and evaluate" <|
            \_ ->
                let
                    z = Integer.fromSafeInt 3
                    m = Integer.fromSafeInt -2
                    sum = Integer.add z m
                in
                Expect.equal (Integer.toInt sum) 1

        , describe "sub"
            [ test "sub 3 (-2) == 5" <|
                \_ ->
                    let
                        a = Integer.fromSafeInt 3
                        b = Integer.fromSafeInt -2
                    in
                    Expect.equal (Integer.toInt (Integer.sub a b)) 5

            , test "sub (-2) 3 == -5" <|
                \_ ->
                    let
                        a = Integer.fromSafeInt -2
                        b = Integer.fromSafeInt 3
                    in
                    Expect.equal (Integer.toInt (Integer.sub a b)) -5
            ]

        , describe "mul"
            [ test "mul 3 (-2) == -6" <|
                \_ ->
                    let
                        a = Integer.fromSafeInt 3
                        b = Integer.fromSafeInt -2
                    in
                    Expect.equal (Integer.toInt (Integer.mul a b)) -6

            , test "mul (-3) (-2) == 6" <|
                \_ ->
                    let
                        a = Integer.fromSafeInt -3
                        b = Integer.fromSafeInt -2
                    in
                    Expect.equal (Integer.toInt (Integer.mul a b)) 6
            ]
        ]
