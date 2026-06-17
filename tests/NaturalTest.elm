module NaturalTest exposing (..)

import Expect
import Test exposing (..)
import Natural exposing (..)


suite : Test
suite =
    describe "Natural (Peano)"
        [ describe "fromInt / toInt"
            [ test "fromInt -1 == Nothing" <|
                \_ -> Expect.equal (fromInt -1) Nothing
            , test "fromInt 0 == Just zero" <|
                \_ -> Expect.equal (fromInt 0) (Just zero)
            , test "toInt zero == 0" <|
                \_ -> Expect.equal (toInt zero) 0
            , test "five == fromSafeInt 5" <|
                \_ -> Expect.equal five (fromSafeInt 5)
            , test "toInt (fromSafeInt 3) == 3" <|
                \_ -> Expect.equal (toInt (fromSafeInt 3)) 3
            ]
        , describe "add"
            [ test "add zero three == three" <|
                \_ -> Expect.equal (add zero three) three
            , test "add two three == five" <|
                \_ -> Expect.equal (add two three) five
            , test "add two three == add three two" <|
                \_ -> Expect.equal (add two three) (add three two)
            ]
        , describe "compare"
            [ test "compare two five == LT" <|
                \_ -> Expect.equal (Natural.compare two five) LT
            , test "compare five five == EQ" <|
                \_ -> Expect.equal (Natural.compare five five) EQ
            , test "compare five two == GT" <|
                \_ -> Expect.equal (Natural.compare five two) GT
            ]
        ]
