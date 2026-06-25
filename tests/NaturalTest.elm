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
            , test "toInt ten == 10" <|
                \_ -> Expect.equal (toInt ten) 10
            ]
        , describe "add"
            [ test "add zero three == three" <|
                \_ -> Expect.equal (add zero three) three
            , test "add two three == five" <|
                \_ -> Expect.equal (add two three) five
            , test "add two three == add three two" <|
                \_ -> Expect.equal (add two three) (add three two)
            ]
        , describe "sub"
            [ test "sub five two == three" <|
                \_ -> Expect.equal (sub five two) three
            , test "sub three three == zero" <|
                \_ -> Expect.equal (sub three three) zero
            , test "sub two five == zero (saturating)" <|
                \_ -> Expect.equal (sub two five) zero
            , test "sub zero three == zero (saturating)" <|
                \_ -> Expect.equal (sub zero three) zero
            ]
        , describe "mul"
            [ test "mul zero five == zero" <|
                \_ -> Expect.equal (mul zero five) zero
            , test "mul one five == five" <|
                \_ -> Expect.equal (mul one five) five
            , test "mul two three == six" <|
                \_ -> Expect.equal (mul two three) six
            , test "mul three three == nine" <|
                \_ -> Expect.equal (mul three three) nine
            ]
        , describe "exp"
            [ test "exp two zero == one" <|
                \_ -> Expect.equal (exp two zero) one
            , test "exp zero zero == one" <|
                \_ -> Expect.equal (exp zero zero) one
            , test "exp two three == eight" <|
                \_ -> Expect.equal (exp two three) eight
            , test "exp three two == nine" <|
                \_ -> Expect.equal (exp three two) nine
            ]
        , describe "divModBy"
            [ test "divModBy zero five == Nothing" <|
                \_ -> Expect.equal (divModBy zero five) Nothing
            , test "divModBy two ten == Just (five, zero)" <|
                \_ -> Expect.equal (divModBy two ten) (Just ( five, zero ))
            , test "divModBy three ten == Just (three, one)" <|
                \_ -> Expect.equal (divModBy three ten) (Just ( three, one ))
            , test "divModBy five two == Just (zero, two)" <|
                \_ -> Expect.equal (divModBy five two) (Just ( zero, two ))
            ]
        , describe "compare"
            [ test "compare two five == LT" <|
                \_ -> Expect.equal (Natural.compare two five) LT
            , test "compare five five == EQ" <|
                \_ -> Expect.equal (Natural.compare five five) EQ
            , test "compare five two == GT" <|
                \_ -> Expect.equal (Natural.compare five two) GT
            ]
        , describe "comparison predicates"
            [ test "two isLessThan five" <|
                \_ -> Expect.equal (two |> isLessThan five) True
            , test "five isLessThan two == False" <|
                \_ -> Expect.equal (five |> isLessThan two) False
            , test "two isLessThanOrEqual two" <|
                \_ -> Expect.equal (two |> isLessThanOrEqual two) True
            , test "five isGreaterThan two" <|
                \_ -> Expect.equal (five |> isGreaterThan two) True
            , test "two isGreaterThanOrEqual two" <|
                \_ -> Expect.equal (two |> isGreaterThanOrEqual two) True
            , test "max two five == five" <|
                \_ -> Expect.equal (Natural.max two five) five
            , test "min two five == two" <|
                \_ -> Expect.equal (Natural.min two five) two
            ]
        , describe "predicates"
            [ test "isZero zero == True" <|
                \_ -> Expect.equal (isZero zero) True
            , test "isZero one == False" <|
                \_ -> Expect.equal (isZero one) False
            , test "isOne one == True" <|
                \_ -> Expect.equal (isOne one) True
            , test "isOne two == False" <|
                \_ -> Expect.equal (isOne two) False
            , test "isPositive five == True" <|
                \_ -> Expect.equal (isPositive five) True
            , test "isPositive zero == False" <|
                \_ -> Expect.equal (isPositive zero) False
            , test "isEven zero == True" <|
                \_ -> Expect.equal (isEven zero) True
            , test "isEven two == True" <|
                \_ -> Expect.equal (isEven two) True
            , test "isEven three == False" <|
                \_ -> Expect.equal (isEven three) False
            , test "isOdd one == True" <|
                \_ -> Expect.equal (isOdd one) True
            , test "isOdd two == False" <|
                \_ -> Expect.equal (isOdd two) False
            ]
        , describe "toString"
            [ test "toString zero == \"0\"" <|
                \_ -> Expect.equal (toString zero) "0"
            , test "toString ten == \"10\"" <|
                \_ -> Expect.equal (toString ten) "10"
            ]
        ]
