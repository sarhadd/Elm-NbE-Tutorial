module PeanoNat exposing
    ( Natural(..)
    , zero, one, two, three, four, five, six, seven, eight, nine, ten
    , fromInt, fromSafeInt
    , compare, isLessThan, isLessThanOrEqual, isGreaterThan, isGreaterThanOrEqual, max, min
    , isZero, isOne, isPositive, isEven, isOdd
    , add, sub, mul, divModBy, divBy, modBy, exp
    , toInt, toString
    )

{-| Natural numbers in Peano (unary) form — kept for comparison experiment.

    type Natural = Zero | Add1 Natural

These are NOT used in the main NbE project.
The main project uses dwayne/elm-natural (binary big-integer, O(1) arithmetic).

Constructors are intentionally exposed here so you can pattern-match and see
the structure — that's the point of the comparison.
-}


type Natural
    = Zero
    | Add1 Natural



-- CONSTANTS


zero : Natural
zero =
    Zero


one : Natural
one =
    Add1 Zero


two : Natural
two =
    Add1 one


three : Natural
three =
    Add1 two


four : Natural
four =
    Add1 three


five : Natural
five =
    Add1 four


six : Natural
six =
    Add1 five


seven : Natural
seven =
    Add1 six


eight : Natural
eight =
    Add1 seven


nine : Natural
nine =
    Add1 eight


ten : Natural
ten =
    Add1 nine



-- CONSTRUCTION


{-| Returns Nothing for negative inputs. -}
fromInt : Int -> Maybe Natural
fromInt n =
    if n < 0 then
        Nothing

    else
        Just (fromIntHelper n)


fromIntHelper : Int -> Natural
fromIntHelper n =
    if n == 0 then
        Zero

    else
        Add1 (fromIntHelper (n - 1))


{-| Like fromInt but returns zero on invalid input. -}
fromSafeInt : Int -> Natural
fromSafeInt =
    fromInt >> Maybe.withDefault zero



-- COMPARISON


{-| Structural compare. -}
compare : Natural -> Natural -> Order
compare m n =
    case ( m, n ) of
        ( Zero, Zero ) ->
            EQ

        ( Zero, Add1 _ ) ->
            LT

        ( Add1 _, Zero ) ->
            GT

        ( Add1 pm, Add1 pn ) ->
            compare pm pn


{-| Is the second argument less than the first?

    (two |> isLessThan eight) == True

    (two |> isLessThan two) == False

-}
isLessThan : Natural -> Natural -> Bool
isLessThan b a =
    compare a b == LT


{-| Is the second argument less than or equal to the first?

    (two |> isLessThanOrEqual eight) == True

    (two |> isLessThanOrEqual two) == True

-}
isLessThanOrEqual : Natural -> Natural -> Bool
isLessThanOrEqual b a =
    not (a |> isGreaterThan b)


{-| Is the second argument greater than the first?

    (eight |> isGreaterThan two) == True

-}
isGreaterThan : Natural -> Natural -> Bool
isGreaterThan b a =
    compare a b == GT


{-| Is the second argument greater than or equal to the first?

    (eight |> isGreaterThanOrEqual two) == True

    (two |> isGreaterThanOrEqual two) == True

-}
isGreaterThanOrEqual : Natural -> Natural -> Bool
isGreaterThanOrEqual b a =
    not (a |> isLessThan b)


{-| Return the larger of two natural numbers. -}
max : Natural -> Natural -> Natural
max a b =
    if a |> isGreaterThanOrEqual b then
        a

    else
        b


{-| Return the smaller of two natural numbers. -}
min : Natural -> Natural -> Natural
min a b =
    if a |> isLessThanOrEqual b then
        a

    else
        b



-- PREDICATES


{-| Is the natural number zero? -}
isZero : Natural -> Bool
isZero n =
    n == Zero


{-| Is the natural number one? -}
isOne : Natural -> Bool
isOne n =
    n == Add1 Zero


{-| Is the natural number positive (greater than zero)? -}
isPositive : Natural -> Bool
isPositive =
    not << isZero


{-| Is the natural number even? -}
isEven : Natural -> Bool
isEven n =
    case n of
        Zero ->
            True

        Add1 p ->
            isOdd p


{-| Is the natural number odd? -}
isOdd : Natural -> Bool
isOdd n =
    case n of
        Zero ->
            False

        Add1 p ->
            isEven p



-- ARITHMETIC


{-| Add by recursion on the first argument. -}
add : Natural -> Natural -> Natural
add m n =
    case m of
        Zero ->
            n

        Add1 p ->
            Add1 (add p n)


{-| Saturating subtraction: sub m n = m - n, or zero if n > m. -}
sub : Natural -> Natural -> Natural
sub m n =
    case ( m, n ) of
        ( _, Zero ) ->
            m

        ( Zero, _ ) ->
            Zero

        ( Add1 pm, Add1 pn ) ->
            sub pm pn


{-| Multiply by recursion on the first argument. -}
mul : Natural -> Natural -> Natural
mul m n =
    case m of
        Zero ->
            Zero

        Add1 p ->
            add n (mul p n)


{-| Euclidean division: find the quotient and remainder.

    divModBy three ten == Just (three, one)
    -- because 3 * 3 = 9 <= 10 and 10 - 9 = 1

    divModBy zero ten == Nothing

-}
divModBy : Natural -> Natural -> Maybe ( Natural, Natural )
divModBy divisor dividend =
    case divisor of
        Zero ->
            Nothing

        _ ->
            Just (divModHelper divisor dividend)


divModHelper : Natural -> Natural -> ( Natural, Natural )
divModHelper divisor dividend =
    case compare dividend divisor of
        LT ->
            ( Zero, dividend )

        EQ ->
            ( one, Zero )

        GT ->
            let
                ( q, r ) =
                    divModHelper divisor (sub dividend divisor)
            in
            ( Add1 q, r )


{-| Quotient of Euclidean division. -}
divBy : Natural -> Natural -> Maybe Natural
divBy divisor =
    divModBy divisor >> Maybe.map Tuple.first


{-| Remainder of Euclidean division. -}
modBy : Natural -> Natural -> Maybe Natural
modBy divisor =
    divModBy divisor >> Maybe.map Tuple.second


{-| Exponentiation: exp b n = b ^ n.

    exp two three == eight

    exp n zero == one

    exp zero zero == one

-}
exp : Natural -> Natural -> Natural
exp b n =
    case n of
        Zero ->
            one

        Add1 p ->
            mul b (exp b p)



-- CONVERSION


{-| Tail-recursive toInt. -}
toInt : Natural -> Int
toInt n =
    toIntHelper n 0


toIntHelper : Natural -> Int -> Int
toIntHelper n acc =
    case n of
        Zero ->
            acc

        Add1 p ->
            toIntHelper p (acc + 1)


{-| Decimal string representation. -}
toString : Natural -> String
toString =
    toInt >> String.fromInt
