module Nat exposing
    ( Natural
    , zero, one, two, three, four, five, six, seven, eight, nine, ten
    , fromInt, fromSafeInt
    , compare, isLessThan, isLessThanOrEqual, isGreaterThan, isGreaterThanOrEqual, max, min
    , isZero, isOne, isPositive, isEven, isOdd
    , add, sub, mul, divModBy, divBy, modBy, exp
    , toInt, toString
    )

{-| Natural numbers in base-2^26 positional form — a local reimplementation of
the representational idea behind dwayne/elm-natural, kept opaque like the real
package (contrast with PeanoNat, which exposes its constructors on purpose).

    value = x0 + x1*base + x2*base^2 + ... + x_{n-1}*base^(n-1)

    type Natural = Natural (List Int)   -- little-endian digits, 0 <= xi < base

The invariant maintained everywhere: the most significant digit (the last
element of the list) is never zero, and zero itself is `Natural []`. That
invariant is what makes `(==)` and `compare` correct without a normalization
pass — every function below is written to preserve it directly, the same
discipline the real library uses.

This is simplified relative to dwayne/elm-natural, trading peak performance
for readability (this is a tutorial, not a production library):

  - `mul` is schoolbook shift-and-add, not Karatsuba.
  - `divModBy` is recursive-doubling long division, not Knuth's Algorithm D.
  - `toString` goes through `toInt`, so — unlike the real library — it only
    works up to `maxSafeInt` (2^53 - 1). Fine for a term language whose
    literals are typed in by hand.

What's preserved is the property PeanoNat cannot have: a number needs only
O(log n) digits (base 2^26) rather than O(n) unary `Add1` constructors, so
`add`/`mul`/`compare` stay fast on numbers PeanoNat would choke on. See
ExperimentTest.elm for the side-by-side.

-}


-- REPRESENTATION


base : Int
base =
    2 ^ 26


type Natural
    = Natural (List Int)



-- CONSTANTS


zero : Natural
zero =
    Natural []


one : Natural
one =
    Natural [ 1 ]


two : Natural
two =
    Natural [ 2 ]


three : Natural
three =
    Natural [ 3 ]


four : Natural
four =
    Natural [ 4 ]


five : Natural
five =
    Natural [ 5 ]


six : Natural
six =
    Natural [ 6 ]


seven : Natural
seven =
    Natural [ 7 ]


eight : Natural
eight =
    Natural [ 8 ]


nine : Natural
nine =
    Natural [ 9 ]


ten : Natural
ten =
    Natural [ 10 ]



-- CONSTRUCTION


{-| Returns Nothing for negative inputs. -}
fromInt : Int -> Maybe Natural
fromInt n =
    if n < 0 then
        Nothing

    else
        Just (Natural (fromIntHelper n))


{-| n is assumed >= 0, so plain `//` / `Basics.modBy` (no floor tricks needed).
Stops exactly when n hits 0, so it never appends a spurious top zero digit.
-}
fromIntHelper : Int -> List Int
fromIntHelper n =
    if n == 0 then
        []

    else
        Basics.modBy base n :: fromIntHelper (n // base)


{-| Like fromInt but returns zero on invalid input. -}
fromSafeInt : Int -> Natural
fromSafeInt =
    fromInt >> Maybe.withDefault zero



-- COMPARISON


{-| Structural compare, most-significant digit wins.

Walks the digit lists least-significant-first, remembering the last pair of
digits where they differed. Because there are no top zero digits, running out
of digits first means "fewer digits" which means "smaller" — no length lookup
needed.
-}
compare : Natural -> Natural -> Order
compare (Natural xsLE) (Natural ysLE) =
    compareLE xsLE ysLE


compareLE : List Int -> List Int -> Order
compareLE xsLE ysLE =
    compareLEHelper 0 0 xsLE ysLE


compareLEHelper : Int -> Int -> List Int -> List Int -> Order
compareLEHelper a b xsLE ysLE =
    case ( xsLE, ysLE ) of
        ( [], [] ) ->
            Basics.compare a b

        ( [], _ ) ->
            LT

        ( _, [] ) ->
            GT

        ( x :: xsRest, y :: ysRest ) ->
            if x == y then
                compareLEHelper a b xsRest ysRest

            else
                compareLEHelper x y xsRest ysRest


{-| Is the second argument less than the first?

    (two |> isLessThan eight) == True

    (two |> isLessThan two) == False

-}
isLessThan : Natural -> Natural -> Bool
isLessThan b a =
    compare a b == LT


{-| Is the second argument less than or equal to the first? -}
isLessThanOrEqual : Natural -> Natural -> Bool
isLessThanOrEqual b a =
    not (a |> isGreaterThan b)


{-| Is the second argument greater than the first? -}
isGreaterThan : Natural -> Natural -> Bool
isGreaterThan b a =
    compare a b == GT


{-| Is the second argument greater than or equal to the first? -}
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
    n == zero


{-| Is the natural number one? -}
isOne : Natural -> Bool
isOne n =
    n == one


{-| Is the natural number positive (greater than zero)? -}
isPositive : Natural -> Bool
isPositive =
    not << isZero


{-| Is the natural number even? Only the least-significant digit matters. -}
isEven : Natural -> Bool
isEven (Natural digitsLE) =
    case digitsLE of
        [] ->
            True

        d :: _ ->
            Basics.modBy 2 d == 0


{-| Is the natural number odd? -}
isOdd : Natural -> Bool
isOdd =
    not << isEven



-- ARITHMETIC


{-| Add two natural numbers, schoolbook column addition with carry. -}
add : Natural -> Natural -> Natural
add (Natural xsLE) (Natural ysLE) =
    Natural (addLE xsLE ysLE)


addLE : List Int -> List Int -> List Int
addLE xsLE ysLE =
    addHelper xsLE ysLE 0 []


addHelper : List Int -> List Int -> Int -> List Int -> List Int
addHelper xsLE ysLE carry zsBE =
    -- carry is always 0 or 1 here, so no top zero digit can be produced:
    -- the final carry is only prepended when it's nonzero.
    case ( xsLE, ysLE ) of
        ( [], [] ) ->
            (if carry == 0 then
                zsBE

             else
                carry :: zsBE
            )
                |> List.reverse

        ( x :: restXsLE, [] ) ->
            let
                z =
                    x + carry
            in
            addHelper restXsLE [] (z // base) (Basics.modBy base z :: zsBE)

        ( [], y :: restYsLE ) ->
            let
                z =
                    y + carry
            in
            addHelper [] restYsLE (z // base) (Basics.modBy base z :: zsBE)

        ( x :: restXsLE, y :: restYsLE ) ->
            let
                z =
                    x + y + carry
            in
            addHelper restXsLE restYsLE (z // base) (Basics.modBy base z :: zsBE)


{-| Saturating subtraction: sub m n = m - n, or zero if n > m. -}
sub : Natural -> Natural -> Natural
sub (Natural xsLE) (Natural ysLE) =
    Natural (subHelper xsLE ysLE 0 [])


subHelper : List Int -> List Int -> Int -> List Int -> List Int
subHelper xsLE ysLE carry zsBE =
    -- carry is 0 or -1 (a borrow). Unlike addition, cancellation at the top
    -- (e.g. 1000000 - 1) can strip several digits, so we trim top zeros
    -- before reversing back to LE.
    case ( xsLE, ysLE ) of
        ( [], [] ) ->
            if carry == 0 then
                zsBE
                    |> removeTopZeros
                    |> List.reverse

            else
                -- xsLE < ysLE: saturate at zero.
                []

        ( x :: restXsLE, [] ) ->
            let
                ( newCarry, z ) =
                    (x + carry) |> floorDivModBy base
            in
            subHelper restXsLE [] newCarry (z :: zsBE)

        ( [], y :: restYsLE ) ->
            let
                ( newCarry, z ) =
                    (carry - y) |> floorDivModBy base
            in
            subHelper [] restYsLE newCarry (z :: zsBE)

        ( x :: restXsLE, y :: restYsLE ) ->
            let
                ( newCarry, z ) =
                    (x - y + carry) |> floorDivModBy base
            in
            subHelper restXsLE restYsLE newCarry (z :: zsBE)


{-| Multiply two natural numbers: schoolbook shift-and-add. For each digit
`y` of the second number at position `i`, scale the first number by `y` and
shift it left by `i` base-digits (i.e. multiply by base^i), then sum all the
partial products. This is the O(n^2)-in-digit-count algorithm you'd do by
hand in base 2^26 instead of base 10 — the real library uses Karatsuba
instead, which pays off once numbers get very large.
-}
mul : Natural -> Natural -> Natural
mul (Natural xsLE) (Natural ysLE) =
    ysLE
        |> List.indexedMap
            (\i y ->
                if y == 0 then
                    []

                else
                    shiftDigits i (scaleHelper y xsLE 0)
            )
        |> List.foldl (\partial acc -> addLE partial acc) []
        |> Natural


{-| Multiply a raw digit list by a single digit `y` (0 < y < base), carrying
between digits. Guaranteed not to leave a top zero digit as long as `y` is
positive and `xsLE` itself has none (see reasoning in the module docs).
-}
scaleHelper : Int -> List Int -> Int -> List Int
scaleHelper y xsLE carry =
    case xsLE of
        [] ->
            if carry == 0 then
                []

            else
                [ carry ]

        x :: restXsLE ->
            let
                product =
                    y * x + carry
            in
            Basics.modBy base product :: scaleHelper y restXsLE (product // base)


{-| Multiply by base^i, i.e. prepend i zero digits. Guarded against the
zero case so we never turn `[]` into a nonempty list of zeros.
-}
shiftDigits : Int -> List Int -> List Int
shiftDigits i xsLE =
    case xsLE of
        [] ->
            []

        _ ->
            List.repeat i 0 ++ xsLE


{-| Euclidean division: find the quotient and remainder.

    divModBy three ten == Just (three, one)

    divModBy zero ten == Nothing

Implemented as recursive-doubling long division rather than Knuth's
Algorithm D: double the divisor until it exceeds the dividend, then unwind,
doubling the quotient at each level and adding one whenever the doubled
divisor still fits. O(digits * log(dividend / divisor)) — slower than the
real library's estimate-and-correct approach, but the recursion mirrors the
textbook proof directly.
-}
divModBy : Natural -> Natural -> Maybe ( Natural, Natural )
divModBy divisor dividend =
    if isZero divisor then
        Nothing

    else
        Just (divModHelper divisor dividend)


divModHelper : Natural -> Natural -> ( Natural, Natural )
divModHelper d n =
    if n |> isLessThan d then
        ( zero, n )

    else
        let
            ( q, r ) =
                divModHelper (double d) n
        in
        if r |> isGreaterThanOrEqual d then
            ( add (double q) one, sub r d )

        else
            ( double q, r )


double : Natural -> Natural
double n =
    add n n


{-| Quotient of Euclidean division. -}
divBy : Natural -> Natural -> Maybe Natural
divBy divisor =
    divModBy divisor >> Maybe.map Tuple.first


{-| Remainder of Euclidean division. -}
modBy : Natural -> Natural -> Maybe Natural
modBy divisor =
    divModBy divisor >> Maybe.map Tuple.second


{-| Exponentiation by repeated squaring: exp b n = b ^ n.

    exp two three == eight

    exp n zero == one

    exp zero zero == one

-}
exp : Natural -> Natural -> Natural
exp b n =
    if isZero n then
        one

    else if isZero b then
        zero

    else
        expHelper b n one


expHelper : Natural -> Natural -> Natural -> Natural
expHelper b n y =
    if isZero n then
        y

    else
        let
            ( q, r ) =
                n |> divModBy two |> Maybe.withDefault ( one, zero )

            bSquared =
                mul b b
        in
        if r == zero then
            expHelper bSquared q y

        else
            expHelper bSquared q (mul y b)



-- CONVERSION


{-| Reconstruct the Int value via Horner's method on the LE digit list. Only
correct up to maxSafeInt (2^53 - 1) — unlike the real library, this does not
implement modular wraparound for larger values, since the tutorial never
needs numbers that large.
-}
toInt : Natural -> Int
toInt (Natural digitsLE) =
    List.foldr (\d acc -> d + acc * base) 0 digitsLE


{-| Decimal string representation, via toInt (see its caveat above). -}
toString : Natural -> String
toString =
    toInt >> String.fromInt



-- HELPERS


{-| Like `//` / `Basics.modBy`, but correct when the dividend is negative
(floors instead of truncating toward zero) — needed by `subHelper`, where
`x - y + carry` can go negative.
-}
floorDivModBy : Int -> Int -> ( Int, Int )
floorDivModBy divisor dividend =
    ( floor (toFloat dividend / toFloat divisor)
    , Basics.modBy divisor dividend
    )


removeTopZeros : List Int -> List Int
removeTopZeros digits =
    case digits of
        [] ->
            []

        d :: restDigits ->
            if d == 0 then
                removeTopZeros restDigits

            else
                digits
