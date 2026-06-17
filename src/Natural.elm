module Natural exposing
    ( Natural
    , zero, one, two, three, four, five
    , fromInt, fromSafeInt
    , add
    , compare
    , toInt
    )

{-| Natural numbers in Peano (unary) form.

    type Natural = Zero | Add1 Natural

Constructors are hidden; use the named constants or fromInt.
-}


type Natural
    = Zero
    | Add1 Natural



-- CONSTANTS


zero : Natural
zero = Zero

one : Natural
one = Add1 Zero

two : Natural
two = Add1 one

three : Natural
three = Add1 two

four : Natural
four = Add1 three

five : Natural
five = Add1 four



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



-- ARITHMETIC


{-| Add by recursion on the first argument. -}
add : Natural -> Natural -> Natural
add m n =
    case m of
        Zero   -> n
        Add1 p -> Add1 (add p n)



-- COMPARISON


{-| Structural compare. -}
compare : Natural -> Natural -> Order
compare m n =
    case ( m, n ) of
        ( Zero,    Zero    ) -> EQ
        ( Zero,    Add1 _  ) -> LT
        ( Add1 _,  Zero    ) -> GT
        ( Add1 pm, Add1 pn ) -> compare pm pn



-- CONVERSION


{-| Tail-recursive toInt. -}
toInt : Natural -> Int
toInt n =
    toIntHelper n 0


toIntHelper : Natural -> Int -> Int
toIntHelper n acc =
    case n of
        Zero   -> acc
        Add1 p -> toIntHelper p (acc + 1)
