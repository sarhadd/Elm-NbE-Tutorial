module Vector exposing (..)

{- Vector Library used for examples and tests using the type Flt (Float)

This module is independent of the evaluator: it provides Elm-level helper functions
for working with Floats:

- "roundFloat" : round a `Float` to the nearest `Int`.
- "dot"        : compute the dot product of two vectors (lists of floats). If lengths
                 differ, it multiplies until one list ends.
- "scale"      : scale every element of a vector by a constant scalar.
-}

roundFloat : Float -> Int
roundFloat f =
    round f

dot : List Float -> List Float -> Float
dot a b =
    List.map2 (\x y -> x * y) a b
        |> List.sum

scale : Float -> List Float -> List Float
scale cnst vec =
    List.map (\x -> cnst * x) vec


add : List Float -> List Float -> List Float
add a b =
    List.map2 (\x y -> x + y) a b

sub : List Float -> List Float -> List Float
sub a b =
    List.map2 (\x y -> x - y) a b

sum : List Float -> Float
sum list = List.sum list

-- Note: map2 for combining two list elements. element by element
--       map is for elementwise transformations on a singular list.