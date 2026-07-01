module ExperimentTest exposing (..)

{-| Peano vs elm-natural comparison experiment.

Two implementations of the same mathematical object (ℕ):

  PeanoNat  ─  type Natural = Zero | Add1 Natural
    Unary representation. Ten is literally Add1(Add1(Add1(...))).
    + Constructors are exposed: you can pattern-match and do structural induction.
    + Easy to read the definition and prove properties by hand.
    - O(n) space. The number 1000 allocates 1000 constructors.
    - O(n) add, O(n²) mul. Large numbers hang or OOM.

  Natural (elm-natural / dwayne)  ─  base-2^26 polynomial
    [c₀, c₁, ...] where value = Σ cᵢ · 2^(26i).
    + O(log n) space, fast arithmetic.
    + Safe for large literals in the term language.
    - Opaque type: no pattern matching, cannot write structural induction in Elm.

For the NbE project: Nat literals in expressions (e.g. the term `1000`) use
elm-natural so the interpreter doesn't choke. PeanoNat lives here for pedagogy.
-}

import Expect
import Test exposing (..)
import PeanoNat
import Natural


suite : Test
suite =
    describe "Peano vs elm-natural"
        [ describe "Both agree on arithmetic results"
            [ test "add: 3 + 4 == 7" <|
                \_ ->
                    Expect.equal
                        (PeanoNat.toInt (PeanoNat.add PeanoNat.three PeanoNat.four))
                        (Natural.toInt  (Natural.add  Natural.three  Natural.four))

            , test "mul: 3 * 3 == 9" <|
                \_ ->
                    Expect.equal
                        (PeanoNat.toInt (PeanoNat.mul PeanoNat.three PeanoNat.three))
                        (Natural.toInt  (Natural.mul  Natural.three  Natural.three))

            , test "sub (saturating): 2 - 5 == 0" <|
                \_ ->
                    Expect.equal
                        (PeanoNat.toInt (PeanoNat.sub PeanoNat.two PeanoNat.five))
                        (Natural.toInt  (Natural.sub  Natural.two  Natural.five))

            , test "compare: 2 vs 5 == LT" <|
                \_ ->
                    Expect.equal
                        (PeanoNat.compare PeanoNat.two PeanoNat.five)
                        (Natural.compare  Natural.two  Natural.five)
            ]

        , describe "Peano only: structural induction via pattern match"
            -- elm-natural is opaque — you cannot write these directly.
            [ test "count Add1 layers to measure a number" <|
                \_ ->
                    let
                        depth n =
                            case n of
                                PeanoNat.Zero   -> 0
                                PeanoNat.Add1 p -> 1 + depth p
                    in
                    Expect.equal (depth PeanoNat.three) 3

            , test "predecessor by stripping one Add1" <|
                \_ ->
                    let
                        pred n =
                            case n of
                                PeanoNat.Zero   -> PeanoNat.Zero
                                PeanoNat.Add1 p -> p
                    in
                    Expect.equal (PeanoNat.toInt (pred PeanoNat.five)) 4

            , test "custom isEven by mutual recursion on structure" <|
                \_ ->
                    let
                        myEven n =
                            case n of
                                PeanoNat.Zero   -> True
                                PeanoNat.Add1 p -> myOdd p
                        myOdd n =
                            case n of
                                PeanoNat.Zero   -> False
                                PeanoNat.Add1 p -> myEven p
                    in
                    Expect.equal
                        [ myEven PeanoNat.zero
                        , myEven PeanoNat.two
                        , myEven PeanoNat.four
                        , myOdd  PeanoNat.one
                        , myOdd  PeanoNat.three
                        ]
                        [ True, True, True, True, True ]
            ]

        , describe "elm-natural only: larger numbers stay fast"
            [ test "fromSafeInt 1000 round-trips through toInt" <|
                \_ ->
                    Expect.equal
                        (Natural.toInt (Natural.fromSafeInt 1000))
                        1000
                -- PeanoNat.fromSafeInt 1000 allocates 1000 constructors
                -- and PeanoNat.toInt walks all of them. Fine for tests,
                -- but multiply two such numbers and you feel it.

            , test "add two 100s gives 200" <|
                \_ ->
                    let n = Natural.fromSafeInt 100
                    in
                    Expect.equal
                        (Natural.toInt (Natural.add n n))
                        200
            ]
        ]
