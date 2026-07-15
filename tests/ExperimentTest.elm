module ExperimentTest exposing (..)

{-| Peano vs Nat comparison experiment.

Two implementations of the same mathematical object (ℕ), both local to this
project (no external elm-natural dependency in the main line anymore):

  PeanoNat  ─  type Natural = Zero | Add1 Natural
    Unary representation. Ten is literally Add1(Add1(Add1(...))).
    + Constructors are exposed: you can pattern-match and do structural induction.
    + Easy to read the definition and prove properties by hand.
    - O(n) space. The number 1000 allocates 1000 constructors.
    - O(n) add, O(n²) mul. Large numbers hang or OOM.

  Natural (elm-natural)  ─  base-2^26 polynomial representation
    [c₀, c₁, ...] where value = Σ cᵢ · 2^(26i).
    + O(log n) space, fast arithmetic (schoolbook mul, recursive-doubling division —
      simpler than elm-natural's Karatsuba/Knuth, same digit-count win over Peano).
    + Safe for large literals in the term language.
    - Opaque type: no pattern matching, cannot write structural induction in Elm.

For the NbE project: Nat literals in expressions (e.g. the term `1000`) use Nat
so the interpreter doesn't choke. PeanoNat lives here for pedagogy and comparison
only — everything else in this project (Types.elm, Eval.elm, ...) uses Nat.
-}

import Dict
import Expect
import Readback
import Test exposing (..)
import Types exposing (..)
import Defs exposing (..)
import PeanoNat
import Natural as Nat


suite : Test
suite =
    describe "Peano vs Nat"
        [ describe "Both agree on arithmetic results"
            [ test "add: 3 + 4 == 7" <|
                \_ ->
                    Expect.equal
                        (PeanoNat.toInt (PeanoNat.add PeanoNat.three PeanoNat.four))
                        (Nat.toInt      (Nat.add      Nat.three      Nat.four))

            , test "mul: 3 * 3 == 9" <|
                \_ ->
                    Expect.equal
                        (PeanoNat.toInt (PeanoNat.mul PeanoNat.three PeanoNat.three))
                        (Nat.toInt      (Nat.mul      Nat.three      Nat.three))

            , test "sub (saturating): 2 - 5 == 0" <|
                \_ ->
                    Expect.equal
                        (PeanoNat.toInt (PeanoNat.sub PeanoNat.two PeanoNat.five))
                        (Nat.toInt      (Nat.sub      Nat.two      Nat.five))

            , test "compare: 2 vs 5 == LT" <|
                \_ ->
                    Expect.equal
                        (PeanoNat.compare PeanoNat.two PeanoNat.five)
                        (Nat.compare      Nat.two      Nat.five)

            , test "divModBy: 10 / 3 == (3, 1)" <|
                \_ ->
                    Expect.equal
                        (PeanoNat.divModBy PeanoNat.three PeanoNat.ten |> Maybe.map (Tuple.mapBoth PeanoNat.toInt PeanoNat.toInt))
                        (Nat.divModBy      Nat.three      Nat.ten      |> Maybe.map (Tuple.mapBoth Nat.toInt Nat.toInt))

            , test "exp: 2 ^ 10 == 1024" <|
                \_ ->
                    Expect.equal
                        (PeanoNat.toInt (PeanoNat.exp PeanoNat.two (PeanoNat.fromSafeInt 10)))
                        (Nat.toInt      (Nat.exp      Nat.two      (Nat.fromSafeInt 10)))
            ]


        , describe "Nat only: larger numbers stay fast"
            [ test "fromSafeInt 1000 round-trips through toInt" <|
                \_ ->
                    Expect.equal
                        (Nat.toInt (Nat.fromSafeInt 1000))
                        1000
                -- PeanoNat.fromSafeInt 1000 allocates 1000 constructors
                -- and PeanoNat.toInt walks all of them. Fine for tests,
                -- but multiply two such numbers and you feel it.

            , test "add two 100s gives 200" <|
                \_ ->
                    let n = Nat.fromSafeInt 100
                    in
                    Expect.equal
                        (Nat.toInt (Nat.add n n))
                        200

            , test "mul: 999 * 999 == 998001 (schoolbook mul keeps up)" <|
                \_ ->
                    let n = Nat.fromSafeInt 999
                    in
                    Expect.equal
                        (Nat.toInt (Nat.mul n n))
                        998001
            ]
        , describe "Neutral normalization"
            [ test "neutral function application readbacks to App" <|
                \_ ->
                    let
                        defs : Defs
                        defs =
                            Dict.fromList
                                [ ( "g"
                                  , Normal
                                        { normalType = TArr TNat TNat
                                        , normalValue = VNeutral (TArr TNat TNat) (NVar "g")
                                        }
                                  )
                                ]

                        expr : Expr
                        expr =
                            App
                                (Var "g")
                                (Nat (Nat.fromSafeInt 1))

                        result : Result Message Expr
                        result =
                            Result.map (Readback.readBackNormal 0) (normWithDefs defs expr)
                    in
                    case result of
                        Ok readExpr ->
                            Expect.equal readExpr expr
                        Err msg ->
                            Expect.fail msg
            ]
        ]


        -- , describe "Peano only: structural induction via pattern match"
        --     -- Nat is opaque — you cannot write these directly.
        --     [ test "count Add1 layers to measure a number" <|
        --         \_ ->
        --             let
        --                 depth n =
        --                     case n of
        --                         PeanoNat.Zero   -> 0
        --                         PeanoNat.Add1 p -> 1 + depth p
        --             in
        --             Expect.equal (depth PeanoNat.three) 3

        --     , test "predecessor by stripping one Add1" <|
        --         \_ ->
        --             let
        --                 pred n =
        --                     case n of
        --                         PeanoNat.Zero   -> PeanoNat.Zero
        --                         PeanoNat.Add1 p -> p
        --             in
        --             Expect.equal (PeanoNat.toInt (pred PeanoNat.five)) 4

        --     , test "custom isEven by mutual recursion on structure" <|
        --         \_ ->
        --             let
        --                 myEven n =
        --                     case n of
        --                         PeanoNat.Zero   -> True
        --                         PeanoNat.Add1 p -> myOdd p
        --                 myOdd n =
        --                     case n of
        --                         PeanoNat.Zero   -> False
        --                         PeanoNat.Add1 p -> myEven p
        --             in
        --             Expect.equal
        --                 [ myEven PeanoNat.zero
        --                 , myEven PeanoNat.two
        --                 , myEven PeanoNat.four
        --                 , myOdd  PeanoNat.one
        --                 , myOdd  PeanoNat.three
        --                 ]
        --                 [ True, True, True, True, True ]
        --     ]
