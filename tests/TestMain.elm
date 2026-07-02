module TestMain exposing (..)

import Test exposing (..)
import Expect
import Debug

-- Imports depend on what the original TestMain provided.
import Defs exposing (..)
import Types exposing (..)
import Checking exposing (..)
import Natural

suite : Test
suite =
    -- Build a small defs containing a primitive + for tests:
    let
        test1 : Result Message Normal
        test1 =
            normWithDefs initDefs (Var "+")

        test2 : Result Message Normal
        test2 =
            normWithDefs initDefs (App (Var "+") (Nat (Natural.fromSafeInt 3)))

        test3 : Result Message Normal
        test3 =
            normWithDefs initDefs (App (App (Var "+") (Nat (Natural.fromSafeInt 3))) (Nat (Natural.fromSafeInt 2)))
    in
    describe "normWithTestDefs"
        [ test "Looking up + by itself stays as a lambda" <|
                \_ ->
                    case test1 of
                        Ok normal ->
                            if normalType normal == TArr TNat (TArr TNat TNat) then
                                case normalValue normal of
                                    VClosure _ _ _ ->
                                        Expect.pass
                                    _ ->
                                        Expect.fail ("Unexpected result: " ++ Debug.toString normal)
                            else
                                Expect.fail ("Unexpected type: " ++ Debug.toString (normalType normal))
                        other ->
                            Expect.fail ("Unexpected result: " ++ Debug.toString other)
                            
            , test "+ applied to three reduces to a partial lambda" <|
                \_ ->
                    case test2 of
                        Ok normal ->
                            if normalType normal == TArr TNat TNat then
                                case normalValue normal of
                                    VClosure _ _ _ ->
                                        Expect.pass
                                    _ ->
                                        Expect.fail ("Unexpected result: " ++ Debug.toString normal)
                            else
                                Expect.fail ("Unexpected type: " ++ Debug.toString (normalType normal))

                        other ->
                            Expect.fail ("Unexpected result: " ++ Debug.toString other)

            -- fuzz runs the test 100 times with randomly-generated inputs!
            -- , fuzz string  " ... " <|
            , test "+ applied to three and two fully reduces to five" <|
                \_ ->
                    case test3 of
                        Ok normal ->
                            if normalType normal == TNat then
                                case normalValue normal of
                                    VNat n ->
                                        Expect.equal (Natural.toInt n) 5
                                    other ->
                                        Expect.fail ("Unexpected result: " ++ Debug.toString other)
                            else
                                Expect.fail ("Unexpected type: " ++ Debug.toString (normalType normal))

                        other ->
                            Expect.fail ("Unexpected result: " ++ Debug.toString other)
 
        ]


-- TODO: Add more tests for other Expr types.