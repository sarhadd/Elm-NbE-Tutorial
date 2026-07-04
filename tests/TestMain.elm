module TestMain exposing (..)

import Test exposing (..)
import Expect
import Debug

-- Imports depend on what the original TestMain provided.
import Defs exposing (..)
import Types exposing (..)
import Checking exposing (..)
import Readback
import Nat as Natural

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


vecSuite : Test
vecSuite =
    let
        litExpr : Expr
        litExpr =
            VecLit [ Flt 1, Flt 2, Flt 3 ]

        litResult : Result Message Normal
        litResult =
            normWithDefs noDefs litExpr

        addResult : Result Message Normal
        addResult =
            normWithDefs noDefs (VecAdd (VecLit [ Flt 1, Flt 2 ]) (VecLit [ Flt 3, Flt 4 ]))

        dotResult : Result Message Normal
        dotResult =
            normWithDefs noDefs (Dot (VecLit [ Flt 1, Flt 2, Flt 3 ]) (VecLit [ Flt 4, Flt 5, Flt 6 ]))

        scaleResult : Result Message Normal
        scaleResult =
            normWithDefs noDefs (Scale (Flt 2) (VecLit [ Flt 1, Flt 2, Flt 3 ]))

        mismatchedAddResult : Result Message Normal
        mismatchedAddResult =
            normWithDefs noDefs (VecAdd (VecLit [ Flt 1, Flt 2 ]) (VecLit [ Flt 3, Flt 4, Flt 5 ]))
    in
    describe "TVec"
        [ test "a vector literal synthesizes Vec 3 Flt and reads back unchanged" <|
            \_ ->
                case litResult of
                    Ok normal ->
                        if normalType normal == TVec (Nat (Natural.fromSafeInt 3)) TFlt then
                            Expect.equal (Readback.readBackNormal 0 normal) litExpr
                        else
                            Expect.fail ("Unexpected type: " ++ Debug.toString (normalType normal))
                    other ->
                        Expect.fail ("Unexpected result: " ++ Debug.toString other)

        , test "VecAdd adds element-wise" <|
            \_ ->
                case addResult of
                    Ok normal ->
                        Expect.equal (Readback.readBackNormal 0 normal) (VecLit [ Flt 4, Flt 6 ])
                    other ->
                        Expect.fail ("Unexpected result: " ++ Debug.toString other)

        , test "Dot computes the dot product" <|
            \_ ->
                case dotResult of
                    Ok normal ->
                        Expect.equal (Readback.readBackNormal 0 normal) (Flt 32)
                    other ->
                        Expect.fail ("Unexpected result: " ++ Debug.toString other)

        , test "Scale multiplies every element by the scalar" <|
            \_ ->
                case scaleResult of
                    Ok normal ->
                        Expect.equal (Readback.readBackNormal 0 normal) (VecLit [ Flt 2, Flt 4, Flt 6 ])
                    other ->
                        Expect.fail ("Unexpected result: " ++ Debug.toString other)

        , test "VecAdd on mismatched lengths is a type error" <|
            \_ ->
                case mismatchedAddResult of
                    Err _ ->
                        Expect.pass
                    Ok normal ->
                        Expect.fail ("Expected a type error, but got: " ++ Debug.toString normal)
        ]


-- TODO: Add more tests for other Expr types.