module Readback exposing (..)

import Types exposing (..)
import Eval exposing (doApply)


readBackNormal : Int -> Normal -> Expr
readBackNormal n norm =
    readBackValue n (normalType norm) (normalValue norm)


readBackValue : Int -> Ty -> Value -> Expr
readBackValue n ty v =
    case ( ty, v ) of
        ( TNat, VNat k ) ->
            Nat k

        ( TInt, VInt z ) ->
            IntLit z

        ( TFlt, VFloat f ) ->
            Flt f

        ( TArr t1 t2, fun ) ->
            let
                x =
                    "x" ++ String.fromInt n

                xVal =
                    VNeutral t1 (NVar x)
            in
            Lambda x (readBackValue (n + 1) t2 (doApply fun xVal))

        ( TVec _ elemTy, VVec vs ) ->
            VecLit (List.map (readBackValue n elemTy) vs)

        ( _, VNeutral _ neu ) ->
            readBackNeutral n neu

        _ ->
            Debug.todo "Internal error: mismatched type and value at readBackValue"


readBackNeutral : Int -> Neutral -> Expr
readBackNeutral n neu =
    case neu of
        NVar x ->
            Var x

        NApp rator arg ->
            App (readBackNeutral n rator) (readBackNormal n arg)

        NPlus l r ->
            Plus (readBackNeutral n l) (readBackNormal n r)

        NVecAdd l r ->
            VecAdd (readBackNeutral n l) (readBackNormal n r)

        NDot l r ->
            Dot (readBackNeutral n l) (readBackNormal n r)

        NScale l r ->
            Scale (readBackNormal n l) (readBackNeutral n r)
