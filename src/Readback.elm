-- Readback TODO

module Readback exposing (..)

import Types exposing (..)
import Fresh exposing (..)
import Eval exposing (doApply)


readBackNormal : List Name -> Normal -> Expr
readBackNormal used (Normal t v) =
    readBack used t v

readBack : List Name -> Ty -> Value -> Expr
readBack used ty v =
    case ( ty, v ) of
        ( TNat, VNat n ) ->
            ELit n

        ( TArr t1 t2, fun ) ->
            let
                x =
                    freshen used (argName fun)

                xVal =
                    VNeutral t1 (NVar x)
            in
            ELambda x
                (readBack (x :: used) t2 (doApply fun xVal))

        ( t1, VNeutral t2 neu ) ->
            -- Note: checking t1 and t2 for equality here is a good way to find bugs,
            -- but is not strictly necessary.
            if t1 == t2 then
                readBackNeutral used neu

            else
                Debug.todo "Internal error: mismatched types at readBackNeutral"

        _ ->
            Debug.todo "Internal error: mismatched type and value at readBack"

argName : Value -> Name
argName fun =
    case fun of
        VClosure _ x _ ->
            x

        _ ->
            "x"


readBackNeutral : List Name -> Neutral -> Expr
readBackNeutral used neu =
    case neu of
        NVar x ->
            EVar x

        NApp rator arg ->
            EApp (readBackNeutral used rator) (readBackNormal used arg)