module Eval exposing (..)

import Dict
import Types exposing (..)
import Natural as Natural exposing (Natural)
import Integer as Integer exposing (Integer)
import Basics exposing (round)
import Vector


eval : Env Value -> Expr -> Value
eval env expr =
    case expr of
        Var x ->
            case lookupVar env x of
                Err msg -> Debug.todo ("Internal error: " ++ msg)
                Ok v -> v

        Lambda x body ->
            VClosure env x body

        App rator rand ->
            doApply (eval env rator) (eval env rand)

        -- Removed Zero and Add1, replaced with:
        Nat n ->
            VNat n

        IntLit z ->
            VInt z

        Flt f ->
            VFloat f

        -- Rec t tgt base step ->
        --     doRec t (eval env tgt) (eval env base) (eval env step)

        Plus l r ->
            case (eval env l, eval env r) of    -- evaluate both sides to get their values
                (VNat a, VNat b) ->             -- Once we get their values, we apply the 'add' opperation from Nat.elm
                    VNat (Natural.add a b)
                (VInt a, VInt b) ->
                    VInt (Integer.add a b)
                (VFloat a, VFloat b) ->
                    VFloat (a + b)
                _ ->
                    Debug.todo  ("Internal error: Plus applied to mismatched numeric types")

        Sub l r ->
            case (eval env l, eval env r) of
                (VNat a, VNat b) ->
                    VNat (Natural.sub a b)
                (VInt a, VInt b) ->
                    VInt (Integer.sub a b)
                (VFloat a, VFloat b) ->
                    VFloat (a - b)
                _ ->
                    Debug.todo  ("Internal error: Sub applied to mismatched numeric types")

        Mult l r ->
            case (eval env l, eval env r) of
                (VNat a, VNat b) ->
                    VNat (Natural.mul a b)
                (VInt a, VInt b) ->
                    VInt (Integer.mul a b)
                (VFloat a, VFloat b) ->
                    VFloat (a * b)
                _ ->
                    Debug.todo  ("Internal error: Mult applied to mismatched numeric types")

        Ann e t ->
            eval env e

        VecLit elems ->
            VVec (List.map (eval env) elems)

        VecAdd l r ->
            case ( eval env l, eval env r ) of
                ( VVec ls, VVec rs ) ->
                    VVec (List.map2 addValue ls rs)
                _ ->
                    Debug.todo "Internal error: VecAdd applied to non-vector"

        Dot l r ->
            case ( eval env l, eval env r ) of
                ( VVec ls, VVec rs ) ->
                    VFloat (Vector.dot (List.map toFloatValue ls) (List.map toFloatValue rs))
                _ ->
                    Debug.todo "Internal error: Dot applied to non-vector"

        Scale s v ->
            case ( eval env s, eval env v ) of
                ( VFloat c, VVec vs ) ->
                    VVec (List.map (\x -> VFloat (c * toFloatValue x)) vs)
                _ ->
                    Debug.todo "Internal error: Scale applied to non-scalar/non-vector"


-- Element-wise addition shared by Plus and VecAdd
addValue : Value -> Value -> Value
addValue a b =
    case ( a, b ) of
        ( VNat x, VNat y ) ->
            VNat (Natural.add x y)
        ( VInt x, VInt y ) ->
            VInt (Integer.add x y)
        ( VFloat x, VFloat y ) ->
            VFloat (x + y)
        _ ->
            Debug.todo "Internal error: addValue applied to mismatched or non-numeric values"


toFloatValue : Value -> Float
toFloatValue v =
    case v of
        VFloat f ->
            f
        _ ->
            Debug.todo "Internal error: expected a float value"


valueToNormal : Value -> Normal
valueToNormal v =
    case v of
        VNat n ->
            Normal { normalType = TNat, normalValue = VNat n }

        VInt z ->
            Normal { normalType = TInt, normalValue = VInt z }

        VFloat f ->
            Normal { normalType = TFlt, normalValue = VFloat f }

        VVec vs ->
            Normal
                { normalType = TVec (Nat (Natural.fromSafeInt (List.length vs))) TFlt
                , normalValue = VVec vs
                }

        VNeutral ty neu ->
            Normal { normalType = ty, normalValue = VNeutral ty neu }

        VClosure _ _ _ ->
            Debug.todo "Internal error: cannot convert closure to normal without a known type"


doApply : Value -> Value -> Value -- Apply the function to the argument
doApply rator arg =
    case rator of
        VClosure env x body ->
            eval (extend env x arg) body

        VNeutral ty neu ->
            VNeutral ty (NApp neu (valueToNormal arg))

        _ -> 
            Debug.todo  ("Error: applying non-function")


-- Attaches new value at the front
extend : Env Value -> Name -> Value -> Env Value
extend env x arg =
    Dict.insert x arg env


-- With insiration of Lucas's getEnv function & the tutorial's lookupVar:
lookupVar : Env Value -> Name -> Result Error Value
lookupVar env x =
  case Dict.get x env of
    Just v -> Ok v
    Nothing -> Err ("Not found: Couldn't get var " ++ x ++ " from evaluation env.")
