module Eval exposing (..)

import Dict
import Types exposing (..)
import Natural exposing (Natural)


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

        -- Rec t tgt base step ->
        --     doRec t (eval env tgt) (eval env base) (eval env step)

        -- TODO: add Plus case!!
        Plus l r ->
            case (eval env l, eval env r) of    -- evaluate both sides to get their values
                (VNat a, VNat b) ->             -- Once we get their values, we apply the 'add' opperation from Natural.elm
                    VNat (Natural.add a b)
                _ ->
                    Debug.todo  ("Internal error: Plus applied to non-natural")

        Ann e t ->
            eval env e


doApply : Value -> Value -> Value
doApply rator arg =
    case rator of
        VClosure env x body ->
            eval (extend env x arg) body
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
