-- This file supports testing in TestMain.elm
module Defs exposing (..)

import Dict
import Debug
import Readback
import Types exposing (..)
import Checking exposing (synth)
import Eval exposing (eval)

noDefs : Defs
noDefs =
    Dict.empty
defsToContext : Defs -> Context
defsToContext defs =
    Dict.map (\_ normal -> normalType normal) defs
defsToEnv : Defs -> Env Value
defsToEnv defs =
    Dict.map (\_ normal -> normalValue normal) defs

plusExpr : Expr
plusExpr =
    Ann
        (Lambda "x" (Lambda "y" (Plus (Var "x") (Var "y"))))
        (TArr TNat (TArr TNat TNat))

initDefs : Defs
initDefs =
    case addDefs noDefs [("+", plusExpr)] of
        Ok d ->
            d
        Err msg ->
            Debug.todo ("Failed to build test defs: " ++ msg)

normToExpr : Expr -> Result Message Expr
normToExpr e =
    Result.map (\n -> Readback.readBackNormal 0 n) (normWithDefs initDefs e)

normWithDefs : Defs -> Expr -> Result Message Normal
normWithDefs defs e =
    case synth (defsToContext defs) e of
        Err msg ->
            Err msg

        Ok t ->
            let
                v =
                    eval (defsToEnv defs) e
            in
            Ok (Normal { normalType = t, normalValue = v })


addDefs : Defs -> List ( Name, Expr ) -> Result Message Defs -- Take a list of definitions and load them into env
addDefs defs pairs =
    case pairs of
        [] ->                   -- no def left, we are done. Return env as it is
            Ok defs

        ( x, e ) :: more ->     -- evaluate e (value)
            case normWithDefs defs e of
                Err msg ->
                    Err msg

                Ok norm ->
                    addDefs (Dict.insert x norm defs) more


definedNames : Defs -> List Name
definedNames defs =
    Dict.keys defs