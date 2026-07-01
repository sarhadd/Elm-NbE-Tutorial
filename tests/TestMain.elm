module TestMain exposing (..)

import Dict
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


addDefs : Defs -> List ( Name, Expr ) -> Result Message Defs
addDefs defs pairs =
    case pairs of
        [] ->
            Ok defs

        ( x, e ) :: more ->
            case normWithDefs defs e of
                Err msg ->
                    Err msg

                Ok norm ->
                    addDefs (Dict.insert x norm defs) more


definedNames : Defs -> List Name
definedNames defs =
    Dict.keys defs