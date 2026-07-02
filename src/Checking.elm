-- Check: Verify that an expression fits a known type
-- Synthesis: Infer a type directly from the expression

module Checking exposing (..)

import Dict exposing (Dict)
import Types exposing (..)


type alias Ctx =
    Dict Name Ty


-- ── Synthesis (↑): push a type OUT of the expression ─────────────────────────


synth : Ctx -> Expr -> ResultTy
synth ctx expr =
    case expr of
        Var x ->
            lookupVar ctx x

        App rator rand ->
            case synth ctx rator of
                Ok ty ->
                    case ty of
                        TArr argT retT ->
                            case check ctx rand argT of
                                Ok _ ->
                                    Ok retT
                                Err msg ->
                                    Err msg
                        other ->
                            Err ("Not a function type: " ++ Debug.toString other)
                Err msg ->
                    Err msg

        Plus l r ->
            case synth ctx l of
                Ok TNat ->
                    case synth ctx r of
                        Ok TNat ->
                            Ok TNat
                        Ok other ->
                            Err ("Plus expected Nat on the right, but got " ++ Debug.toString other)
                        Err msg ->
                            Err msg
                Ok other ->
                    Err ("Plus expected Nat on the left, but got " ++ Debug.toString other)
                Err msg ->
                    Err msg

        Nat _ ->
            Ok TNat

        Flt _ ->
            Ok TFlt

        -- Ann switches from checking → synthesis
        Ann e t ->
            case check ctx e t of
                Ok _ ->
                    Ok t
                Err msg ->
                    Err msg

        other ->
            Err
                ("Can't find a type for "
                    ++ Debug.toString other
                    ++ ". Try adding a type annotation."
                )


-- ── Checking (↓): push a type IN to the expression ───────────────────────────


check : Ctx -> Expr -> Ty -> Result String ()
check ctx expr expectedTy =
    case expr of
        Lambda x body ->
            case expectedTy of
                TArr argT retT ->
                    let
                        ctxNew =
                            Dict.insert x argT ctx
                    in
                    check ctxNew body retT
                other ->
                    Err ("Lambda requires a function type, but got " ++ Debug.toString other)

        -- Mode switch: synthesize and compare
        _ ->
            case synth ctx expr of
                Ok ty ->
                    if ty == expectedTy then
                        Ok ()
                    else
                        Err ("Expected " ++ Debug.toString expectedTy ++ " but got " ++ Debug.toString ty)
                Err msg ->
                    Err msg


-- ── Helpers ───────────────────────────────────────────────────────────────────


lookupVar : Ctx -> Name -> ResultTy
lookupVar ctx x =
    case Dict.get x ctx of
        Just ty ->
            Ok ty
        Nothing ->
            Err ("Unbound variable: " ++ x)
