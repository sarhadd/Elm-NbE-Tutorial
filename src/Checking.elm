-- Check: Verify that an expression fits a known type
-- Synthesis: Infer a type directly from the expression

module Checking exposing (..)

import Types exposing (..)

-- Move to Types.elm?
type alias Ctx =
    Dict Name Ty


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
        -- Rec ...
        -- TODO: Add one for Plus!
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

        -- Ann switches from synthesis -> checking
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
        
        -- Zero ...
        -- Add1 ...

        Add1 n ->
            case expectedTy of
                TNat ->
                    case synth ctx n of
                        Ok TNat ->
                            Ok ()
                        Ok other ->
                            Err ("Add1 should be Nat, but got " ++ Debug.toString other)
                        Err msg ->
                            Err msg
                other ->
                    Err ("Add1 expected Nat, but got " ++ Debug.toString other)

        -- Mode switches from checking -> synthesis
        Mode e t1 ->
            case synth ctx e of
                Ok ty ->
                    if ty == t1 then
                        Ok ()
                    else
                        Err ("Expected " ++ Debug.toString t1 ++ " but got " ++ Debug.toString ty)
                Err msg ->
                    Err msg