-- Value, Neutral, Normal types.
module Types exposing (..)

import Natural exposing (Natural)
import Dict exposing (Dict)


-- ── Shared primitives ─────────────────────────────────────────────────────


type alias Name =
    String


type alias Env v =
    Dict Name v


type alias Context =  -- used in Test.elm
    Env Ty


type alias Defs =     -- used in Test.elm
    Env Normal

type alias Message =
    String


-- ── Type language (§4) ────────────────────────────────────────────────────


type Ty
    = TNat        -- natural type
    | TFlt        -- float type
    | TArr Ty Ty  -- function type (t₁ → t₂)


-- ── Expression syntax ─────────────────────────────────────────────────────
-- Defined here to avoid a circular import: Value references Expr (closures),
-- and Expr references Ty. Move to Expr.elm once boundaries stabilise.


type Expr
    = Var Name
    | Lambda Name Expr
    | App Expr Expr
    | Plus Expr Expr   -- replaces Rec
    | Nat Natural      -- replaces Zero/Add1
    | Flt Float        -- floating-point literal
    | Ann Expr Ty


-- ── Semantic domain: Values, Neutrals, Normals (§5) ───────────────────────

type Value
    = VNat Natural
    | VFloat Float
    | VClosure (Env Value) Name Expr
    | VNeutral Ty Neutral


-- A Neutral is an eliminator stuck on a free variable.
type Neutral
    = NVar Name
    | NApp Neutral Normal
    -- NRec omitted: arithmetic is handled directly via NPlus.
    | NPlus Neutral Normal  -- stuck plus: neutral left operand, normal right


-- A Normal pairs a value with its type for typed read-back.
type Normal
    = Normal
        { normalType  : Ty
        , normalValue : Value
        }


-- Normal accessors

normalType : Normal -> Ty
normalType (Normal r) =
    r.normalType

normalValue : Normal -> Value
normalValue (Normal r) =
    r.normalValue


-- Error Types

type alias Error =
    String

type alias ResultTy =
    Result Error Ty