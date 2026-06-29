-- Value, Neutral, Normal types.
module Types exposing (..)

import Natural exposing (Natural)


-- ── Shared primitives ─────────────────────────────────────────────────────


type alias Name =
    String


type alias Env a =
    List ( Name, a )


-- ── Type language (§4) ────────────────────────────────────────────────────


type Ty
    = TNat        -- natural type
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
    | Ann Expr Ty


-- ── Semantic domain: Values, Neutrals, Normals (§5) ───────────────────────

type Value
    = VNat Natural
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


-- Error Types

type alias Error =
    String

type alias ResultTy =
    Result Error Ty