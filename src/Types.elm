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
    = TNat           -- natural type
    | TFlt           -- float type
    | TVec Expr Ty   -- vector type (Expr has to be Nat)
    | TArr Ty Ty     -- function type (t₁ → t₂)

-- Note: Type of Vec.map: TArr (TArr TFlt TNat) -> TVec n TFlt -> TVec n TNat 
--       We can't express this with the four constructors we have and would need a constructor for the type index
--       First n is binding, next n is subsitution

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
    | VecLit (List Expr)  -- vector literal; length is List.length
    | VecAdd Expr Expr    -- element-wise addition (Nat or Flt elements)
    | Dot Expr Expr       -- dot product, Vec n Flt -> Vec n Flt -> Flt
    | Scale Expr Expr     -- scalar * vector, Flt -> Vec n Flt -> Vec n Flt


-- ── Semantic domain: Values, Neutrals, Normals (§5) ───────────────────────

type Value
    = VNat Natural
    | VFloat Float
    | VClosure (Env Value) Name Expr
    | VNeutral Ty Neutral
    | VVec (List Value)


-- A Neutral is an eliminator stuck on a free variable.
type Neutral    -- either a neutral or a neutral applied to a value
    = NVar Name
    | NApp Neutral Normal
    -- NRec omitted: arithmetic is handled directly via NPlus.
    | NPlus Neutral Normal  -- stuck plus: neutral left operand, normal right
    | NVecAdd Neutral Normal  -- mirrors NPlus's convention
    | NDot Neutral Normal
    | NScale Normal Neutral  -- scalar is normal, vector operand is the stuck one


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