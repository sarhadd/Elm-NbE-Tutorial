-- Value, Neutral, Normal types.
module Types exposing (...)

import Natural exposing (Natural)

type Ty
    = TNat        -- natural type
    | TArr Ty Ty  -- function type (t₁ → t₂)

type Value
    = VNat Natural   -- Replaces VZero and VAdd1 Value
    | VClousre (Env Value) Name Expr
    | VNeutral Ty Neutral

type Natural
    = NVar Name
    | NApp Neutral Normal
    -- | NRec Ty Neutral Normal Normal
    | NPlus Neutral Nomral -- This is a stuck Plus (Neutral left operand)

type Normal
    = Normal { normalType  : Ty,
               normalValue : Value }