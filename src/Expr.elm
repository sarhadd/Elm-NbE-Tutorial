module Expr exposing (...)

type alias Name = String

type Expr
    = Var Name
    | Lambda Name Expr
    | App Expr Expr
    | Literal Natural   -- replaces Zero/Add1
    -- | Rec Ty Expr Expr Expr
    | Plus Expr Expr    -- replaces Rec
    | Ann Expr Ty