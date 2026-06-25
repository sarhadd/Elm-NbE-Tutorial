eval : Env Value -> Expr -> Value
eval env expr =
    case expr of
        Var x ->
            case lookupVar env x of
                Left msg -> error ("Internal error: " ++ show msg)
                Right v -> v

        Lambda x body ->
            VClosure env x body

        App rator rand ->
            doApply (eval env rator) (eval env rand)

        -- Removed Zero and Add1, replaced with:
        NatLit n ->
            VNat n

        -- Rec t tgt base step ->
        --     doRec t (eval env tgt) (eval env base) (eval env step)

        -- TODO: add Plus case!!

        Ann e t ->
            eval env e

-- With insiration of Lucas's getEnv function:
lookupVar : Env -> Name -> Result ErrMsg Value
lookupVar env x =
  case Dict.get x env of
    Just v -> Ok v
    Nothing -> failure <| "Not found: Couldn't get var " ++ x ++ " from evaluation env."
