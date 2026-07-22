# Example: Add boolean literal support

## Goal

Add a small feature that lets the language represent boolean literals.

## Code snippet

```elm
-- in src/Types.elm
    | Nat Natural
    | IntLit Integer
    | Flt Float
    | BoolLit Bool
```

## What to do

- Create a new git branch before making changes.
- Add `BoolLit Bool` to `Expr` in `src/Types.elm`
- Add evaluation for `BoolLit` in `src/Eval.elm`
- Add type synthesis for `BoolLit` in `src/Checking.elm`
- Add readback support for `BoolLit` in `src/Readback.elm`

## Expected behavior

- `BoolLit` should evaluate to a new boolean `Value` variant
- `BoolLit` should synthesize to a new `TBool` type
- `BoolLit` should round-trip through readback

## Verify

Run the Elm compiler checks:

```bash
cd c:\Users\dinas\OneDrive\Documents\GitHub\Elm-NbE-Tutorial
elm make src/Types.elm --output /dev/null
elm make src/Eval.elm --output /dev/null
elm make src/Checking.elm --output /dev/null
elm make src/Readback.elm --output /dev/null
```
