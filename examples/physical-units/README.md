# Example: Add physical units support

## Goal

Add a small feature that lets the language track a basic unit for numeric values.

## Code snippet

```elm
-- in src/Types.elm
    | IntLit Integer
    | Flt Float
    | UnitLit String Float
```

## What to do

- Create a new git branch before making changes.
- Add `UnitLit String Float` to `Expr` in `src/Types.elm`
- Add a new `TUnit String` type to `src/Types.elm`
- Add evaluation for `UnitLit` in `src/Eval.elm`
- Add type synthesis for `UnitLit` in `src/Checking.elm`
- Add readback support for `UnitLit` in `src/Readback.elm`

## Expected behavior

- `UnitLit "m" 3.0` should evaluate to a unit value
- `UnitLit` should synthesize to `TUnit "m"` or similar
- Readback should preserve the unit literal

## Verify

Run the Elm compiler checks:

```bash
cd c:\Users\dinas\OneDrive\Documents\GitHub\Elm-NbE-Tutorial
elm make src/Types.elm --output /dev/null
elm make src/Eval.elm --output /dev/null
elm make src/Checking.elm --output /dev/null
elm make src/Readback.elm --output /dev/null
```
