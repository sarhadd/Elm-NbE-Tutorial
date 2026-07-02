module NatBenchmark exposing (main)

{-| Rigorous timing comparison: PeanoNat (unary) vs elm-natural (base-2^26).

This is NOT an elm-test suite. elm-test can only assert pass/fail on pure
Expectation values, it has no access to a timer and can't warm up or
resample — see ExperimentTest.elm's "elm-natural only" group, which proves
correctness at scale but never measures speed.

`elm-explorations/benchmark` runs each function thousands of times inside a
requestAnimationFrame loop until the estimate is statistically stable, so it
needs a browser, not a console. Build and open it with:

    elm reactor
    -- then open http://localhost:8000/benchmarks/NatBenchmark.elm

or

    elm make benchmarks/NatBenchmark.elm --output=benchmark.html
    -- then open benchmark.html directly

-}

import Benchmark exposing (Benchmark)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Natural
import PeanoNat


main : BenchmarkProgram
main =
    program suite


suite : Benchmark
suite =
    Benchmark.describe "PeanoNat (unary) vs elm-natural (base-2^26)"
        [ constructionSuite
        , addSuite
        , mulSuite
        ]


{-| fromSafeInt n: PeanoNat allocates n Add1 constructors, elm-natural does not.
-}
constructionSuite : Benchmark
constructionSuite =
    Benchmark.compare "fromSafeInt 2000"
        "PeanoNat.fromSafeInt"
        (\_ -> PeanoNat.fromSafeInt 2000)
        "Natural.fromSafeInt"
        (\_ -> Natural.fromSafeInt 2000)


{-| add is O(n) for PeanoNat (recurse on the first argument's Add1 chain),
O(log n) for elm-natural.
-}
addSuite : Benchmark
addSuite =
    let
        peanoA =
            PeanoNat.fromSafeInt 500

        peanoB =
            PeanoNat.fromSafeInt 500

        natA =
            Natural.fromSafeInt 500

        natB =
            Natural.fromSafeInt 500
    in
    Benchmark.compare "add: 500 + 500"
        "PeanoNat.add"
        (\_ -> PeanoNat.add peanoA peanoB)
        "Natural.add"
        (\_ -> Natural.add natA natB)


{-| mul is O(n^2) for PeanoNat (n additions, each O(n)), O(log n) for elm-natural.
Kept small (80) since PeanoNat's quadratic cost is the whole point being measured.
-}
mulSuite : Benchmark
mulSuite =
    let
        peanoA =
            PeanoNat.fromSafeInt 80

        peanoB =
            PeanoNat.fromSafeInt 80

        natA =
            Natural.fromSafeInt 80

        natB =
            Natural.fromSafeInt 80
    in
    Benchmark.compare "mul: 80 * 80"
        "PeanoNat.mul"
        (\_ -> PeanoNat.mul peanoA peanoB)
        "Natural.mul"
        (\_ -> Natural.mul natA natB)
