module Test.Generated.Main exposing (main)

import NaturalTest

import Test.Reporter.Reporter exposing (Report(..))
import Console.Text exposing (UseColor(..))
import Test.Runner.Node
import Test

main : Test.Runner.Node.TestProgram
main =
    Test.Runner.Node.run
        { runs = 100
        , report = ConsoleReport Monochrome
        , seed = 261460878076222
        , processes = 22
        , globs =
            [ "tests/NaturalTest.elm"
            ]
        , paths =
            [ "C:\\Users\\zzm\\Documents\\Summer2026\\Elm-NbE-Tutorial\\tests\\NaturalTest.elm"
            ]
        }
        [ ( "NaturalTest"
          , [ Test.Runner.Node.check NaturalTest.suite
            ]
          )
        ]