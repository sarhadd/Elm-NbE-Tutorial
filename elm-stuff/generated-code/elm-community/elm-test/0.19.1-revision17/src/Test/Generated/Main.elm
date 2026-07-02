module Test.Generated.Main exposing (main)

import VectorTest

import Test.Reporter.Reporter exposing (Report(..))
import Console.Text exposing (UseColor(..))
import Test.Runner.Node
import Test

main : Test.Runner.Node.TestProgram
main =
    Test.Runner.Node.run
        { runs = 100
        , report = ConsoleReport UseColor
        , seed = 296212711616802
        , processes = 24
        , globs =
            [ "tests/VectorTest.elm"
            ]
        , paths =
            [ "C:\\Users\\dinas\\OneDrive\\Documents\\GitHub\\Elm-NbE-Tutorial\\tests\\VectorTest.elm"
            ]
        }
        [ ( "VectorTest"
          , [ Test.Runner.Node.check VectorTest.suite
            ]
          )
        ]