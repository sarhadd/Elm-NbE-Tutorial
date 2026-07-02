module Test.Generated.Main exposing (main)

import TestMain

import Test.Reporter.Reporter exposing (Report(..))
import Console.Text exposing (UseColor(..))
import Test.Runner.Node
import Test

main : Test.Runner.Node.TestProgram
main =
    Test.Runner.Node.run
        { runs = 100
        , report = ConsoleReport UseColor
        , seed = 91780436811699
        , processes = 24
        , globs =
            [ "tests/TestMain.elm"
            ]
        , paths =
            [ "C:\\Users\\dinas\\OneDrive\\Documents\\GitHub\\Elm-NbE-Tutorial\\tests\\TestMain.elm"
            ]
        }
        [ ( "TestMain"
          , [ Test.Runner.Node.check TestMain.suite
            ]
          )
        ]