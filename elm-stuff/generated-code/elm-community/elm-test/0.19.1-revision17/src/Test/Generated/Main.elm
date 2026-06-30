module Test.Generated.Main exposing (main)

import Example
import NaturalTest
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
        , seed = 51911610811099
        , processes = 24
        , globs =
            []
        , paths =
            [ "C:\\Users\\dinas\\OneDrive\\Documents\\GitHub\\Elm-NbE-Tutorial\\tests\\Example.elm"
            , "C:\\Users\\dinas\\OneDrive\\Documents\\GitHub\\Elm-NbE-Tutorial\\tests\\NaturalTest.elm"
            , "C:\\Users\\dinas\\OneDrive\\Documents\\GitHub\\Elm-NbE-Tutorial\\tests\\TestMain.elm"
            ]
        }
        [ ( "Example"
          , []
          )
        , ( "NaturalTest"
          , [ Test.Runner.Node.check NaturalTest.suite
            ]
          )
        , ( "TestMain"
          , [ Test.Runner.Node.check TestMain.noDefs
            ]
          )
        ]