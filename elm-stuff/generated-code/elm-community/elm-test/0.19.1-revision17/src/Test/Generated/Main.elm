module Test.Generated.Main exposing (main)

import Example
import ExperimentTest
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
        , report = ConsoleReport Monochrome
        , seed = 337779823637829
        , processes = 22
        , globs =
            []
        , paths =
            [ "C:\\Users\\zzm\\Documents\\stabl\\Elm-NbE-Tutorial\\tests\\Example.elm"
            , "C:\\Users\\zzm\\Documents\\stabl\\Elm-NbE-Tutorial\\tests\\ExperimentTest.elm"
            , "C:\\Users\\zzm\\Documents\\stabl\\Elm-NbE-Tutorial\\tests\\NaturalTest.elm"
            , "C:\\Users\\zzm\\Documents\\stabl\\Elm-NbE-Tutorial\\tests\\TestMain.elm"
            ]
        }
        [ ( "Example"
          , [ Test.Runner.Node.check Example.placeholder
            ]
          )
        , ( "ExperimentTest"
          , [ Test.Runner.Node.check ExperimentTest.suite
            ]
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