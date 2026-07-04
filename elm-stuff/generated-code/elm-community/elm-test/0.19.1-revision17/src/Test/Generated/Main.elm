module Test.Generated.Main exposing (main)

import ExperimentTest
import NaturalTest
import TestMain
import VectorTest

import Test.Reporter.Reporter exposing (Report(..))
import Console.Text exposing (UseColor(..))
import Test.Runner.Node
import Test

main : Test.Runner.Node.TestProgram
main =
    Test.Runner.Node.run
        { runs = 100
        , report = ConsoleReport Monochrome
        , seed = 165158144764754
        , processes = 22
        , globs =
            []
        , paths =
            [ "C:\\Users\\zzm\\Documents\\stabl\\Elm-NbE-Tutorial\\tests\\ExperimentTest.elm"
            , "C:\\Users\\zzm\\Documents\\stabl\\Elm-NbE-Tutorial\\tests\\NaturalTest.elm"
            , "C:\\Users\\zzm\\Documents\\stabl\\Elm-NbE-Tutorial\\tests\\TestMain.elm"
            , "C:\\Users\\zzm\\Documents\\stabl\\Elm-NbE-Tutorial\\tests\\VectorTest.elm"
            ]
        }
        [ ( "ExperimentTest"
          , [ Test.Runner.Node.check ExperimentTest.suite
            ]
          )
        , ( "NaturalTest"
          , [ Test.Runner.Node.check NaturalTest.suite
            ]
          )
        , ( "TestMain"
          , [ Test.Runner.Node.check TestMain.suite
            , Test.Runner.Node.check TestMain.vecSuite
            ]
          )
        , ( "VectorTest"
          , [ Test.Runner.Node.check VectorTest.suite
            ]
          )
        ]