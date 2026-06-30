-- module Example exposing (..)

-- import Expect exposing (Expectation)
-- -- import Fuzz exposing (Fuzzer, int, list, string)
-- -- import Test exposing (..)
-- import TestMain exposing (..)

module Example exposing (..)

-- whatever you import here depends on what TestMain.elm actually contains
-- e.g. if it defines test1/test2/test3, it probably needs:
import Types exposing (..)
import Eval exposing (..)
import Checking exposing (..)
import Natural

suite : Test
suite =
    --todo "Implement our first test. See https://package.elm-lang.org/packages/elm-explorations/test/latest for how to do this!"
    describe "normWithTestDefs"
        [ test "Looking up + by itself stays as a lambda" <|
                \_ ->
                    case test1 of
                        Ok (Lambda _ (Lambda _ (Plus _ _))) ->
                            Expect.pass
                        other ->
                            Expect.fail ("Unexpected result: " ++ Debug.toString other)
                            
            , test "+ applied to three reduces to a partial lambda" <|
                \_ ->
                    case test2 of
                        Ok (Lambda _ (Plus (Lit n) (Var _))) ->
                            Expect.equal (Natural.toInt n) 3

                        other ->
                            Expect.fail ("Unexpected result: " ++ Debug.toString other)

            -- fuzz runs the test 100 times with randomly-generated inputs!
            -- , fuzz string  " ... " <|
            , test "+ applied to three and two fully reduces to five" <|
                \_ ->
                    case test3 of
                        Ok (Lit n) ->
                            Expect.equal (Natural.toInt n) 5

                        other ->
                            Expect.fail ("Unexpected result: " ++ Debug.toString other)
 
        ]