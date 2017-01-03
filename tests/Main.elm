port module Main exposing (main)

import UtilTest exposing (testUtil)
import CoreTest exposing (testCore)
import Test exposing (..)
import Test.Runner.Node exposing (run, TestProgram)
import Json.Encode exposing (Value)


main : TestProgram
main =
    run emit tests


tests : Test
tests =
    describe "Hangman Test Suite"
        [ testUtil
        , testCore
        ]


port emit : ( String, Value ) -> Cmd msg
