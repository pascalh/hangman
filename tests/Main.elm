port module Main exposing (main)

import UtilTest exposing (testUtil)
import Test.Runner.Node exposing (run, TestProgram)
import Json.Encode exposing (Value)


main : TestProgram
main =
    run emit testUtil


port emit : ( String, Value ) -> Cmd msg
