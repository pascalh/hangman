module Hangman exposing (main)

import Types exposing (..)
import View exposing (view)
import Core exposing (update, initialGame, fetchLibrary)
import Html.App exposing (program)


main : Program Never
main =
    Html.App.program
        { view = view
        , update = update
        , init = ( initialGame, fetchLibrary )
        , subscriptions = \_ -> Sub.none
        }
