module Hangman exposing (main)

import Types exposing (Msg, Game)
import View exposing (view)
import Core exposing (update, initialGame, fetchLibrary)
import Html exposing (program)


main : Program Never Game Msg
main =
    Html.program
        { view = view
        , update = update
        , init = ( initialGame, fetchLibrary )
        , subscriptions = \_ -> Sub.none
        }
