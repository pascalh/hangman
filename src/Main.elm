module Hangman exposing (main)

import Browser exposing (document)
import Core exposing (fetchLibrary, initialGame, subscriptions, update)
import Types exposing (Game, Msg)
import View exposing (view)


main : Program () Game Msg
main =
    Browser.element
        { view = view
        , update = update
        , init = \() -> ( initialGame, fetchLibrary )
        , subscriptions = subscriptions
        }
