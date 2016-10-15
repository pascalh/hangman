module Types exposing (..)

import Http
import Set


type State
    = Lost { word : String }
    | Win
    | Active
        { guessedCharacters : Set.Set Char
        , mistakesLeft : Int
        , word : String
        }
    | Pregame
    | LibraryFetchError


type Page
    = Gameboard
    | Options


type alias Game =
    { state : State
    , words : List String
    , minWordSize : Int
    , page : Page
    }


type Msg
    = Guess Char
    | NewGame
    | StartGameWithWord String
    | FetchLibError Http.Error
    | FetchLibSuccess (List String)
    | OpenPage Page
    | MinWordSizeModify (Int -> Int)
