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
    | LibraryFetchError Http.Error


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
    | LibraryFetch (Result Http.Error (List String))
    | OpenPage Page
    | MinWordSizeModify (Int -> Int)
    | DoNothing
