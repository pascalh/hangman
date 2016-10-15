module Core exposing (update, initialGame)

import Random exposing (Generator, Seed, initialSeed, generate, map)
import String
import Set
import Task exposing (perform)
import Http exposing (get)
import Json.Decode exposing (..)
import Types exposing (..)
import Util exposing (..)


update : Msg -> Game -> ( Game, Cmd Msg )
update action game =
    case action of
        FetchLibError _ ->
            ( { game | state = LibraryFetchError }, Cmd.none )

        FetchLibSuccess lib ->
            let
                game' =
                    { game | words = List.filter containsOnlyLetters lib }
            in
                ( game', generate StartGameWithWord (wordGen game') )

        Guess c ->
            ( step c game, Cmd.none )

        NewGame ->
            ( game, generate StartGameWithWord (wordGen game) )

        StartGameWithWord w ->
            ( startGameWithWord w game, Cmd.none )

        MinWordSizeModify f ->
            let
                newMinWordSize : Int
                newMinWordSize =
                    f <| game.minWordSize
            in
                ( { game | minWordSize = newMinWordSize }
                , Cmd.none
                )

        OpenPage p ->
            ( { game | page = p }, Cmd.none )


initialGame : Game
initialGame =
    { state = Pregame
    , words = []
    , minWordSize = 8
    , page = Gameboard
    }


startGameWithWord : String -> Game -> Game
startGameWithWord w game =
    { game
        | state =
            Active
                { guessedCharacters = Set.empty
                , mistakesLeft = 6
                , word = w
                }
    }


wordGen : Game -> Generator String
wordGen game =
    let
        words : List String
        words =
            List.filter
                (\w -> String.length w >= game.minWordSize)
                game.words

        maxIndex : Int
        maxIndex =
            (List.length words) - 1
    in
        Random.map
            (\index -> String.toUpper <| get index words)
            (Random.int 0 maxIndex)


step : Char -> Game -> Game
step c game =
    case game.state of
        Active { guessedCharacters, mistakesLeft, word } ->
            let
                mistakeMade =
                    not <|
                        Set.member c guessedCharacters
                            || List.member c (String.toList word)

                state' =
                    { guessedCharacters = Set.insert c guessedCharacters
                    , mistakesLeft =
                        if mistakeMade then
                            mistakesLeft - 1
                        else
                            mistakesLeft
                    , word = word
                    }
            in
                { game | state = nextState state' }

        _ ->
            game


nextState :
    { guessedCharacters : Set.Set Char
    , mistakesLeft : Int
    , word : String
    }
    -> State
nextState s =
    let
        isLost =
            s.mistakesLeft < 0

        isWon =
            isSubsetOf (Set.fromList <| String.toList s.word)
                s.guessedCharacters
    in
        if isLost then
            Lost { word = s.word }
        else if isWon then
            Win
        else
            Active s


fetchLibrary : Cmd Msg
fetchLibrary =
    let
        decodeLibrary : Decoder (List String)
        decodeLibrary =
            at [ "nouns" ] (list string)

        liburl : String
        liburl =
            "https://raw.githubusercontent.com/dariusk/corpora/master/data/words/nouns.json"
    in
        Task.perform
            FetchLibError
            FetchLibSuccess
            (Http.get decodeLibrary liburl)
