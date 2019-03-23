module Core exposing (fetchLibrary, initialGame, subscriptions, update)

import Browser.Events exposing (..)
import Char exposing (toCode)
import Http exposing (get)
import Json.Decode as Decode exposing (..)
import Random exposing (Generator, Seed, generate, initialSeed, map)
import Set
import String
import Types exposing (..)
import Util exposing (..)


defaultMinWordSize : Int
defaultMinWordSize =
    8


numberOfAllowedMistakes : Int
numberOfAllowedMistakes =
    6


update : Msg -> Game -> ( Game, Cmd Msg )
update action game =
    case action of
        DoNothing ->
            ( game, Cmd.none )

        LibraryFetch (Err e) ->
            ( { game | state = LibraryFetchError e }, Cmd.none )

        LibraryFetch (Ok lib) ->
            let
                newGame =
                    { game | words = List.filter containsOnlyLetters lib }
            in
            ( newGame, generate StartGameWithWord (wordGen newGame) )

        Guess c ->
            let
                newGame =
                    if game.page == Gameboard then
                        step c game

                    else
                        game
            in
            ( newGame, Cmd.none )

        NewGame ->
            ( game, generate StartGameWithWord (wordGen game) )

        StartGameWithWord w ->
            ( startGameWithWord w game, Cmd.none )

        MinWordSizeModify minWordSizeAction ->
            let
                f : Int -> Int
                f x =
                    case minWordSizeAction of
                        Increase ->
                            x + 1

                        Decrease ->
                            x - 1

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
    , minWordSize = defaultMinWordSize
    , page = Gameboard
    }


startGameWithWord : String -> Game -> Game
startGameWithWord w game =
    { game
        | state =
            Active
                { guessedCharacters = Set.empty
                , mistakesLeft = numberOfAllowedMistakes
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
            List.length words - 1
    in
    Random.map
        (\index -> String.toUpper <| Util.get index words)
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

                newState =
                    { guessedCharacters = Set.insert c guessedCharacters
                    , mistakesLeft =
                        if mistakeMade then
                            mistakesLeft - 1

                        else
                            mistakesLeft
                    , word = word
                    }
            in
            { game | state = nextState newState }

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
    Http.send
        LibraryFetch
        (Http.get liburl decodeLibrary)


subscriptions : Game -> Sub Msg
subscriptions game =
    case game.state of
        Active _ ->
            keyboardGuess

        _ ->
            Sub.none


keyDecoder : Decode.Decoder Msg
keyDecoder =
    let
        toGuessMsg : String -> Msg
        toGuessMsg str =
            case String.uncons str of
                Just ( char, _ ) ->
                    let
                        upper =
                            Char.toUpper char
                    in
                    if List.member upper chars then
                        Guess upper

                    else
                        DoNothing

                Nothing ->
                    DoNothing
    in
    Decode.map toGuessMsg <| Decode.field "key" Decode.string


keyboardGuess : Sub Msg
keyboardGuess =
    onKeyPress keyDecoder
