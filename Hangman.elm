module Hangman exposing (main)

import String
import Random exposing (Generator, Seed, initialSeed, generate, map)
import List
import Set
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style, disabled)
import Html.App exposing (program)
import Char exposing (fromCode)
import Task exposing (perform)
import Http exposing (get)
import Json.Decode exposing (..)


-- global config


allowedMistakes : Int
allowedMistakes =
    4


minLength : Int
minLength =
    5


{-| a couple of nouns, which are in the pool if fetching online library fails
-}
library : List String
library =
    List.filter
        (\w -> (String.length w) >= minLength)
        [ "chair", "table", "window", "mirror", "supermarket" ]



-- the main program loop


main : Program Never
main =
    Html.App.program
        { view = view
        , update = update
        , init = ( initialGame, fetchLibrary )
        , subscriptions = \_ -> Sub.none
        }


fetchLibrary : Cmd Msg
fetchLibrary =
    let
        decodeLibrary : Decoder (List String)
        decodeLibrary =
            Json.Decode.map
                (List.filter
                    (\w -> (String.length w) >= minLength)
                )
            <|
                at [ "nouns" ] (list string)

        liburl : String
        liburl =
            "https://raw.githubusercontent.com/dariusk/corpora/master/data/words/nouns.json"
    in
        Task.perform
            FetchLibError
            FetchLibSuccess
            (Http.get decodeLibrary liburl)



-- core types and functions


type State
    = Lost { word : String }
    | Win
    | Active
        { guessedCharacters : Set.Set Char
        , mistakesLeft : Int
        , word : String
        }
    | Pregame


type alias Game =
    { state : State
    , words : List String
    }


initialGame : Game
initialGame =
    { state = Pregame
    , words = []
    }


type Msg
    = Guess Char
    | GenerateWord
    | StartWith String
    | FetchLibError Http.Error
    | FetchLibSuccess (List String)


update : Msg -> Game -> ( Game, Cmd Msg )
update action model =
    case action of
        FetchLibError _ ->
            ( { model | words = [ "hallo" ] }, Cmd.none )

        FetchLibSuccess s ->
            let
                model' =
                    { model | words = s }
            in
                ( model', generate StartWith (wordGen model') )

        Guess c ->
            ( step c model, Cmd.none )

        GenerateWord ->
            ( initialGame, generate StartWith (wordGen model) )

        StartWith w ->
            ( startGameWith w model, Cmd.none )


startGameWith : String -> Game -> Game
startGameWith w game =
    { game
        | state =
            Active
                { guessedCharacters = Set.empty
                , mistakesLeft = allowedMistakes
                , word = String.toUpper w
                }
    }


wordGen : Game -> Generator String
wordGen model =
    let
        maxIndex : Int
        maxIndex =
            (List.length model.words) - 1
    in
        Random.map (\index -> get index model.words) (Random.int 0 maxIndex)


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



-- the view


newGame : Html Msg
newGame =
    button [ onClick GenerateWord ] [ text "New game" ]


view : Game -> Html Msg
view game =
    div [ style css ] <|
        case game.state of
            Pregame ->
                []

            Lost s ->
                [ h2 []
                    [ text <|
                        "You lost! The hidden word was \""
                            ++ s.word
                            ++ "\""
                    ]
                , newGame
                ]

            Win ->
                [ h2 [] [ text "You won!" ]
                , newGame
                ]

            Active s ->
                [ h1 [] [ text <| displayString s.guessedCharacters s.word ]
                , text <|
                    "Allowed number of mistakes: "
                        ++ toString s.mistakesLeft
                , br [] []
                , br [] []
                ]
                    ++ (List.map
                            (\char -> buildGuessButton char s.guessedCharacters)
                            chars
                       )
                    ++ [ br [] [], br [] [], newGame ]


buildGuessButton : Char -> Set.Set Char -> Html Msg
buildGuessButton c guessedCharacters =
    let
        alreadyGuessed =
            Set.member c guessedCharacters
    in
        button [ onClick (Guess c), disabled alreadyGuessed ]
            [ text (String.fromChar c) ]


chars : List Char
chars =
    List.map Char.fromCode [65..90]


displayString : Set.Set Char -> String -> String
displayString guessedChars word =
    let
        visualize : Char -> String
        visualize char =
            if Set.member char guessedChars then
                " " ++ String.fromList [ char ] ++ " "
            else
                " _ "
    in
        String.concat <| List.map visualize <| String.toList word


css : List ( String, String )
css =
    [ ( "font-family", "sans-serif" )
    , ( "color", "rgb(3, 33, 73)" )
    , ( "text-align", "center" )
    , ( "background-color", "rgb(200, 220, 200)" )
    ]



-- helper functions


get : Int -> List String -> String
get n list =
    case list of
        [] ->
            ""

        x :: xs ->
            if n == 0 then
                x
            else
                get (n - 1) xs


isSubsetOf : Set.Set comparable -> Set.Set comparable -> Bool
isSubsetOf set1 set2 =
    Set.empty == Set.diff set1 set2
