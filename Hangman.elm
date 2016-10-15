module Hangman exposing (main)

import String
import Random exposing (Generator, Seed, initialSeed, generate, map)
import List
import Set
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style, disabled, href, target)
import Html.App exposing (program)
import Char exposing (fromCode)
import Task exposing (perform)
import Http exposing (get)
import Json.Decode exposing (..)


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


initialGame : Game
initialGame =
    { state = Pregame
    , words = []
    , minWordSize = 8
    , page = Gameboard
    }


type Msg
    = Guess Char
    | NewGame
    | StartGameWithWord String
    | FetchLibError Http.Error
    | FetchLibSuccess (List String)
    | OpenPage Page
    | MinWordSizeModify (Int -> Int)


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



-- the view


view : Game -> Html Msg
view game =
    case game.page of
        Options ->
            viewOptions game

        Gameboard ->
            viewGameboard game


newGame : Html Msg
newGame =
    div [ style [ ( "padding-top", "50px" ) ] ]
        [ button [ onClick NewGame ] [ text "New game" ]
        ]


options : Html Msg
options =
    div [ style [] ]
        [ button [ onClick (OpenPage Options) ] [ text "Options" ]
        ]


viewGameboard : Game -> Html Msg
viewGameboard game =
    div
        [ style
            (( "margin-left", "50px" )
                :: ( "margin-right", "50px" )
                :: css
            )
        ]
    <|
        case game.state of
            LibraryFetchError ->
                [ h2 []
                    [ text "Could not fetch remote word list from "
                    , linkToWordLibrary
                    , text ". Check your internet connection."
                    ]
                ]

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
                    "Allowed number of mistakes left: "
                        ++ toString s.mistakesLeft
                , br [] []
                , br [] []
                ]
                    ++ (List.map
                            (\char -> buildGuessButton char s.guessedCharacters)
                            chars
                       )
                    ++ [ br [] []
                       , br [] []
                       , newGame
                       , options
                       , br [] []
                       , div []
                            [ text <| "The words are randomly selected from a "
                            , linkToWordLibrary
                            , text "."
                            ]
                       ]


linkToWordLibrary : Html a
linkToWordLibrary =
    a
        [ href "https://github.com/dariusk/corpora/blob/master/data/words/nouns.json"
        , target "_blank"
        , style [ ( "color", "rgb(3, 33, 73)" ) ]
        ]
        [ text "dictionary" ]


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


viewOptions : Game -> Html Msg
viewOptions game =
    div [ style css ]
        [ text "Select the minimum length of words:"
        , div []
            [ button [ onClick (MinWordSizeModify (\x -> x - 1)) ] [ text "-" ]
            , text <| toString <| game.minWordSize
            , button [ onClick (MinWordSizeModify (\x -> x + 1)) ] [ text "+" ]
            ]
        , br [] []
        , button [ onClick (OpenPage Gameboard) ] [ text "back" ]
        ]


css : List ( String, String )
css =
    [ ( "font-family", "sans-serif" )
    , ( "color", "rgb(3, 33, 73)" )
    , ( "text-align", "center" )
    , ( "background-color", "rgb(200, 220, 200)" )
    , ( "padding-top", "70px" )
    , ( "padding-bottom", "50px" )
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


containsOnlyLetters : String -> Bool
containsOnlyLetters str =
    String.all (\c -> List.member (Char.toUpper c) chars) str
