module View exposing (view)

import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style, disabled, href, target)
import Set
import String exposing (..)
import Types exposing (..)
import Util exposing (chars)


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
            LibraryFetchError e ->
                [ h2 []
                    [ text "Could not fetch remote word list from "
                    , linkToWordLibrary
                    , text ". "
                    , text <| toString e
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
