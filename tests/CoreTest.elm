module CoreTest exposing (testCore)

import Core exposing (update)
import Expect exposing (..)
import Fuzz exposing (..)
import Set
import Test exposing (..)
import Tuple exposing (first)
import Types exposing (..)
import Util exposing (chars)


testCore : Test
testCore =
    describe "Game logic"
        [ testWin
        , propWin
        , testLose
        , testGameover
        , testPage
        ]


testWin : Test
testWin =
    test "Win game after guessing all characters of word \"abc\"" <|
        \() ->
            let
                result =
                    run <|
                        StartGameWithWord "abc"
                            :: guess "bacc"
            in
            equal result.state Win


propWin : Test
propWin =
    fuzz wordFuzzer "Win game after guessing all characters" <|
        \word ->
            let
                result =
                    run <|
                        StartGameWithWord word
                            :: guess word
            in
            if String.isEmpty word then
                pass

            else
                equal result.state Win


testLose : Test
testLose =
    test "Lose game after too many mistakes" <|
        \() ->
            (run <|
                StartGameWithWord
                    "a"
                    :: guess "bcdefgh"
            ).state
                |> Expect.equal (Lost { word = "a" })


testGameover : Test
testGameover =
    test "Game is over after losing" <|
        \() ->
            (run <|
                StartGameWithWord "a"
                    :: guess "bcdefgha"
            ).state
                |> Expect.equal (Lost { word = "a" })


testPage : Test
testPage =
    test "Guessing characters only possible on game page" <|
        \() ->
            let
                expected =
                    Active
                        { guessedCharacters = Set.empty
                        , mistakesLeft = defaultMinWordSize
                        , word = "a"
                        }
            in
            (run
                [ StartGameWithWord "a"
                , OpenPage Options
                , Guess 'a'
                ]
            ).state
                |> Expect.equal expected



-- Util functions


run : List Msg -> Game
run =
    List.foldl (\msg game -> Tuple.first (update msg game)) initialGame



-- initial game config


defaultMinWordSize : Int
defaultMinWordSize =
    6


initialGame : Game
initialGame =
    { state = Pregame
    , words = []
    , minWordSize = defaultMinWordSize
    , page = Gameboard
    }



-- guess the characters in given string from left to right


guess : String -> List Msg
guess =
    List.foldr
        (\c acc -> Guess c :: acc)
        []
        << String.toList



-- creates strings containing letters only


wordFuzzer : Fuzzer String
wordFuzzer =
    Fuzz.map String.fromList <| list (elements chars)



-- returns every element of the list with equal probability


elements : List a -> Fuzzer a
elements =
    frequency << List.map (\x -> ( 1, constant x ))
