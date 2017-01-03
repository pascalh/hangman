module CoreTest exposing (testCore)

import Test exposing (..)
import Tuple exposing (first)
import Expect exposing (..)
import Fuzz exposing (..)
import Core exposing (update, initialGame)
import Util exposing (chars)
import Types exposing (..)


testCore : Test
testCore =
    describe "Game logic"
        [ testWin
        , propWin
        , testLose
        , testGameover
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



-- Util functions


run : List Msg -> Game
run =
    List.foldl (\msg game -> Tuple.first (update msg game)) initialGame



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
    frequencyOrCrash << List.map (\x -> ( 1, constant x ))
