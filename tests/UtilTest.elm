module UtilTest exposing (testUtil)

import Test exposing (..)
import Expect exposing (..)
import Fuzz exposing (..)
import Util exposing (get, containsOnlyLetters)


testUtil : Test
testUtil =
    describe "Util Test Suite"
        [ testsGet
        , testsContainsOnlyLetters
        ]



--Tests for get


testsGet : Test
testsGet =
    describe "Get"
        [ propGetWorks
        , propGetEmptyList
        , propGetNegativeIndex
        ]


propGetEmptyList : Test
propGetEmptyList =
    fuzz int "Get returns empty string for empty list" <|
        \index ->
            equal (get index []) ""


propGetWorks : Test
propGetWorks =
    fuzz (list string) "Get" <|
        \list ->
            let
                indexes : List Int
                indexes =
                    List.range 0 (List.length list |> \x -> x - 1)
            in
                Expect.equalLists list <|
                    List.map (\i -> Util.get i list) indexes


propGetNegativeIndex : Test
propGetNegativeIndex =
    let
        negativeInt : Fuzzer Int
        negativeInt =
            Fuzz.map (\x -> min (-1) <| negate <| abs x) int
    in
        fuzz (tuple ( list string, negativeInt ))
            "Get returns empty string for negative index"
        <|
            \( list, index ) -> equal (get index list) ""



-- Tests for containsOnlyLetters


testsContainsOnlyLetters : Test
testsContainsOnlyLetters =
    describe "containsOnlyLetters"
        [ containsTest "" True
        , containsTest "abc1" False
        , containsTest "a1a" False
        , containsTest "1aa" False
        , containsTest "a a" False
        , containsTest "abc" True
        , containsTest "Abc" True
        ]


containsTest : String -> Bool -> Test
containsTest word expected =
    let
        description =
            "word under test \"" ++ word ++ "\""
    in
        test description <|
            \() ->
                equal expected (containsOnlyLetters word)
