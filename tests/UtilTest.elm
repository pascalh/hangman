module UtilTest exposing (testUtil)

import Expect exposing (..)
import Fuzz exposing (..)
import Set
import Test exposing (..)
import Util exposing (containsOnlyLetters, get, isSubsetOf)


testUtil : Test
testUtil =
    describe "Util Test Suite"
        [ testsGet
        , testsContainsOnlyLetters
        , testIsSubsetOf
        ]



--Tests for get


testsGet : Test
testsGet =
    describe "Index-based access of lists"
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
                    List.range 0 (List.length list |> (\x -> x - 1))
            in
            Expect.equalLists list <|
                List.map (\i -> Util.get i list) indexes


propGetNegativeIndex : Test
propGetNegativeIndex =
    let
        negativeInt : Fuzzer Int
        negativeInt =
            Fuzz.map (\x -> min -1 <| negate <| abs x) int
    in
    fuzz (tuple ( list string, negativeInt ))
        "Get returns empty string for negative index"
    <|
        \( list, index ) -> equal (get index list) ""



-- Tests for isSubsetOf


testIsSubsetOf : Test
testIsSubsetOf =
    describe "isSubsetOf"
        [ testSubsetOf [] [] True
        , testSubsetOf [] [ 1, 2 ] True
        , testSubsetOf [ 1 ] [] False
        , testSubsetOf [ 3, 1 ] [ 1, 3, 2 ] True
        , testSubsetOf [ 3, 1 ] [ 1, 2 ] False
        ]


testSubsetOf : List Int -> List Int -> Bool -> Test
testSubsetOf sub super expected =
    let
        subset =
            Set.fromList sub

        superset =
            Set.fromList super

        listToString xs =
            String.join "," <| List.map String.fromInt xs

        description =
            "check if " ++ listToString sub ++ "is a subset of " ++ listToString super
    in
    test description <|
        \() ->
            equal expected <| isSubsetOf subset superset



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
