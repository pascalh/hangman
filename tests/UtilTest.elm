module UtilTest exposing (testUtil)

import Test exposing (..)
import Expect exposing (equal)
import Fuzz exposing (list, int, tuple, string, Fuzzer)
import Util exposing (get)


testUtil : Test
testUtil =
    describe "Util Test Suite"
        [ describe "Get"
            [ propGetWorks
            , propGetEmptyList
            , propGetNegativeIndex
            ]
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
