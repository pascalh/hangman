module Util exposing (..)

import Set
import String
import Char exposing (..)


get : Int -> List String -> String
get n list =
    if n < 0 then
        ""
    else
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
    String.all (\c -> List.member (toUpper c) chars) str


chars : List Char
chars =
    List.map fromCode <| List.range 65 90
