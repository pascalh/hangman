module Hangman where
-- use a random wikipedia entry as word
-- improve visualization

import String
import Window
import Random exposing (Generator,Seed,initialSeed,generate)
import List
import Set
import Graphics.Input exposing (dropDown)

import Graphics.Element exposing (..)
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (disabled)
import StartApp.Simple as StartApp
import Signal exposing (..)
import Text exposing (fromString,Text)

-- global config
knownWords : List Word
knownWords = ["welt", "foobar","a","aa","aaa"]

allowedMistakes : Int
allowedMistakes = 4

main : Signal Html
main = StartApp.start { model = initialGame, view = view, update = update }

-- core types and functions

type alias Word = String

type State = Lost { word : Word }
           | Win
           | Active { guessedCharacters : Set.Set Char , mistakesLeft : Int , word : Word }
           | Pregame

type alias Game =
            { allowedMistakes : Int
            , state : State
            , seed : Seed
            , words : List Word
            }

initialGame : Game
initialGame =
  { allowedMistakes = allowedMistakes
  , state = Pregame
  , seed = initialSeed 37231
  , words = List.map String.toUpper knownWords
  }

type Action = Guess Char | Start

update : Action -> Game -> Game
update action model =
  case action of
    Guess c   -> step c model
    Start     -> startGame model

startGame : Game -> Game
startGame  game =
  let (w,seed') = randomWord game
  in
    { game | state = Active { guessedCharacters = Set.empty
                         , mistakesLeft = game.allowedMistakes
                         , word = w
                         }
           , seed = seed'
    }

randomWord : Game -> (Word,Seed)
randomWord game =
  let maxIndex : Int
      maxIndex = List.length game.words - 1
      (randomIndex,seed') = generate (Random.int 0 maxIndex) game.seed
  in  (get randomIndex game.words , seed')

step : Char -> Game -> Game
step c game = case game.state of
  Active {guessedCharacters , mistakesLeft , word } ->
    let
      mistakeMade = not <| Set.member c guessedCharacters || List.member c (String.toList word)
      state' = { guessedCharacters = Set.insert c guessedCharacters
               , mistakesLeft = if mistakeMade then mistakesLeft - 1 else mistakesLeft
               , word = word
               }
    in { game | state = nextState state'}
  _ -> game

nextState :{ guessedCharacters : Set.Set Char, mistakesLeft : Int, word : Word } -> State
nextState s =
  let isLost = s.mistakesLeft < 0
      isWon = isSubsetOf (Set.fromList <| String.toList s.word) s.guessedCharacters
  in if isLost then Lost { word = s.word }
              else if isWon then Win else Active s

-- the view

view : Address Action -> Game -> Html
view address model = case model.state of
  Pregame -> div [] [button [onClick address Start] [text "New game"]]
  Lost s -> div [] [fromElement <| leftAligned <| Text.fromString <| "You lost! The hidden word was \""++s.word++"\""
                   ,button [onClick address Start] [text "New game"]
                   ]
  Win  -> div [] [fromElement <| leftAligned <| Text.fromString "You win!",button [onClick address Start] [text "New game"]
                   ]
  Active s ->
   div [] (
    --[ div [] [ text (toString model) ]]++
    [fromElement <| flow down [ display s.guessedCharacters  s.word
              , leftAligned <| Text.fromString <| "Allowed number of mistakes: " ++ toString s.mistakesLeft
              ]
    ]
    ++ (List.map (\c -> buildGuessButton c s address) chars))

buildGuessButton c s address =
  let alreadyGuessed = Set.member c s.guessedCharacters
  in button [onClick address (Guess c), disabled alreadyGuessed]
            [text (String.fromChar c)]

chars : List Char
chars =
 ['A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z']

display : Set.Set Char -> Word -> Element
display guessedChars word =
  let
    visualize : Char -> Text
    visualize char = Text.fromString <| if Set.member char guessedChars
                    then  " " ++ String.fromList [char] ++ " "
                    else " _ "
  in leftAligned <| Text.concat <| List.map visualize <| String.toList word

-- helper functions

get : Int -> List Word -> Word
get n list = case list  of
    [] -> ""
    x :: xs   -> if n == 0 then x else get (n-1) xs

isSubsetOf : Set.Set comparable -> Set.Set comparable -> Bool
isSubsetOf set1 set2 = Set.empty == Set.diff set1 set2
