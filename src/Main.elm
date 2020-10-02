-- A text input for reversing text. Very useful!
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/text_fields.html
--

module Main exposing (..)

import Browser
import Html exposing (Html, Attribute, div, br, input, text, textarea, p)
import Html.Attributes exposing (placeholder, value, rows)
import Html.Events exposing (onInput)
import String exposing (join, lines, fromInt)
import List exposing (intersperse, partition, length, take,sum, drop, map, concat, foldr, filter, head)
import Set exposing (fromList, size, Set)
import Debug
import Dict exposing (Dict)
import Maybe exposing (withDefault)



-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
  { content : String
--  , payments : Dict String Int
  }

type alias Payment = (String, Int)
type alias InputText = String

init : Model
init =
  { content =
      """Паша пицца 100
Маша суши 50
Маша колу 10
Иван жвачка 5"""
  }



-- UPDATE


type Msg
  = Change String


update : Msg -> Model -> Model
update msg model =
  case msg of
    Change newContent ->
      { model | content = newContent }



-- VIEW


view : Model -> Html Msg
view model =
  let
    numOfPeople = peopleCount model.content |> Debug.toString
    sum = totalValue model.content |> Debug.toString
    perPerson = perPersonValue model.content
    (dolzhniki, nuzhdaushiesya) =
      allPayments model.content
      |> partition (\(name, val) -> perPerson - val > 0)
    saldos = [dolzhniki, nuzhdaushiesya]
      |> map (map (\(name, val) -> name ++ " : " ++ (perPerson - val |> Debug.toString)))
      |> map (join ", ")
      |> intersperse " -> "
      |> join " "
      |> Debug.toString
    payments =
      allPayments model.content
      |> map (\(name, val) -> name ++ " : " ++ (fromInt val))
      |> join ", "
      |> Debug.toString

  in
    div []
      [ p [] [ text "как мы тратили бабосы :"]
      , textarea
        [ placeholder "Лена такси 1000"
        , value model.content, onInput Change
        , rows 6
        ] []
      , div [] [ text <| "нас было : " ++ numOfPeople ]
      , div [] [ text <| "прокутили : " ++ sum ]
      , div [] [ text <| "с носа вышло : " ++ Debug.toString perPerson  ]
      , div [] [ text <| "вложились : " ++ payments  ]
      , br [] []
      , div [] [ text <| "че каво : " ++ saldos  ]
      ]

peopleCount : InputText -> Int
peopleCount text =
 peopleUniqNames text
 |> Set.size


takeFromLine : Int -> String -> String
takeFromLine num string =
  String.split " " string
   |> drop (num - 1)
   |> head
   |> withDefault ""

totalValue : InputText -> Int
totalValue text =
  text
   |> lines
   |> map (takeFromLine 3)
   |> map (\num -> withDefault 0 <| String.toInt num)
   |> sum

perPersonValue : InputText -> Int
perPersonValue text =
  let
    sum = totalValue text |> toFloat
    people = peopleCount text |> toFloat
  in
    sum / people |> round

peopleUniqNames : InputText -> Set String
peopleUniqNames text =
  text
 |> lines
 |> map (takeFromLine 1)
 |> filter (\word -> word /= "")
 |> Set.fromList

peopleUniqPlaces : InputText -> Set String
peopleUniqPlaces text =
  text
 |> lines
 |> map (takeFromLine 2)
 |> filter (\word -> word /= "")
 |> Set.fromList

sumPaymentsByName : InputText -> String -> Int
sumPaymentsByName text name =
 text
 |> lines
 |> filter (\line -> takeFromLine 1 line == name)
 |> map (takeFromLine 3)
 |> map (\num -> withDefault 0 <| String.toInt num)
 |> sum


allPayments : InputText -> List Payment
allPayments text =
  let
    uniqNames = peopleUniqNames text
  in
  uniqNames
  |> Set.toList
  |> map (\name -> (name, sumPaymentsByName text name))
