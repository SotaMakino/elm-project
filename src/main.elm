module Main exposing (Model, Msg, init, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import SimpleGraph exposing (Option(..), barChart, lineChart)



--DATA


floatedList : List Int -> List Float
floatedList list =
    List.map (\a -> toFloat a) list


barGraphAttributes =
    { graphHeight = 200
    , graphWidth = 800
    , options = [ Color "gray", DeltaX 50, YTickmarks 6, XTickmarks 1, Scale 1.0 1.0 ]
    }



--MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



--MODEL


type alias Model =
    { barList : List Int }


init : Model
init =
    { barList = List.range 1 15 }


type Msg
    = NewValue



--UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        NewValue ->
            model



--VIEW


view : Model -> Html Msg
view model =
    div []
        [ header
            [ style "background-color" "skyblue"
            , style "font-size" "30px"
            , style "padding" "3px 5px"
            ]
            [ text "Comparison Sorting Algorithms" ]
        , div [] [ barChart barGraphAttributes (floatedList model.barList) ]
        ]
