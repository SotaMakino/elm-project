module Main exposing (Model, Msg, init, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import SimpleGraph exposing (Option(..), barChart, lineChart)



--MAIN


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }



--MODEL


type alias Model =
    { id : Int, value : Int }


init : Model
init =
    Model 1 1


barData : List Float
barData =
    [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 ]


barGraphAttributes =
    { graphHeight = 100
    , graphWidth = 400
    , options = [ Color "rgb(200,0,0)", DeltaX 15, YTickmarks 6, XTickmarks 2, Scale 1.0 1.0 ]
    }


type Msg
    = Msg1
    | Msg2



--UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        Msg1 ->
            model

        Msg2 ->
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
        , div [ style "padding" "5px" ] [ button [ onClick Msg1 ] [ text "Rondamise Array" ] ]
        , div [] [ barChart barGraphAttributes barData ]
        ]
