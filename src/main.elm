module Main exposing (Model, Msg, init, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)



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
        ]



-- BARS


bar1 : Model
bar1 =
    { id = 1, value = 1 }


bar2 : Model
bar2 =
    { id = 2, value = 2 }


bar3 : Model
bar3 =
    { id = 3, value = 3 }


bars : List Model
bars =
    [ bar1, bar2, bar3 ]
