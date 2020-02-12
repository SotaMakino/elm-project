module Main exposing (Model, Msg, init, update, view)

import Browser
import Html exposing (..)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


type alias Model =
    { id : Int, value : Int }


init : Model
init =
    Model 1 1


type Msg
    = Msg1
    | Msg2


update : Msg -> Model -> Model
update msg model =
    case msg of
        Msg1 ->
            model

        Msg2 ->
            model


view : Model -> Html Msg
view model =
    div []
        [ text "New Sandbox" ]



-- Bars


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
