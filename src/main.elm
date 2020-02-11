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
    { id = 1, value = 10 }


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
