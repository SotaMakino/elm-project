module Main exposing (Model, Msg, init, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Random
import SimpleGraph exposing (Option(..), barChart, lineChart)



--DATA


barData : List Float
barData =
    [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 ]


barGraphAttributes =
    { graphHeight = 200
    , graphWidth = 800
    , options = [ Color "gray", DeltaX 50, YTickmarks 6, XTickmarks 1, Scale 1.0 1.0 ]
    }



--MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }



--MODEL


type alias Model =
    { value : Int }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model 1
    , Cmd.none
    )


type Msg
    = Randomize
    | NewValue Int



--UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Randomize ->
            ( model, Random.generate NewValue (Random.int 1 15) )

        NewValue value ->
            ( Model value, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



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
        , div [ style "padding" "5px" ] [ button [ onClick Randomize ] [ text "Randomise Array" ] ]
        , div [] [ barChart barGraphAttributes barData ]
        , div [] [ text (String.fromInt model.value) ]
        ]
