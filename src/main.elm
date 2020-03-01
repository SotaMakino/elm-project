module Main exposing (Model, Msg, init, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Random exposing (Seed, generate)
import Random.List exposing (shuffle)
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
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



--MODEL


type alias Model =
    { barList : List Int }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { barList = List.range 1 15 }
    , Cmd.none
    )


type Msg
    = Randomize
    | RandomizedList (List Int)



--UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Randomize ->
            ( model, generate RandomizedList (shuffle model.barList) )

        RandomizedList randomizedList ->
            ( { model | barList = randomizedList }, Cmd.none )



--SUBSCRIPTIONS


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
        , div [] [ barChart barGraphAttributes (floatedList model.barList) ]
        , div [] [ button [ onClick Randomize ] [ text "Randomize Array" ] ]
        ]
