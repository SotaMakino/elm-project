module Main exposing (Model, Msg, init, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Random exposing (Seed, generate)
import Random.List exposing (shuffle)
import SimpleGraph exposing (Option(..), barChart, lineChart)



--DATA


insertionSort : List Int -> List Int
insertionSort list =
    case list of
        [] ->
            []

        [ rest ] ->
            [ rest ]

        left :: right :: rest ->
            if left < right then
                left :: insertionSort (right :: rest)

            else
                right :: insertionSort (left :: rest)


floatedList : List Int -> List Float
floatedList list =
    List.map (\a -> toFloat a) list


barGraphAttributes =
    { graphHeight = 200
    , graphWidth = 800
    , options = [ Color "#87E5CB", DeltaX 52, YTickmarks 6, XTickmarks 1, Scale 1.0 1.0 ]
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
    | InsertionSort



--UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Randomize ->
            ( model, generate RandomizedList (shuffle model.barList) )

        RandomizedList randomizedList ->
            ( { model | barList = randomizedList }, Cmd.none )

        InsertionSort ->
            ( { model | barList = insertionSort model.barList }, Cmd.none )



--SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



--VIEW


view : Model -> Html Msg
view model =
    div []
        [ header
            [ style "color" "#6E7372"
            , style "font-size" "30px"
            , style "font-weight" "800"
            , style "padding" "3px 20px"
            ]
            [ text "Comparison Sorting Algorithms" ]
        , div [ style "padding-top" "20px" ] [ barChart barGraphAttributes (floatedList model.barList) ]
        , div [ style "margin-left" "10px" ]
            [ sortButton Randomize "Randomize"
            , sortButton InsertionSort "Insertion Sort"
            ]
        ]


sortButton : Msg -> String -> Html Msg
sortButton f s =
    button
        [ style "margin" "4px"
        , style "font-size" "16px"
        , onClick f
        ]
        [ text s ]
