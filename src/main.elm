module Main exposing (Model, Msg, init, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Random exposing (Seed, generate)
import Random.List exposing (shuffle)
import SimpleGraph exposing (GraphAttributes, Option(..), barChart, lineChart)
import SingleSlider exposing (..)



--DATA


insertionSort : List Int -> List Int
insertionSort list =
    case list of
        [] ->
            []

        [ rest ] ->
            [ rest ]

        left :: (right :: rest) ->
            if left < right then
                left :: insertionSort (right :: rest)

            else
                right :: insertionSort (left :: rest)


floatedList : List Int -> List Float
floatedList list =
    List.map (\a -> toFloat a) list


balancedAttributes : Model -> GraphAttributes
balancedAttributes model =
    { graphHeight = 300
    , graphWidth = 900
    , options = [ Color "#87E5CB", YTickmarks 6, XTickmarks 1, Scale 1.0 1.0, DeltaX model.deltaX ]
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
    { barSize : Int
    , barList : List Int
    , deltaX : Float
    , singleSlider : SingleSlider.SingleSlider Msg
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { barSize = 20
      , barList = List.range 1 20
      , deltaX = 52
      , singleSlider =
            SingleSlider.init
                { min = 20
                , max = 100
                , value = 40
                , step = 1
                , onChange = SingleSliderChange
                }
      }
    , Cmd.none
    )


type Msg
    = Randomize
    | RandomizedList (List Int)
    | InsertionSort
    | SingleSliderChange Float



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

        SingleSliderChange flt ->
            let
                newSlider =
                    SingleSlider.update flt model.singleSlider

                newDeltaX =
                    if flt > 80 then
                        flt * 0.1

                    else if flt > 50 then
                        flt * 0.2

                    else if flt > 45 then
                        flt * 0.3

                    else if flt > 30 then
                        flt * 0.5

                    else
                        flt
            in
            ( { model | singleSlider = newSlider, barList = List.range 1 (round flt), deltaX = newDeltaX }, Cmd.none )



--SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



--VIEW


view : Model -> Html Msg
view model =
    div [ style "padding" "35px" ]
        [ header
            [ style "color" "#6E7372"
            , style "font-size" "30px"
            , style "font-weight" "800"
            ]
            [ text "Comparison Sorting Algorithms" ]
        , div [ style "padding-top" "20px" ] [ barChart (balancedAttributes model) (floatedList model.barList) ]
        , div []
            [ sortButton Randomize "Randomize"
            , sortButton InsertionSort "Insertion Sort"
            ]
        , div [ style "padding-top" "15px" ] [ SingleSlider.view model.singleSlider ]
        ]


sortButton : Msg -> String -> Html Msg
sortButton f s =
    button
        [ style "margin" "4px"
        , style "font-size" "16px"
        , onClick f
        ]
        [ text s ]
