module Main exposing (Model, Msg, init, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Random exposing (Seed, generate)
import Random.List exposing (shuffle)
import SimpleGraph exposing (GraphAttributes, Option(..), barChart, lineChart)
import SingleSlider exposing (..)
import Time exposing (..)



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


dummyPosix : Posix
dummyPosix =
    millisToPosix 0



--MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



--MODEL


type State
    = NoOp
    | InsertionSorting


type alias Model =
    { barList : List Int
    , deltaX : Float
    , singleSlider : SingleSlider.SingleSlider Msg
    , state : State
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { barList = List.range 1 25
      , deltaX = 25
      , singleSlider =
            SingleSlider.init
                { min = 25
                , max = 100
                , value = 25
                , step = 1
                , onChange = SingleSliderChange
                }
                |> SingleSlider.withMinFormatter (always "")
                |> SingleSlider.withMaxFormatter (always "")
                |> SingleSlider.withValueFormatter (\n _ -> String.concat [ "List Size: ", String.fromFloat n ])
      , state = NoOp
      }
    , Cmd.none
    )


type Msg
    = Randomize
    | RandomizedList (List Int)
    | InsertionSort Time.Posix
    | SingleSliderChange Float



--UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Randomize ->
            ( { model | state = NoOp }, generate RandomizedList (shuffle model.barList) )

        RandomizedList randomizedList ->
            ( { model | barList = randomizedList }, Cmd.none )

        InsertionSort _ ->
            ( { model | barList = insertionSort model.barList, state = InsertionSorting }, Cmd.none )

        SingleSliderChange flt ->
            let
                newSlider =
                    SingleSlider.update flt model.singleSlider

                newDeltaX =
                    if flt > 90 then
                        flt * 0.08

                    else if flt > 80 then
                        flt * 0.11

                    else if flt > 66 then
                        flt * 0.15

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
    case model.state of
        InsertionSorting ->
            Time.every 1000 InsertionSort

        NoOp ->
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
            , sortButton (InsertionSort dummyPosix) "Insertion Sort"
            ]
        , div [ style "padding-top" "15px" ] [ SingleSlider.view model.singleSlider ]
        ]


sortButton : Msg -> String -> Html Msg
sortButton message title =
    button
        [ style "margin" "4px"
        , style "font-size" "16px"
        , onClick message
        ]
        [ text title ]
