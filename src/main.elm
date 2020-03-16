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


selectionSort : List Int -> List Int
selectionSort list =
    case list of
        [] ->
            []

        [ rest ] ->
            [ rest ]

        left :: (right :: rest) ->
            if List.minimum list == Just left then
                left :: selectionSort (right :: rest)

            else
                right :: selectionSort (left :: rest)


quickSort : List Int -> List Int
quickSort list =
    case list of
        [] ->
            []

        pivot :: rest ->
            let
                smaller =
                    List.filter (\n -> n <= pivot) rest

                bigger =
                    List.filter (\n -> n > pivot) rest
            in
            if List.sort smaller == smaller then
                smaller ++ [ pivot ] ++ quickSort bigger

            else
                quickSort smaller ++ [ pivot ] ++ bigger


floatedList : List Int -> List Float
floatedList list =
    List.map (\n -> toFloat n) list


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
    = Stop
    | InsertionSorting
    | SelectionSorting
    | QuickSorting


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
                |> SingleSlider.withValueFormatter (\n _ -> String.concat [ "- List Size: ", String.fromFloat n ])
      , state = Stop
      }
    , Cmd.none
    )


type Msg
    = Randomize
    | RandomizedList (List Int)
    | InsertionSort Time.Posix
    | SelectionSort Time.Posix
    | QuickSort Time.Posix
    | SingleSliderChange Float
    | NoOp



--UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( { model | state = Stop }, Cmd.none )

        Randomize ->
            ( model, generate RandomizedList (shuffle model.barList) )

        RandomizedList randomizedList ->
            ( { model | barList = randomizedList, state = Stop }, Cmd.none )

        InsertionSort _ ->
            ( { model | barList = insertionSort model.barList, state = InsertionSorting }, Cmd.none )

        SelectionSort _ ->
            ( { model | barList = selectionSort model.barList, state = SelectionSorting }, Cmd.none )

        QuickSort _ ->
            ( { model | barList = quickSort model.barList, state = QuickSorting }, Cmd.none )

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
            Time.every 400 InsertionSort

        SelectionSorting ->
            Time.every 400 SelectionSort

        QuickSorting ->
            Time.every 400 QuickSort

        Stop ->
            Sub.none



--VIEW


view : Model -> Html Msg
view model =
    div [ style "padding" "35px" ]
        [ header
            [ style "color" "#6e7372"
            , style "font-size" "30px"
            , style "font-weight" "800"
            ]
            [ text "Comparison Sorting Algorithms" ]
        , div [ style "padding-top" "20px" ] [ barChart (balancedAttributes model) (floatedList model.barList) ]
        , div []
            [ sortButton Randomize "Randomize" False
            , sortButton (InsertionSort dummyPosix) "Insertion Sort" False
            , sortButton (SelectionSort dummyPosix) "Selection Sort" False
            , sortButton (QuickSort dummyPosix) "Quick Sort" False
            , sortButton NoOp "Stop" True
            ]
        , div [ style "padding-top" "10px" ] [ SingleSlider.view model.singleSlider ]
        ]


sortButton : Msg -> String -> Bool -> Html Msg
sortButton message title isProminent =
    let
        color =
            if isProminent == True then
                "#fff"

            else
                "#333"

        backgroundColor =
            if isProminent == True then
                "f08080"

            else
                "#fff"
    in
    button
        [ style "margin" "13px 5px"
        , style "font-size" "16px"
        , style "font-weight" "500"
        , style "border" "0.1em solid #6e7372"
        , style "padding" "0.5em 1em"
        , style "color" color
        , style "background-color" backgroundColor
        , onClick message
        ]
        [ text title ]
