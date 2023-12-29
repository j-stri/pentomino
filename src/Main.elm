module Main exposing (Flags, Model, Msg, main)

import Browser
import Browser.Events
import Html exposing (Html, text)
import Html.Attributes exposing (style)
import Json.Decode
import Pentomino exposing (Pentomino)
import Random
import Random.List
import Svg exposing (Svg, text_)
import Svg.Attributes exposing (alignmentBaseline, textAnchor, x, y)
import Theme


playingFieldWidth : number
playingFieldWidth =
    10


playingFieldHeight : number
playingFieldHeight =
    20


rightPaneWidth : number
rightPaneWidth =
    8


screenHeight : number
screenHeight =
    1 + playingFieldHeight + 1


screenWidth : number
screenWidth =
    1 + playingFieldWidth + 1 + rightPaneWidth + 1


type alias Flags =
    ()


type Model
    = Welcome
    | Playing PlayingModel
    | Lost LostModel


type alias PlayingModel =
    { pause : Bool
    , nextPiece : Pentomino
    , queue : List Pentomino
    , grid : List (List String)
    }


type alias LostModel =
    { nextPiece : Pentomino
    , grid : List (List String)
    }


type Msg
    = Enter
    | Space
    | Generated PlayingModel


main : Program Flags Model Msg
main =
    Browser.element
        { init = \flags -> ( init flags, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Flags -> Model
init _ =
    Welcome


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( Enter, Welcome ) ->
            ( model
            , Random.generate Generated playingModelGenerator
            )

        ( Generated playingModel, _ ) ->
            ( Playing playingModel, Cmd.none )

        ( Space, Playing playingModel ) ->
            ( Playing { playingModel | pause = not playingModel.pause }, Cmd.none )

        _ ->
            ( model, Cmd.none )


playingModelGenerator : Random.Generator PlayingModel
playingModelGenerator =
    Random.map
        (\( nextPiece, queue ) ->
            { pause = False
            , nextPiece = nextPiece
            , queue = queue
            , grid =
                List.repeat playingFieldHeight
                    (List.repeat playingFieldWidth "")
            }
        )
        (Random.List.shuffle Theme.coloredPentominos
            |> Random.map
                (\list ->
                    case list of
                        [] ->
                            Debug.todo "TODO"

                        head :: tail ->
                            ( head, tail )
                )
        )


view : Model -> Html Msg
view model =
    Html.div
        [ Html.Attributes.style "width" "100vw"
        , Html.Attributes.style "height" "100vh"
        , Html.Attributes.style "display" "flex"
        , Html.Attributes.style "align-items" "center"
        , Html.Attributes.style "justify-content" "center"
        ]
        [ Html.node "style" [] [ Html.text pageStyle ]
        , [ layout
          , backgroundGrid
          , playingField model
          , rightPane model
          , overlay model
          ]
            |> Svg.svg
                [ [ 0
                  , 0
                  , screenWidth
                  , screenHeight
                  ]
                    |> List.map coordinate
                    |> String.join " "
                    |> Svg.Attributes.viewBox
                , style "height" "100%"
                , style "width" "100%"
                , style "max-height" "calc(100vh - 16px)"
                , style "max-width" "calc(100vw - 16px)"
                , Svg.Attributes.stroke "none"
                , Svg.Attributes.fill "none"
                ]
        ]


rightPane : Model -> Svg Msg
rightPane model =
    case model of
        Welcome ->
            none

        Playing { nextPiece } ->
            viewNextPiece nextPiece

        Lost { nextPiece } ->
            viewNextPiece nextPiece


viewNextPiece : Pentomino -> Svg Msg
viewNextPiece pentomino =
    viewPentomino (1 + playingFieldWidth + 1 + 1) 2 pentomino


playingField : Model -> Svg Msg
playingField model =
    case model of
        Welcome ->
            none

        Playing { grid } ->
            viewGrid grid

        Lost { grid } ->
            viewGrid grid


none : Svg Msg
none =
    Svg.g [] []


viewGrid : List (List String) -> Svg Msg
viewGrid rows =
    let
        viewRow : Int -> List String -> List (Svg msg)
        viewRow y row =
            row
                |> List.indexedMap (viewGridCell y)
                |> List.concat

        viewGridCell : Int -> Int -> String -> List (Svg msg)
        viewGridCell y x cell =
            viewCell cell (toFloat x + 1) (toFloat y + 1) (cell /= "")
    in
    Svg.g [] <| List.concat <| List.indexedMap viewRow rows


pageStyle : String
pageStyle =
    """
    body {
        margin: 0;
        background: """ ++ Theme.background ++ """;
        overflow: hidden;
    }
    
    text {
        font: """ ++ coordinate 1 ++ """px sans-serif;
        fill: white;
    }
    """


overlay : Model -> Svg Msg
overlay model =
    Svg.g []
        (case model of
            Welcome ->
                viewWelcome

            Playing playingModel ->
                viewPlaying playingModel

            Lost lostModel ->
                viewLost lostModel
        )


layout : Svg Msg
layout =
    Svg.g []
        [ rect "black"
            0
            0
            screenWidth
            screenHeight
        , rect Theme.background
            1
            1
            playingFieldWidth
            playingFieldHeight
        , rect Theme.background
            (1 + playingFieldWidth + 1)
            1
            rightPaneWidth
            7
        , rect Theme.background
            (1 + playingFieldWidth + 1)
            9
            rightPaneWidth
            (playingFieldHeight - 8)
        ]


backgroundGrid : Svg Msg
backgroundGrid =
    let
        horizontalRows : List (Svg msg)
        horizontalRows =
            List.map
                (\y ->
                    line "gray" 1 (playingFieldWidth + 1) (toFloat y) (toFloat y)
                )
                (List.range 1 (playingFieldHeight + 1))

        verticalRows : List (Svg msg)
        verticalRows =
            List.map
                (\x ->
                    line "gray" (toFloat x) (toFloat x) 1 (playingFieldHeight + 1)
                )
                (List.range 1 (playingFieldWidth + 1))
    in
    Svg.g [] (horizontalRows ++ verticalRows)


coordinate : Float -> String
coordinate c =
    String.fromFloat (c * 100)


line : String -> Float -> Float -> Float -> Float -> Svg msg
line color x1 x2 y1 y2 =
    Svg.line
        [ Svg.Attributes.stroke color
        , Svg.Attributes.x1 <| coordinate x1
        , Svg.Attributes.x2 <| coordinate x2
        , Svg.Attributes.y1 <| coordinate y1
        , Svg.Attributes.y2 <| coordinate y2
        ]
        []


rect : String -> Float -> Float -> Float -> Float -> Svg msg
rect color x y width height =
    Svg.rect
        [ Svg.Attributes.fill color
        , Svg.Attributes.x <| coordinate x
        , Svg.Attributes.y <| coordinate y
        , Svg.Attributes.width <| coordinate width
        , Svg.Attributes.height <| coordinate height
        ]
        []


viewWelcome : List (Svg msg)
viewWelcome =
    [ middleText "Welcome! Press <Enter> to begin!"
    ]


viewPlaying : PlayingModel -> List (Svg msg)
viewPlaying model =
    if model.pause then
        [ middleText "Paused! Press <Space> to resume!"
        ]

    else
        []


viewLost : LostModel -> List (Svg msg)
viewLost _ =
    [ middleText "Lost! Press <Enter> to restart!"
    ]


middleText : String -> Svg msg
middleText content =
    text_
        [ textAnchor "middle"
        , alignmentBaseline "middle"
        , x <| coordinate (screenWidth / 2)
        , y <| coordinate (screenHeight / 2)
        ]
        [ text content ]


viewPentomino : Float -> Float -> Pentomino -> Svg msg
viewPentomino dx dy ( color, pentomino ) =
    let
        viewRow : Int -> List Bool -> List (Svg msg)
        viewRow y row =
            row
                |> List.indexedMap
                    (\x ->
                        viewCell color
                            (toFloat x + dx)
                            (toFloat y + dy)
                    )
                |> List.concat
    in
    Svg.g [] <| List.concat <| List.indexedMap viewRow pentomino


viewCell : String -> Float -> Float -> Bool -> List (Svg msg)
viewCell color x y cell =
    if cell then
        let
            border : Float
            border =
                0.05
        in
        [ rect "gray" x y 1 1
        , rect color (x + border) (y + border) (1 - border * 2) (1 - border * 2)
        ]

    else
        []


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onKeyPress
        (Json.Decode.field "key" Json.Decode.string
            |> Json.Decode.andThen
                (\key ->
                    case key of
                        "Enter" ->
                            Json.Decode.succeed Enter

                        " " ->
                            Json.Decode.succeed Space

                        _ ->
                            let
                                _ =
                                    Debug.log "Ignored" key
                            in
                            Json.Decode.fail "Ignored"
                )
        )
