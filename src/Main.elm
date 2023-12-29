module Main exposing (Flags, Model, Msg, main)

import Browser
import Browser.Events
import Html exposing (Html, text)
import Html.Attributes exposing (style)
import Json.Decode
import List.Extra
import Svg exposing (Svg, text_)
import Svg.Attributes exposing (alignmentBaseline, textAnchor, x, y)
import Theme exposing (Pentomino)


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
    }


type alias LostModel =
    {}


type Msg
    = Enter
    | Space


main : Program Flags Model Msg
main =
    Browser.element
        { init = \flags -> ( init flags, Cmd.none )
        , view = view
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = subscriptions
        }


init : Flags -> Model
init _ =
    Welcome


update : Msg -> Model -> Model
update msg model =
    case ( msg, model ) of
        ( Enter, Welcome ) ->
            Playing { pause = False }

        ( Space, Playing playingModel ) ->
            Playing { playingModel | pause = not playingModel.pause }

        _ ->
            model


view : Model -> Html Msg
view model =
    Html.div
        [ Html.Attributes.style "width" "100vw"
        , Html.Attributes.style "height" "100vh"
        , Html.Attributes.style "display" "flex"
        , Html.Attributes.style "align-items" "center"
        , Html.Attributes.style "justify-content" "center"
        ]
        [ Html.node "style" [] [ Html.text <| """
            body {
                margin: 0;
                background: """ ++ Theme.background ++ """;
                overflow: hidden;
            }
            """ ]
        , [ Svg.style [] [ Svg.text <| """
            text {
                font: """ ++ coordinate 1 ++ """px sans-serif;
                fill: white;
            }
            """ ]
          , Svg.g []
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
                    playingFieldHeight
                ]
          , Svg.g []
                (case model of
                    Welcome ->
                        viewWelcome

                    Playing playingModel ->
                        viewPlaying playingModel

                    Lost lostModel ->
                        viewLost lostModel
                )
          , Theme.coloredPentominos
                |> List.reverse
                |> List.head
                |> Maybe.map List.singleton
                |> Maybe.withDefault []
                |> List.map (\( color, pentomino ) -> viewPentomino color 1 1 pentomino)
                |> Svg.g []
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
    [ text_
        [ textAnchor "middle"
        , alignmentBaseline "middle"
        , x <| coordinate (screenWidth / 2)
        , y <| coordinate (screenHeight / 2)
        ]
        [ text "Welcome! Press <Enter> to begin!" ]
    ]


viewPlaying : PlayingModel -> List (Svg msg)
viewPlaying model =
    if model.pause then
        [ text_
            [ textAnchor "middle"
            , alignmentBaseline "middle"
            , x <| coordinate (screenWidth / 2)
            , y <| coordinate (screenHeight / 2)
            ]
            [ text "Paused! Press <Space> to resume!"
            ]
        ]

    else
        []


viewLost : LostModel -> List (Svg msg)
viewLost _ =
    [ text_
        [ textAnchor "middle"
        , alignmentBaseline "middle"
        , x <| coordinate (screenWidth / 2)
        , y <| coordinate (screenHeight / 2)
        ]
        [ text "Lost!" ]
    ]


rotateCW : Pentomino -> Pentomino
rotateCW pentomino =
    pentomino
        |> List.Extra.transpose
        |> List.map List.reverse


viewPentomino : String -> Float -> Float -> Pentomino -> Svg msg
viewPentomino color dx dy pentomino =
    let
        viewRow : Int -> List Bool -> List (Svg msg)
        viewRow y row =
            row
                |> List.indexedMap (viewCell (toFloat y))
                |> List.concat

        border : Float
        border =
            0.05

        viewCell : Float -> Int -> Bool -> List (Svg msg)
        viewCell y x cell =
            if cell then
                [ rect "gray" (toFloat x + dx) (y + dy) 1 1
                , rect color (toFloat x + dx + border) (y + dy + border) (1 - border * 2) (1 - border * 2)
                ]

            else
                []
    in
    Svg.g [] <| List.concat <| List.indexedMap viewRow pentomino


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
