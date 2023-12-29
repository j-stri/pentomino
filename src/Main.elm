module Main exposing (Flags, Model, Msg, main)

import Browser
import Html exposing (Html, br, div, node, text)
import Html.Attributes exposing (style)
import List.Extra
import Svg exposing (Svg, text_)
import Svg.Attributes exposing (alignmentBaseline, textAnchor, x, y)
import Theme exposing (Pentomino)


playingFieldWidth : Int
playingFieldWidth =
    10


playingFieldHeight : Int
playingFieldHeight =
    20


rightPaneWidth : Int
rightPaneWidth =
    8


screenHeight : Int
screenHeight =
    1 + playingFieldHeight + 1


screenWidth : Int
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


type alias Msg =
    ()


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( Welcome
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div
        [ style "width" "100vw"
        , style "height" "100vh"
        , style "display" "flex"
        , style "align-items" "center"
        , style "justify-content" "center"
        ]
        [ node "style" [] [ text <| """
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

                    Playing _ ->
                        Debug.todo "branch 'Playing _' not implemented"

                    Lost _ ->
                        Debug.todo "branch 'Lost _' not implemented"
                )
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


coordinate : Int -> String
coordinate c =
    String.fromInt (c * 100)


line : String -> Int -> Int -> Int -> Int -> Svg msg
line color x1 x2 y1 y2 =
    Svg.line
        [ Svg.Attributes.stroke color
        , Svg.Attributes.x1 <| coordinate x1
        , Svg.Attributes.x2 <| coordinate x2
        , Svg.Attributes.y1 <| coordinate y1
        , Svg.Attributes.y2 <| coordinate y2
        ]
        []


rect : String -> Int -> Int -> Int -> Int -> Svg msg
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
        , x <| coordinate (screenWidth // 2)
        , y <| coordinate (screenHeight // 2)
        ]
        [ text "Welcome! Press <Enter> to begin!" ]
    ]


rotateCW : Pentomino -> Pentomino
rotateCW pentomino =
    pentomino
        |> List.Extra.transpose
        |> List.map List.reverse


viewPentomino : String -> Pentomino -> List (Svg msg)
viewPentomino color pentomino =
    let
        viewRow : Int -> List Bool -> Svg.Svg msg
        viewRow y row =
            row
                |> List.indexedMap (viewCell y)
                |> Svg.g []

        viewCell : Int -> Int -> Bool -> Svg.Svg msg
        viewCell y x cell =
            if cell then
                Svg.rect
                    [ Svg.Attributes.fill color
                    , Svg.Attributes.width "1"
                    , Svg.Attributes.height "1"
                    , Svg.Attributes.x (String.fromInt x)
                    , Svg.Attributes.y (String.fromInt y)
                    ]
                    []

            else
                Svg.g [] []
    in
    List.indexedMap viewRow pentomino


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
