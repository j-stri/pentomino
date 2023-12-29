module Main exposing (Flags, Model, Msg, main)

import Browser
import Html exposing (Html, br, div, node, text)
import Html.Attributes exposing (style)
import List.Extra
import Svg exposing (Svg)
import Svg.Attributes
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


type alias Flags =
    ()


type Model
    = Welcome


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
        [ node "style" [] [ text """
            body {
                margin: 0;
                background: #333;
                overflow: hidden;
            }
        """ ]
        , let
            width : Int
            width =
                1 + playingFieldWidth + 1 + rightPaneWidth + 1

            height : Int
            height =
                1 + playingFieldHeight + 1

            horizontalLine : Int -> Svg msg
            horizontalLine y =
                Svg.line
                    [ Svg.Attributes.stroke "white"
                    , Svg.Attributes.x1 "0"
                    , Svg.Attributes.x2 <| String.fromInt width
                    , Svg.Attributes.y1 <| String.fromInt y
                    , Svg.Attributes.y2 <| String.fromInt y
                    ]
                    []

            verticalLine : Int -> Svg msg
            verticalLine x =
                Svg.line
                    [ Svg.Attributes.stroke "white"
                    , Svg.Attributes.x1 <| String.fromInt x
                    , Svg.Attributes.x2 <| String.fromInt x
                    , Svg.Attributes.y1 "0"
                    , Svg.Attributes.y2 <| String.fromInt height
                    ]
                    []
          in
          (List.map horizontalLine (List.range 0 height)
            ++ List.map verticalLine (List.range 0 width)
          )
            |> Svg.svg
                [ [ 0
                  , 0
                  , width
                  , height
                  ]
                    |> List.map String.fromInt
                    |> String.join " "
                    |> Svg.Attributes.viewBox
                , style "height" "100%"
                , style "width" "100%"
                , style "max-height" "calc(100vh - 16px)"
                , style "max-width" "calc(100vw - 16px)"
                , Svg.Attributes.stroke "black"
                , Svg.Attributes.strokeWidth "0.05"
                ]
        , case model of
            Welcome ->
                viewWelcome
        ]


viewWelcome : Html Msg
viewWelcome =
    text ""


viewAllPentominos : Html Msg
viewAllPentominos =
    List.map
        (\( color, pentomino ) ->
            div
                [ style "display" "flex"
                , style "gap" "16px"
                ]
            <|
                List.map (viewPentomino color) (rotateAll pentomino)
        )
        Theme.coloredPentominos
        |> List.intersperse (br [] [])
        |> div
            [ style "display" "flex"
            , style "gap" "16px"
            , style "flex-wrap" "wrap"
            ]


rotateAll : Pentomino -> List Pentomino
rotateAll pentomino =
    let
        go :
            List Pentomino
            -> Pentomino
            -> List Pentomino
        go acc p =
            if p == pentomino then
                List.reverse acc

            else
                go
                    (p :: acc)
                    (rotateCW p)
    in
    go [ pentomino ] (rotateCW pentomino)


rotateCW : Pentomino -> Pentomino
rotateCW pentomino =
    pentomino
        |> List.Extra.transpose
        |> List.map List.reverse


viewPentomino : String -> Pentomino -> Html Msg
viewPentomino color pentomino =
    let
        width : Int
        width =
            pentomino
                |> List.map List.length
                |> List.maximum
                |> Maybe.withDefault 1

        height : Int
        height =
            List.length pentomino

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
    pentomino
        |> List.indexedMap viewRow
        |> Svg.svg
            [ Svg.Attributes.viewBox <|
                String.join " " <|
                    List.map String.fromInt
                        [ 0
                        , 0
                        , width
                        , height
                        ]
            , Svg.Attributes.height (String.fromInt <| height * 32)
            , Svg.Attributes.width (String.fromInt <| width * 32)
            ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
