module View exposing (view)

import Constants exposing (playingFieldHeight, playingFieldWidth)
import Html exposing (Html, text)
import Html.Attributes exposing (style)
import Pentomino exposing (Pentomino)
import Svg exposing (Svg, text_)
import Svg.Attributes exposing (alignmentBaseline, textAnchor, x, y)
import Theme
import Types exposing (LostModel, Model(..), PlayingModel)


screenHeight : number
screenHeight =
    1 + playingFieldHeight + 1


screenWidth : number
screenWidth =
    1 + playingFieldWidth + 1 + rightPaneWidth + 1


rightPaneWidth : number
rightPaneWidth =
    8


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


coordinate : Float -> String
coordinate c =
    String.fromFloat (c * 100)


backgroundGrid : Svg msg
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


view : Model -> Html msg
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


rightPane : Model -> Svg msg
rightPane model =
    case model of
        Welcome ->
            none

        Playing { nextPiece, score } ->
            Svg.g []
                [ viewNextPiece nextPiece
                , viewScore score
                ]

        Lost { nextPiece, score } ->
            Svg.g []
                [ viewNextPiece nextPiece
                , viewScore score
                ]


viewScore score =
    text_
        [ x <| coordinate (playingFieldWidth + 3)
        , y <| coordinate 11
        ]
        [ text ("Score is " ++ String.fromInt score) ]


viewNextPiece : Pentomino -> Svg msg
viewNextPiece pentomino =
    viewPentomino ( 1 + playingFieldWidth + 1 + 1, 2, pentomino )


playingField : Model -> Svg msg
playingField model =
    case model of
        Welcome ->
            none

        Playing { currentPiece, grid } ->
            Svg.g []
                [ viewPentomino (Pentomino.moveRight <| Pentomino.moveDown currentPiece)
                , viewGrid grid
                ]

        Lost { grid } ->
            Svg.g [] [ viewGrid grid ]


none : Svg msg
none =
    Svg.g [] []


viewGrid : List (List String) -> Svg msg
viewGrid rows =
    let
        viewRow : Int -> List String -> List (Svg msg)
        viewRow y row =
            row
                |> List.indexedMap (viewGridCell y)
                |> List.concat

        viewGridCell : Int -> Int -> String -> List (Svg msg)
        viewGridCell y x cell =
            viewCell cell (x + 1) (y + 1) (cell /= "")
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


overlay : Model -> Svg msg
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


layout : Svg msg
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
        [ middleText "Paused! Press \"p\" to resume or <Enter> to restart!"
        ]

    else
        []


viewLost : LostModel -> List (Svg msg)
viewLost _ =
    [ middleText "Lost! Press <Enter> to restart!"
    ]


middleText : String -> Svg msg
middleText content =
    let
        xcoord = coordinate (screenWidth / 2)
        ycoord = coordinate (screenHeight / 2)

        width = 25

        height = 2


    in
    
    Svg.g []
        [ rect "gray" (screenWidth / 2 - width / 2) (screenHeight / 2 - height / 2 - 0.12) width height
        , text_
            [ textAnchor "middle"
            , alignmentBaseline "middle"
            , x <| xcoord
            , y <| ycoord
            ]
            [ text content ]
        ]


viewPentomino : ( Int, Int, Pentomino ) -> Svg msg
viewPentomino ( dx, dy, ( color, pentomino ) ) =
    let
        viewRow : Int -> List Bool -> List (Svg msg)
        viewRow y row =
            row
                |> List.indexedMap
                    (\x ->
                        viewCell color
                            (x + dx)
                            (y + dy)
                    )
                |> List.concat
    in
    Svg.g [] <| List.concat <| List.indexedMap viewRow pentomino


viewCell : String -> Int -> Int -> Bool -> List (Svg msg)
viewCell color x y cell =
    if cell then
        let
            fx : Float
            fx =
                toFloat x

            fy : Float
            fy =
                toFloat y

            border : Float
            border =
                0.05
        in
        [ rect "gray" fx fy 1 1
        , rect color (fx + border) (fy + border) (1 - border * 2) (1 - border * 2)
        ]

    else
        []
