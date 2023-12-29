module Main exposing (Flags, Model, Msg, main)

import Browser
import Html exposing (Html, br, div)
import Html.Attributes exposing (style)
import List.Extra
import Svg
import Svg.Attributes
import Theme exposing (Pentomino)


type alias Flags =
    ()


type alias Model =
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
    ( {}
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view _ =
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
                    [ Svg.Attributes.stroke "black"
                    , Svg.Attributes.strokeWidth "0.05"
                    , Svg.Attributes.fill color
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
