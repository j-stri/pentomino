module Main exposing (main)

import Browser
import Browser.Events
import Constants exposing (playingFieldHeight, playingFieldWidth)
import Json.Decode
import Pentomino exposing (Color, Pentomino)
import Random
import Random.List
import Theme
import Time
import Types exposing (Flags, Model(..), Msg(..), PlayingModel)
import View exposing (view)


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

        ( Tick, Playing playingModel ) ->
            if playingModel.pause then
                ( model, Cmd.none )

            else
                ( Playing (moveDown playingModel), Cmd.none )

        _ ->
            ( model, Cmd.none )


moveDown : PlayingModel -> PlayingModel
moveDown model =
    let
        moved : ( Int, Int, Pentomino )
        moved =
            Pentomino.moveDown model.currentPiece
    in
    if collides moved model.grid then
        let
            _ =
                Debug.todo
        in
        model

    else
        { model | currentPiece = moved }


collides : ( Int, Int, Pentomino ) -> List (List Color) -> Bool
collides ( x, y, pentomino ) grid =
    let
        pentominoWidth : Int
        pentominoWidth =
            Pentomino.width pentomino

        pentominoHeight : Int
        pentominoHeight =
            Pentomino.height pentomino

        gridSlice : List (List Color)
        gridSlice =
            grid
                |> List.drop y
                |> List.take pentominoHeight
                |> List.map
                    (\line ->
                        line
                            |> List.drop x
                            |> List.take pentominoWidth
                    )
    in
    if List.length gridSlice < pentominoHeight then
        -- Pentomino got offscreen at the bottom
        True

    else
        let
            _ =
                Debug.todo
        in
        False


playingModelGenerator : Random.Generator PlayingModel
playingModelGenerator =
    Random.map2
        (\seed ( currentPiece, nextPiece, queue ) ->
            let
                currentPieceWidth : Int
                currentPieceWidth =
                    Pentomino.width currentPiece
            in
            { pause = False
            , score = 0
            , currentPiece =
                ( (playingFieldWidth - currentPieceWidth // 2)
                    // 2
                    - 1
                , 0
                , currentPiece
                )
            , nextPiece = nextPiece
            , queue = queue
            , grid =
                List.repeat playingFieldHeight
                    (List.repeat playingFieldWidth "")
            , seed = seed
            }
        )
        Random.independentSeed
        (Random.List.shuffle Theme.coloredPentominos
            |> Random.map
                (\list ->
                    case list of
                        one :: two :: tail ->
                            ( one, two, tail )

                        _ ->
                            horribleHackPleaseForgiveMe ()
                )
        )


horribleHackPleaseForgiveMe : () -> a
horribleHackPleaseForgiveMe _ =
    horribleHackPleaseForgiveMe ()


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ case model of
            Playing playingModel ->
                ticks playingModel

            _ ->
                Sub.none
        , Browser.Events.onKeyPress
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
        ]


ticks : PlayingModel -> Sub Msg
ticks { score } =
    let
        -- Should tick every second at the beginning
        initial : number
        initial =
            1000

        -- Should get faster every 100 points
        increases : Int
        increases =
            score // 100

        interval : Int
        interval =
            initial * 9 ^ increases // (10 ^ increases)
    in
    Time.every (toFloat interval) (\_ -> Tick)
