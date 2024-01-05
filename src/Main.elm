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
import Types exposing (Flags, Key(..), Model(..), Msg(..), PlayingModel)
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
        ( KeyUp Enter, Welcome ) ->
            ( model
            , Random.generate Generated playingModelGenerator
            )

        ( KeyUp Enter, Lost _ ) ->
            ( model
            , Random.generate Generated playingModelGenerator
            )

        ( Generated playingModel, _ ) ->
            ( Playing playingModel, Cmd.none )

        ( KeyDown Pause, Playing playingModel ) ->
            ( Playing { playingModel | pause = not playingModel.pause }, Cmd.none )

        ( Tick, Playing playingModel ) ->
            if playingModel.pause then
                ( model, Cmd.none )

            else
                let
                    newModel : PlayingModel
                    newModel =
                        moveDown playingModel
                in
                if collides newModel.currentPiece newModel.grid then
                    ( Lost
                        { grid = newModel.grid
                        , score = newModel.score
                        , nextPiece = playingModel.nextPiece
                        }
                    , Cmd.none
                    )

                else
                    ( Playing newModel, Cmd.none )

        ( KeyDown key, Playing playingModel ) ->
            if playingModel.pause then
                ( model, Cmd.none )

            else
                ( (case key of
                    ArrowUp ->
                        rotateCW playingModel

                    ArrowLeft ->
                        moveUnlessCollides (Pentomino.moveLeft playingModel.currentPiece) playingModel

                    ArrowRight ->
                        moveUnlessCollides (Pentomino.moveRight playingModel.currentPiece) playingModel

                    Space ->
                        let
                            go : PlayingModel -> PlayingModel
                            go m =
                                let
                                    new : PlayingModel
                                    new =
                                        moveDown m
                                in
                                if new.grid == m.grid then
                                    go new

                                else
                                    m
                        in
                        go playingModel

                    _ ->
                        playingModel
                  )
                    |> Playing
                , Cmd.none
                )

        _ ->
            ( model, Cmd.none )


rotateCW : PlayingModel -> PlayingModel
rotateCW model =
    let
        (( rotatedX, rotatedY, rotatedPentomino ) as rotated) =
            Pentomino.rotateCW model.currentPiece
    in
    if offBounds rotated then
        let
            movedInBounds : ( Int, Int, Pentomino )
            movedInBounds =
                if rotatedX < 0 then
                    ( 0, rotatedY, rotatedPentomino )

                else
                    -- We want
                    -- mvinbndX + Pentomino.width mvinbndPentomino == playingFieldWidth
                    -- mvinbndX = playingFieldWidth - Pentomino.width mvinbndPentomino
                    ( playingFieldWidth - Pentomino.width rotatedPentomino, rotatedY, rotatedPentomino )
        in
        moveUnlessCollides movedInBounds model

    else
        moveUnlessCollides rotated model


moveUnlessCollides : ( Int, Int, Pentomino ) -> PlayingModel -> PlayingModel
moveUnlessCollides newPiece model =
    if offBounds newPiece || collides newPiece model.grid then
        model

    else
        { model | currentPiece = newPiece }


offBounds : ( Int, Int, Pentomino ) -> Bool
offBounds ( x, _, pentomino ) =
    x < 0 || x + Pentomino.width pentomino > playingFieldWidth


moveDown : PlayingModel -> PlayingModel
moveDown model =
    let
        moved : ( Int, Int, Pentomino )
        moved =
            Pentomino.moveDown model.currentPiece
    in
    if collides moved model.grid then
        let
            ( newGrid, points ) =
                writePentominoInGrid model
                    |> removeFullLines

            step : Pentomino -> List Pentomino -> PlayingModel -> PlayingModel
            step newPiece newQueue m =
                { m
                    | grid = newGrid
                    , currentPiece = initialPentominoPosition model.nextPiece
                    , nextPiece = newPiece
                    , queue = newQueue
                    , score = model.score + points
                }
        in
        case model.queue of
            [] ->
                let
                    ( ( newPiece, queueHead, queueTail ), newSeed ) =
                        Random.step queueGenerator model.seed
                in
                step newPiece (queueHead :: queueTail) { model | seed = newSeed }

            newPiece :: newQueue ->
                step newPiece newQueue model

    else
        { model | currentPiece = moved }


removeFullLines : List (List Color) -> ( List (List Color), Int )
removeFullLines grid =
    let
        newGrid : List (List Color)
        newGrid =
            List.filter
                (\gridRow ->
                    List.member "" gridRow
                )
                grid

        newHeight : Int
        newHeight =
            List.length newGrid

        removed : Int
        removed =
            playingFieldHeight - newHeight

        newTop : List (List Color)
        newTop =
            List.repeat
                removed
                emptyGridRow
    in
    ( newTop ++ newGrid, removed * removed )


writePentominoInGrid : PlayingModel -> List (List Color)
writePentominoInGrid model =
    let
        ( pentominoX, pentominoY, ( pentominoColor, pentominoShape ) ) =
            model.currentPiece
    in
    List.foldl
        (\gridRow state ->
            if state.y < pentominoY then
                { inProgressGrid = gridRow :: state.inProgressGrid
                , y = state.y + 1
                , pentominoShape = state.pentominoShape
                }

            else
                case state.pentominoShape of
                    [] ->
                        { inProgressGrid = gridRow :: state.inProgressGrid
                        , y = state.y + 1
                        , pentominoShape = state.pentominoShape
                        }

                    firstPentominoRow :: restOfThePentomino ->
                        let
                            newRow : List Color
                            newRow =
                                List.foldl
                                    (\gridCell innerState ->
                                        if innerState.x < pentominoX then
                                            { inProgressRow = gridCell :: innerState.inProgressRow
                                            , x = innerState.x + 1
                                            , pentominoRow = innerState.pentominoRow
                                            }

                                        else
                                            case innerState.pentominoRow of
                                                [] ->
                                                    { inProgressRow = gridCell :: innerState.inProgressRow
                                                    , x = innerState.x + 1
                                                    , pentominoRow = innerState.pentominoRow
                                                    }

                                                pentominoHead :: pentominoTail ->
                                                    { inProgressRow =
                                                        (if pentominoHead then
                                                            pentominoColor

                                                         else
                                                            gridCell
                                                        )
                                                            :: innerState.inProgressRow
                                                    , x = innerState.x + 1
                                                    , pentominoRow = pentominoTail
                                                    }
                                    )
                                    { inProgressRow = []
                                    , x = 0
                                    , pentominoRow = firstPentominoRow
                                    }
                                    gridRow
                                    |> (\innerState -> List.reverse innerState.inProgressRow)
                        in
                        { inProgressGrid = newRow :: state.inProgressGrid
                        , y = state.y + 1
                        , pentominoShape = restOfThePentomino
                        }
        )
        { inProgressGrid = []
        , y = 0
        , pentominoShape = pentominoShape
        }
        model.grid
        |> (\state -> List.reverse state.inProgressGrid)


collides : ( Int, Int, Pentomino ) -> List (List Color) -> Bool
collides ( x, y, ( _, pentominoShape ) as pentomino ) grid =
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

        any2 : (a -> b -> Bool) -> List a -> List b -> Bool
        any2 f a b =
            List.any identity (List.map2 f a b)
    in
    if List.length gridSlice < pentominoHeight then
        -- Pentomino got offscreen at the bottom
        True

    else
        any2
            (\gridRow pentominoRow ->
                any2
                    (\gridCell pentominoCell ->
                        pentominoCell && (gridCell /= "")
                    )
                    gridRow
                    pentominoRow
            )
            gridSlice
            pentominoShape


playingModelGenerator : Random.Generator PlayingModel
playingModelGenerator =
    Random.map2
        (\seed ( currentPiece, nextPiece, queue ) ->
            { pause = False
            , score = 0
            , currentPiece = initialPentominoPosition currentPiece
            , nextPiece = nextPiece
            , queue = queue
            , grid = List.repeat playingFieldHeight emptyGridRow
            , seed = seed
            }
        )
        Random.independentSeed
        queueGenerator


queueGenerator : Random.Generator ( Pentomino, Pentomino, List Pentomino )
queueGenerator =
    Random.List.shuffle Theme.coloredPentominos
        |> Random.map
            (\list ->
                case list of
                    one :: two :: tail ->
                        ( one, two, tail )

                    _ ->
                        horribleHackPleaseForgiveMe ()
            )


emptyGridRow : List Color
emptyGridRow =
    List.repeat playingFieldWidth ""


initialPentominoPosition : Pentomino -> ( Int, Int, Pentomino )
initialPentominoPosition currentPiece =
    let
        currentPieceWidth : Int
        currentPieceWidth =
            Pentomino.width currentPiece
    in
    ( (playingFieldWidth - currentPieceWidth // 2)
        // 2
        - 1
    , 0
    , currentPiece
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
        , Browser.Events.onKeyDown
            (keyDecoder
                |> Json.Decode.map KeyDown
            )
        , Browser.Events.onKeyUp
            (keyDecoder
                |> Json.Decode.map KeyUp
            )
        ]


keyDecoder : Json.Decode.Decoder Key
keyDecoder =
    Json.Decode.field "key" Json.Decode.string
        |> Json.Decode.andThen
            (\key ->
                case key of
                    "Enter" ->
                        Json.Decode.succeed Enter

                    "ArrowLeft" ->
                        Json.Decode.succeed ArrowLeft

                    "ArrowUp" ->
                        Json.Decode.succeed ArrowUp

                    "ArrowDown" ->
                        Json.Decode.succeed ArrowDown

                    "ArrowRight" ->
                        Json.Decode.succeed ArrowRight

                    "a" ->
                        Json.Decode.succeed ArrowLeft

                    "w" ->
                        Json.Decode.succeed ArrowUp

                    "s" ->
                        Json.Decode.succeed ArrowDown

                    "d" ->
                        Json.Decode.succeed ArrowRight

                    " " ->
                        Json.Decode.succeed Space

                    "p" ->
                        Json.Decode.succeed Pause

                    _ ->
                        let
                            _ =
                                Debug.log "Ignored" key
                        in
                        Json.Decode.fail "Ignored"
            )


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
