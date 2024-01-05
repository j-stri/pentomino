module Types exposing (Flags, Key(..), LostModel, Model(..), Msg(..), PlayingModel)

import Pentomino exposing (Color, Pentomino)
import Random


type alias Flags =
    ()


type Model
    = Welcome
    | Playing PlayingModel
    | Lost LostModel


type alias PlayingModel =
    { pause : Bool
    , score : Int
    , currentPiece : ( Int, Int, Pentomino )
    , nextPiece : Pentomino
    , queue : List Pentomino
    , grid : List (List Color)
    , seed : Random.Seed
    , downArrowPressed : Bool
    }


type alias LostModel =
    { nextPiece : Pentomino
    , score : Int
    , grid : List (List Color)
    }


type Msg
    = KeyDown Key
    | KeyUp Key
    | Generated PlayingModel
    | Tick


type Key
    = Enter
    | ArrowLeft
    | ArrowRight
    | ArrowUp
    | ArrowDown
    | Space
    | Pause
