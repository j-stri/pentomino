module Pentomino exposing (Pentomino, list, rotateCW)

import List.Extra


type alias Pentomino =
    ( String, List (List Bool) )


rotateCW : Pentomino -> Pentomino
rotateCW ( color, pentomino ) =
    ( color
    , pentomino
        |> List.Extra.transpose
        |> List.map List.reverse
    )


list : List Pentomino
list =
    [ ( "I", List.repeat 5 [ True ] )
    , ( "F"
      , [ [ False, True, True ]
        , [ True, True, False ]
        , [ False, True, False ]
        ]
      )
    , ( "F'"
      , [ [ True, True, False ]
        , [ False, True, True ]
        , [ False, True, False ]
        ]
      )
    , ( "L"
      , [ [ True, False ]
        , [ True, False ]
        , [ True, False ]
        , [ True, True ]
        ]
      )
    , ( "L'"
      , [ [ False, True ]
        , [ False, True ]
        , [ False, True ]
        , [ True, True ]
        ]
      )
    , ( "N"
      , [ [ False, True ]
        , [ False, True ]
        , [ True, True ]
        , [ True, False ]
        ]
      )
    , ( "N'"
      , [ [ True, False ]
        , [ True, False ]
        , [ True, True ]
        , [ False, True ]
        ]
      )
    , ( "P"
      , [ [ True, True ]
        , [ True, True ]
        , [ True, False ]
        ]
      )
    , ( "P'"
      , [ [ True, True ]
        , [ True, True ]
        , [ False, True ]
        ]
      )
    , ( "T"
      , [ [ True, True, True ]
        , [ False, True, False ]
        , [ False, True, False ]
        ]
      )
    , ( "U"
      , [ [ True, False, True ]
        , [ True, True, True ]
        ]
      )
    , ( "V"
      , [ [ True, False, False ]
        , [ True, False, False ]
        , [ True, True, True ]
        ]
      )
    , ( "W"
      , [ [ True, False, False ]
        , [ True, True, False ]
        , [ False, True, True ]
        ]
      )
    , ( "X"
      , [ [ False, True, False ]
        , [ True, True, True ]
        , [ False, True, False ]
        ]
      )
    , ( "Y"
      , [ [ False, True ]
        , [ True, True ]
        , [ False, True ]
        , [ False, True ]
        ]
      )
    , ( "Y'"
      , [ [ True, False ]
        , [ True, True ]
        , [ True, False ]
        , [ True, False ]
        ]
      )
    , ( "Z"
      , [ [ False, True, True ]
        , [ False, True, False ]
        , [ True, True, False ]
        ]
      )
    , ( "Z'"
      , [ [ True, True, False ]
        , [ False, True, False ]
        , [ False, True, True ]
        ]
      )
    ]
        |> List.map rotateCW
