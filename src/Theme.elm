module Theme exposing (Pentomino, coloredPentominos)


type alias Pentomino =
    List (List Bool)


coloredPentominos : List ( String, Pentomino )
coloredPentominos =
    List.map2
        (\( _, color ) ( _, pentomino ) ->
            ( color, pentomino )
        )
        colors
        pentominos


colors : List ( String, String )
colors =
    [ ( "Sollux Captor", "#a1a100" )
    , ( "Dave Strider", "#e00707" )
    , ( "Aradia Megido", "#a10000" )
    , ( "Tavros Nitram", "#a15000" )
    , ( "Dirk Strider", "#f2a400" )
    , ( "Jade Harley", "#4ac925" )
    , ( "Kanaya Maryam", "#008141" )
    , ( "Terezi Pyrope", "#008282" )
    , ( "Vriska Serket", "#005682" )
    , ( "Nepeta Leijon", "#416600" )
    , ( "Karkat Vantas", "#626262" )
    , ( "John Egbert", "#0715cd" )
    , ( "Equius Zahhak", "#000056" )
    , ( "Gamzee Makara", "#2b0057" )
    , ( "Rose Lalonde", "#b536da" )
    , ( "Eridan Ampora", "#6a006a" )
    , ( "Roxy Lalonde", "#ff6ff2" )
    , ( "Feferi Peixes", "#77003c" )
    ]


pentominos : List ( String, Pentomino )
pentominos =
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
