module Theme exposing (background, coloredPentominos)

import Pentomino exposing (Pentomino)


coloredPentominos : List Pentomino
coloredPentominos =
    List.map2
        (\( _, color ) ( _, pentomino ) ->
            ( color, pentomino )
        )
        colors
        Pentomino.list


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


background : String
background =
    "#323232"
