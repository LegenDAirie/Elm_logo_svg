module Main exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import String


colors =
    { grey = "#5a6378"
    , green = "#83c833"
    , orange = "#efa500"
    , blue = "#5fb4ca"
    }


type alias Point =
    ( Float, Float )


main : Svg msg
main =
    let
        bigGrey =
            triangle 2
                |> rotate (45 + 180)

        bigBlue =
            triangle 2
                |> rotate -45

        littleOrange =
            triangle 1
                |> rotate (90 + 45)

        littleBlue =
            triangle (sqrt 2)
                |> rotate -90
                |> snap 3 (to greenParallelagram 4)

        greenParallelagram =
            parallelagram
                |> rotate -45
                |> snap 1 (to bigGrey 3)

        greenSquare =
            square
                |> rotate 45

        littleOrange2 =
            triangle 1
                |> rotate 45
                |> snap 1 (to greenSquare 4)
    in
        svg [ viewBox "-2 -2 10 10" ]
            [ bigGrey |> draw colors.grey
            , bigBlue |> draw colors.blue
            , littleOrange |> draw colors.orange
            , greenParallelagram |> draw colors.green
            , littleBlue |> draw colors.blue
            , greenSquare |> draw colors.green
            , littleOrange2 |> draw colors.orange
            ]


to : List Point -> Int -> Point
to pnts pointNumber =
    pnts
        |> List.drop (pointNumber - 1)
        |> List.head
        |> Maybe.withDefault ( 0, 0 )


snap : Int -> Point -> List Point -> List Point
snap pointNumber snapTarget pnts =
    case
        pnts
            |> List.drop (pointNumber - 1)
            |> List.head
    of
        Just pivot ->
            pnts
                |> sub pivot
                |> add snapTarget

        Nothing ->
            pnts


sub : Point -> List Point -> List Point
sub ( x, y ) =
    add ( -x, -y )


add : Point -> List Point -> List Point
add ( dx, dy ) pnts =
    List.map (\( x, y ) -> ( x + dx, y + dy )) pnts


rotate : Float -> List Point -> List Point
rotate angle pnts =
    let
        rad =
            degrees angle

        rotate' ( x, y ) =
            ( cos rad * x + sin rad * y
            , sin rad * -x + cos rad * y
            )
    in
        List.map rotate' pnts


square : List Point
square =
    [ ( 0, 0 )
    , ( 1, 0 )
    , ( 1, 1 )
    , ( 0, 1 )
    ]


triangle : Float -> List Point
triangle size =
    [ ( 0, 0 )
    , ( size, 0 )
    , ( 0, size )
    ]


parallelagram : List Point
parallelagram =
    [ ( 0, 0 )
    , ( 1, 0 )
    , ( 2, -1 )
    , ( 1, -1 )
    ]


draw : String -> List Point -> Svg msg
draw color pnts =
    polygon
        [ pnts
            |> List.map (\( x, y ) -> [ x, y ])
            |> List.concat
            |> List.map toString
            |> String.join (",")
            |> points
        , fill color
        , stroke "white"
        , strokeWidth "0.1"
        ]
        []
