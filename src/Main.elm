module Main exposing (..)

import Html exposing (Html, text)
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
                |> rotate (90 + 45)

        bigBlue =
            triangle 2
                |> rotate 45

        littleOrange =
            triangle 1
                |> rotate -(90 + 45)
    in
        svg [ viewBox "-2 -2 6 6" ]
            [ bigGrey |> draw colors.grey
            , bigBlue |> draw colors.blue
            , littleOrange |> draw colors.orange
            ]


triangle : Float -> List Point
triangle size =
    [ ( 0, 0 )
    , ( size, 0 )
    , ( 0, size )
    ]


rotate : Float -> List Point -> List Point
rotate angle pnts =
    let
        rad =
            degrees angle

        rotate' ( x, y ) =
            ( x * cos rad - y * sin rad
            , x * sin rad + y * cos rad
            )
    in
        List.map rotate' pnts


draw : String -> List Point -> Svg msg
draw color pnts =
    polygon
        [ pnts
            |> List.map (\( x, y ) -> [ x, y ])
            |> List.concat
            |> List.map (\n -> toString n)
            |> String.join (",")
            |> points
        , fill color
        ]
        []
