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



triangle size color =
  polygon
    [  [ (0, 0)
      , (size, 0)
      , (0, size)
      ]
        |> List.concatMap (\(x,y) -> [x, y])
        |> List.map toString
        |> String.join ","
        |> points
    , fill color
    ]
    []

main =
  svg [ viewBox "-2 -2 10 10"]
    [ triangle 4 colors.grey
    , triangle 3 colors.orange
    , triangle 2 colors.blue
    , triangle 1 colors.green

    ]
