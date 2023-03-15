module BackGammon.Board.Style exposing (Style, default)

import Array exposing (Array)
import Svg.Color as Color exposing (Color, Name(..))


type alias Style =
    { enclosure : Color
    , felt : Color
    , point : Array Color
    , alpha : Color
    , omega : Color
    }


default : Style
default =
    { enclosure = Color.byName GoldenRod
    , felt = Color.byName Green
    , point = Array.fromList [ Color.byName White, Color.byName Black ]
    , alpha = Color.byName LightGray
    , omega = Color.byName DarkGray
    }
