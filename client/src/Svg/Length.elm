module Svg.Length exposing (px)


px : Int -> String
px value =
    String.fromInt value ++ "px"
