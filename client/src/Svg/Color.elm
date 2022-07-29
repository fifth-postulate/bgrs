module Svg.Color exposing (Color, Name(..), byName, toString)


type Color
    = ByName Name


type Name
    = Red
    | Blue
    | White
    | Black
    | Green


byName : Name -> Color
byName =
    ByName


toString : Color -> String
toString color =
    case color of
        ByName name ->
            nameToString name


nameToString : Name -> String
nameToString name =
    case name of
        Red ->
            "red"

        Blue ->
            "blue"

        White ->
            "white"

        Black ->
            "black"

        Green ->
            "green"
