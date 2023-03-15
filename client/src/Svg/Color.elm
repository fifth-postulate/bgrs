module Svg.Color exposing (Color, Name(..), byName, toString)


type Color
    = ByName Name


type Name
    = Red
    | Blue
    | White
    | Black
    | DarkGray
    | Gray
    | Green
    | LightGray


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

        DarkGray ->
            grayPercentage 30

        Gray ->
            grayPercentage 50

        Green ->
            "green"

        LightGray ->
            grayPercentage 80


grayPercentage : Int -> String
grayPercentage percentage =
    rgbPercentage percentage percentage percentage


rgbPercentage : Int -> Int -> Int -> String
rgbPercentage r b g =
    let
        toPercentage v =
            String.fromInt v ++ "%"

        values =
            [ r, g, b ]
                |> List.map toPercentage
                |> String.join ", "
    in
    "rgb(" ++ values ++ ")"
