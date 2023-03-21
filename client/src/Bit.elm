module Bit exposing (Bit(..), fromInt, one, toInt, zero)


type Bit
    = Zero
    | One


zero : Bit
zero =
    Zero


one : Bit
one =
    One


toInt : Bit -> Int
toInt bit =
    case bit of
        Zero ->
            0

        One ->
            1


fromInt : Int -> Maybe Bit
fromInt value =
    case value of
        0 ->
            Just Zero

        1 ->
            Just One

        _ ->
            Nothing
