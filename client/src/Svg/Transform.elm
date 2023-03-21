module Svg.Transform exposing (Transform, flipX, flipY, identity, sequence, toString, translate)


type Transform
    = Identity
    | Translate Float Float
    | Scale Float Float
    | Sequence (List Transform)


identity : Transform
identity =
    Identity


translate : Float -> Float -> Transform
translate =
    Translate


flipX : Transform
flipX =
    Scale -1 1


flipY : Transform
flipY =
    Scale 1 -1


sequence : List Transform -> Transform
sequence =
    Sequence


toString : Transform -> String
toString transform =
    case transform of
        Identity ->
            ""

        Scale x y ->
            [ "scale("
            , String.fromFloat x
            , ","
            , String.fromFloat y
            , ")"
            ]
                |> String.concat

        Translate x y ->
            [ "translate("
            , String.fromFloat x
            , ","
            , String.fromFloat y
            , ")"
            ]
                |> String.concat

        Sequence transforms ->
            List.map toString transforms
                |> String.join " "
