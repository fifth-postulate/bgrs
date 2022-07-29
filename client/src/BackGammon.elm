module BackGammon exposing (..)

import String exposing (left)
import Svg exposing (Svg, a)
import Svg.Attributes as Attribute
import Svg.Color as Color exposing (Color, Name(..))
import Svg.Transform as Transform exposing (Transform)


type Board
    = Board
        { stones : List ( Point, Player, Nat )
        }


type alias Point =
    Int


type Player
    = Alpha
    | Omega


type alias Nat =
    Int


view : Board -> Svg msg
view (Board { stones }) =
    Svg.svg [ Attribute.width "840px", Attribute.height "440px", Attribute.viewBox "0 0 14 12" ]
        [ background ]


background : Svg msg
background =
    Svg.g []
        [ enclosure
        , left
        , right
        ]


enclosure : Svg msg
enclosure =
    Svg.rect [ Attribute.width "14", Attribute.height "12", Attribute.fill "goldenrod" ] []


left : Svg msg
left =
    board <| Transform.translate 0.5 0.5


right : Svg msg
right =
    board <| Transform.translate 7.5 0.5


board : Transform -> Svg msg
board transform =
    Svg.g [ Attribute.transform <| Transform.toString transform ]
        [ felt 
        , halfBoard <| Transform.identity 
        , halfBoard <| Transform.sequence [Transform.translate 6 11, Transform.flipX, Transform.flipY] 
        ]

felt : Svg msg
felt =
    let
        color = Color.byName Green
    in
    Svg.rect [Attribute.width "6", Attribute.height "11", Attribute.fill <| Color.toString color] []

halfBoard : Transform -> Svg msg
halfBoard transform =
    Svg.g [ Attribute.transform <| Transform.toString transform ]
        [ point (Color.byName White) <| Transform.translate 0 0
        , point (Color.byName Black) <| Transform.translate 1 0
        , point (Color.byName White) <| Transform.translate 2 0
        , point (Color.byName Black) <| Transform.translate 3 0
        , point (Color.byName White) <| Transform.translate 4 0
        , point (Color.byName Black) <| Transform.translate 5 0
        ]




point : Color -> Transform -> Svg msg
point color transform =
    let
        toString ( x, y ) =
            [ String.fromFloat x, ",", String.fromFloat y ] |> String.join ""

        points =
            [ ( 0, 0 )
            , ( 1, 0 )
            , ( 0.5, 5 )
            ]
                |> List.map toString
                |> String.join " "
    in
    Svg.polygon [ Attribute.transform <| Transform.toString transform, Attribute.points points, Attribute.fill <| Color.toString color ]
        []
