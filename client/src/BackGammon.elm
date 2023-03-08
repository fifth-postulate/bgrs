module BackGammon exposing (..)

import String exposing (left)
import Svg exposing (Svg)
import Svg.Attributes as Attribute
import Svg.Color as Color exposing (Color, Name(..))
import Svg.Transform as Transform exposing (Transform)


type Board
    = Board
        { checkers : List ( Point, Player, Nat )
        }


type alias Point =
    Int


type Player
    = Alpha
    | Omega


type alias Nat =
    Int


view : Board -> Svg msg
view aBoard =
    let
        dimension =
            60

        width =
            14 * dimension

        height =
            12 * dimension

        px : Int -> String
        px n =
            String.fromInt n ++ "px"
    in
    Svg.svg [ Attribute.width <| px width, Attribute.height <| px height, Attribute.viewBox "0 0 14 12" ]
        [ background
        , checkers aBoard
        ]


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
        , halfBoard <| Transform.sequence [ Transform.translate 6 11, Transform.flipX, Transform.flipY ]
        ]


felt : Svg msg
felt =
    let
        color =
            Color.byName Green
    in
    Svg.rect [ Attribute.width "6", Attribute.height "11", Attribute.fill <| Color.toString color ] []


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


checkers : Board -> Svg msg
checkers (Board b) =
    Svg.g [] <| List.map checkerStack b.checkers


checkerStack : ( Point, Player, Nat ) -> Svg msg
checkerStack ( p, player, n ) =
    let
        color =
            case player of
                Alpha ->
                    Color.byName Color.LightGray

                Omega ->
                    Color.byName Color.DarkGray

        transform =
            if p <= 6 then
                Transform.translate (toFloat <| p - 1) 0

            else if p <= 12 then
                Transform.translate (toFloat p) 0

            else if p <= 18 then
                Transform.sequence [ Transform.translate 13 12, Transform.flipX, Transform.flipY, Transform.translate (toFloat <| p - 13 - 1) 0 ]

            else
                Transform.sequence [ Transform.translate 13 12, Transform.flipX, Transform.flipY, Transform.translate (toFloat <| p - 13) 0 ]
    in
    List.range 1 n
        |> List.map checker
        |> Svg.g [ Attribute.transform <| Transform.toString transform, Attribute.fill <| Color.toString color ]


checker : Nat -> Svg msg
checker n =
    let
        cx =
            String.fromFloat 1

        cy =
            String.fromFloat <| toFloat n

        radius =
            String.fromFloat 0.475

        strokeWidth =
            String.fromFloat <| 1.0 / 25.0

        color =
            Color.byName Black
    in
    Svg.circle [ Attribute.cx cx, Attribute.cy cy, Attribute.r radius, Attribute.stroke <| Color.toString color, Attribute.strokeWidth strokeWidth ] []
