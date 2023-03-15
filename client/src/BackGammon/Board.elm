module BackGammon.Board exposing (Board, empty, initial, view)

import Array
import BackGammon.Board.Style exposing (Style)
import BackGammon.Id.Position exposing (Key)
import BackGammon.Player exposing (Player(..))
import String exposing (left)
import Svg exposing (Svg)
import Svg.Attributes as Attribute
import Svg.Color as Color exposing (Color, Name(..))
import Svg.Length exposing (px)
import Svg.Transform as Transform exposing (Transform)


type Board
    = Board
        { alpha : List ( Point, Nat )
        , omega : List ( Point, Nat )
        }


empty : Board
empty =
    Board { alpha = [], omega = [] }


initial : Board
initial =
    empty
        |> checkersAt Alpha 24 2
        |> checkersAt Alpha 13 5
        |> checkersAt Alpha 8 3
        |> checkersAt Alpha 6 5
        |> checkersAt Omega 24 2
        |> checkersAt Omega 13 5
        |> checkersAt Omega 8 3
        |> checkersAt Omega 6 5


checkersAt : Player -> Point -> Nat -> Board -> Board
checkersAt player p n (Board b) =
    case player of
        Alpha ->
            Board
                { b | alpha = ( p, n ) :: b.alpha }

        Omega ->
            Board
                { b | omega = ( p, n ) :: b.omega }


type alias Point =
    Int


type alias Nat =
    Int


fromKey : Key -> Board
fromKey key =
    empty


view : Style -> Board -> Svg msg
view style aBoard =
    let
        dimension =
            60

        width =
            px <| standard.width * dimension

        height =
            px <| standard.height * dimension

        viewbox =
            [ 0, 0, standard.width, standard.height ]
                |> List.map String.fromInt
                |> String.join " "
    in
    Svg.svg
        [ Attribute.width width
        , Attribute.height height
        , Attribute.viewBox viewbox
        ]
        [ background style
        , checkers style aBoard
        ]


standard : Dimensions
standard =
    { width = 14, height = 12 }


type alias Dimensions =
    { width : Int
    , height : Int
    }


background : Style -> Svg msg
background style =
    Svg.g []
        [ enclosure style
        , outerBoard style
        , homeBoard style
        ]


enclosure : Style -> Svg msg
enclosure style =
    Svg.rect
        [ Attribute.width <| String.fromInt standard.width
        , Attribute.height <| String.fromInt standard.height
        , Attribute.fill <| Color.toString style.enclosure
        ]
        []


outerBoard : Style -> Svg msg
outerBoard style =
    board style <| Transform.translate 0.5 0.5


homeBoard : Style -> Svg msg
homeBoard style =
    board style <| Transform.translate 7.5 0.5


board : Style -> Transform -> Svg msg
board style transform =
    Svg.g [ Attribute.transform <| Transform.toString transform ]
        [ felt style
        , halfBoard style <| Transform.identity
        , halfBoard style <| Transform.sequence [ Transform.translate 6 11, Transform.flipX, Transform.flipY ]
        ]


felt : Style -> Svg msg
felt style =
    Svg.rect
        [ Attribute.width <| String.fromInt 6
        , Attribute.height <| String.fromInt 11
        , Attribute.fill <| Color.toString style.felt
        ]
        []


halfBoard : Style -> Transform -> Svg msg
halfBoard style transform =
    let
        pointColor index =
            style.point
                |> Array.get (modBy 2 index)
                |> Maybe.withDefault (Color.byName Red)

        toPoint ( index, dx ) =
            point (pointColor index) <| Transform.translate dx 0
    in
    List.range 0 5
        |> List.map toFloat
        |> List.indexedMap Tuple.pair
        |> List.map toPoint
        |> Svg.g [ Attribute.transform <| Transform.toString transform ]


point : Color -> Transform -> Svg msg
point color transform =
    let
        toString ( x, y ) =
            [ x, y ]
                |> List.map String.fromFloat
                |> String.join ","

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


checkers : Style -> Board -> Svg msg
checkers style (Board b) =
    Svg.g []
        [ Svg.g [] <| List.map (checkerStack style Alpha) b.alpha
        , Svg.g [] <| List.map (checkerStack style Omega) b.omega
        ]


checkerStack : Style -> Player -> ( Point, Nat ) -> Svg msg
checkerStack style player ( index, n ) =
    let
        color =
            case player of
                Alpha ->
                    style.alpha

                Omega ->
                    style.omega

        p =
            case player of
                Alpha ->
                    index

                Omega ->
                    25 - index

        checkerPlacement =
            if p <= 6 then
                Transform.translate (toFloat <| p - 1) 0

            else if p <= 12 then
                Transform.translate (toFloat p) 0

            else if p <= 18 then
                Transform.sequence [ Transform.translate 13 12, Transform.flipX, Transform.flipY, Transform.translate (toFloat <| p - 13 - 1) 0 ]

            else
                Transform.sequence [ Transform.translate 13 12, Transform.flipX, Transform.flipY, Transform.translate (toFloat <| p - 13) 0 ]

        transform =
            Transform.sequence [ Transform.translate 14 0, Transform.flipX, checkerPlacement ]
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
    Svg.circle
        [ Attribute.cx cx
        , Attribute.cy cy
        , Attribute.r radius
        , Attribute.stroke <| Color.toString color
        , Attribute.strokeWidth strokeWidth
        ]
        []
