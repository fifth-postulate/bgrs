module BackGammon.Board exposing (Board, Error(..), empty, fromKey, initial, toKey, view)

import Array
import BackGammon.Board.Style exposing (Style)
import BackGammon.Id.Position as Id exposing (Key)
import BackGammon.Player as Player exposing (Player(..))
import Bit exposing (Bit(..))
import Bit.Pattern as Pattern exposing (Pattern)
import Bytes exposing (Endianness(..))
import Bytes.Decode exposing (decode)
import Bytes.Encode exposing (encode)
import Dict exposing (Dict)
import Svg exposing (Svg)
import Svg.Attributes as Attribute
import Svg.Color as Color exposing (Color, Name(..))
import Svg.Length exposing (px)
import Svg.Transform as Transform exposing (Transform)


type Board
    = Board
        { alpha : Dict Point Nat
        , omega : Dict Point Nat
        }


empty : Board
empty =
    Board { alpha = Dict.empty, omega = Dict.empty }


initial : Board
initial =
    empty
        |> placeCheckersAt Alpha 24 2
        |> placeCheckersAt Alpha 13 5
        |> placeCheckersAt Alpha 8 3
        |> placeCheckersAt Alpha 6 5
        |> placeCheckersAt Omega 24 2
        |> placeCheckersAt Omega 13 5
        |> placeCheckersAt Omega 8 3
        |> placeCheckersAt Omega 6 5


placeCheckersAt : Player -> Point -> Nat -> Board -> Board
placeCheckersAt player p n (Board b) =
    case player of
        Alpha ->
            Board
                { b | alpha = Dict.insert p n b.alpha }

        Omega ->
            Board
                { b | omega = Dict.insert p n b.omega }


type alias Point =
    Int


type alias Nat =
    Int


fromKey : Player -> Key -> Result Error Board
fromKey current key =
    if 10 == Bytes.width key then
        key
            |> decode (Pattern.decoder LE 10)
            |> Result.fromMaybe NoBitPattern
            |> Result.andThen (fromPattern current)

    else
        let
            width =
                Bytes.width key
        in
        Err <| IncorrectWidth width


type Error
    = IncorrectEncoding Id.Error
    | IncorrectWidth Int
    | NoBitPattern
    | IncorrectPatternWidth Int


fromPattern : Player -> Pattern -> Result Error Board
fromPattern current ps =
    let
        width =
            List.length ps
    in
    if 80 == width then
        let
            first =
                List.take 40 ps

            second =
                List.drop 40 ps
        in
        empty
            |> fillFromPattern current first
            |> fillFromPattern (Player.opponent current) second
            |> Ok

    else
        Err <| IncorrectPatternWidth width


fillFromPattern : Player -> Pattern -> Board -> Board
fillFromPattern =
    tailrec_fill 1 0


tailrec_fill : Point -> Nat -> Player -> Pattern -> Board -> Board
tailrec_fill p n current pattern b =
    case pattern of
        [] ->
            b

        bit :: tail ->
            case bit of
                Zero ->
                    if n > 0 then
                        tailrec_fill (p + 1) 0 current tail (placeCheckersAt current p n b)

                    else
                        tailrec_fill (p + 1) 0 current tail b

                One ->
                    tailrec_fill p (n + 1) current tail b


toKey : Player -> Board -> Key
toKey current (Board { alpha, omega }) =
    let
        cs =
            case current of
                Alpha ->
                    [ alpha, omega ]

                Omega ->
                    [ omega, alpha ]
    in
    cs
        |> List.concatMap toPattern
        |> Pattern.encoder LE
        |> encode


toPattern : Dict Point Nat -> Pattern
toPattern cs =
    let
        checkersAt index =
            Dict.get index cs
                |> Maybe.withDefault 0

        run count =
            0
                :: List.repeat count 1
                |> List.reverse

        toBit b =
            Bit.fromInt b
                |> Maybe.withDefault Bit.zero
    in
    List.range 1 25
        |> List.map checkersAt
        |> List.concatMap run
        |> List.map toBit


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
        [ Svg.g [] <| List.map (checkerStack style Alpha) <| Dict.toList b.alpha
        , Svg.g [] <| List.map (checkerStack style Omega) <| Dict.toList b.omega
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
