module BackGammon exposing (..)

import Html exposing (Html)


type Board
    = Board
        { stones : List ( Point, Player, Nat )
        }


type alias Point =
    Int


type Player
    = Red
    | Blue


type alias Nat =
    Int


view : Board -> Html msg
view (Board { stones }) =
    Html.text <| Debug.toString stones
