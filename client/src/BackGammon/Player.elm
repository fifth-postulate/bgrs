module BackGammon.Player exposing (Player(..), opponent)


type Player
    = Alpha
    | Omega


opponent : Player -> Player
opponent player =
    case player of
        Alpha ->
            Omega

        Omega ->
            Alpha
