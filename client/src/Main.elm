module Main exposing (main)

import BackGammon.Board as Board exposing (Board)
import BackGammon.Board.Style exposing (default)
import BackGammon.Id.Position as Position
import BackGammon.Player exposing (Player(..))
import Html exposing (Html)


main =
    view Board.initial


view : Board -> Html msg
view board =
    let
        position =
            board
                |> Board.toKey Alpha
                |> Position.encode
    in
    Html.div []
        [ Html.div [] [ Html.span [] [ Html.text position ] ]
        , Board.view default board
        ]
