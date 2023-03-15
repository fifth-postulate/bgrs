module Main exposing (..)

import BackGammon.Board as Board
import BackGammon.Board.Style exposing (default)


main =
    Board.view default Board.initial
