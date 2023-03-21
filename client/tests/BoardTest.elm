module BoardTest exposing (suite)

import BackGammon.Board as Board
import BackGammon.Id.Position as Id
import BackGammon.Player exposing (Player(..))
import Expect
import Test exposing (..)


suite : Test
suite =
    describe "Board"
        [ describe "fromKey"
            [ test "4HPwATDgc/ABMA is the id of the initial board" <|
                \_ ->
                    let
                        actual =
                            "4HPwATDgc/ABMA"
                                |> Id.decode
                                |> Result.mapError Board.IncorrectEncoding
                                |> Result.andThen (Board.fromKey Alpha)

                        expected =
                            Ok Board.initial
                    in
                    Expect.equal actual expected
            ]
        , describe "toKey"
            [ test "the id of the initial board is 4HPwATDgc/ABMA" <|
                \_ ->
                    let
                        actual =
                            Board.initial
                                |> Board.toKey Alpha
                                |> Id.encode

                        expected =
                            "4HPwATDgc/ABMA"
                    in
                    Expect.equal actual expected
            ]
        ]
