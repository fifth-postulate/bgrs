module Main exposing (..)

import BackGammon as BG exposing (..)


main =
    let
        board =
            Board
                { stones =
                    [ ( 1, Red, 2 )
                    , ( 6, Blue, 5 )
                    , ( 8, Blue, 3 )
                    , ( 12, Red, 5 )
                    , ( 13, Blue, 5 )
                    , ( 18, Red, 3 )
                    , ( 20, Red, 5 )
                    , ( 24, Blue, 2 )
                    ]
                }
    in
    BG.view board
