module Main exposing (..)

import BackGammon as BG exposing (..)


main =
    let
        board =
            Board
                { stones =
                    [ ( 1, Alpha, 2 )
                    , ( 6, Omega, 5 )
                    , ( 8, Omega, 3 )
                    , ( 12, Alpha, 5 )
                    , ( 13, Omega, 5 )
                    , ( 18, Alpha, 3 )
                    , ( 20, Alpha, 5 )
                    , ( 24, Omega, 2 )
                    ]
                }
    in
    BG.view board
