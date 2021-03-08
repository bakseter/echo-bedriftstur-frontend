module Util exposing (..)

import Json.Decode as Decode


getPng : String -> String
getPng str =
    "/assets/" ++ str ++ ".png"


edges : { top : Int, right : Int, bottom : Int, left : Int }
edges =
    { top = 0
    , right = 0
    , bottom = 0
    , left = 0
    }


stringOrNullDecoder : String -> Decode.Decoder String
stringOrNullDecoder field =
    Decode.oneOf
        [ Decode.at [ field ] Decode.string
        , Decode.at [ field ] (Decode.null "")
        , Decode.succeed ""
        ]


boolOrNullDecoder : String -> Decode.Decoder Bool
boolOrNullDecoder field =
    Decode.oneOf
        [ Decode.at [ field ] Decode.bool
        , Decode.at [ field ] (Decode.null False)
        , Decode.succeed False
        ]


intOrNulllDecoder : String -> Decode.Decoder Int
intOrNulllDecoder field =
    Decode.oneOf
        [ Decode.at [ field ] Decode.int
        , Decode.at [ field ] (Decode.null -1)
        , Decode.succeed -1
        ]
