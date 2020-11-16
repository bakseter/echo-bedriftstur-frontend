module Util exposing (..)

import Json.Decode as Decode


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
