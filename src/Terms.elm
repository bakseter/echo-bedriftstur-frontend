module Terms exposing (..)

import Json.Decode as Decode
import Array exposing (Array(..))

type Terms
    = Terms Bool

toBool : Terms -> Bool
toBool (Terms bool) =
    bool

orNullDecoder : String -> Decode.Decoder Terms
orNullDecoder field =
    Decode.oneOf
        [ Decode.map Terms (Decode.at [ field ] Decode.bool)
        , Decode.at [ field ] (Decode.null (Terms False))
        , Decode.succeed (Terms False)
        ]
