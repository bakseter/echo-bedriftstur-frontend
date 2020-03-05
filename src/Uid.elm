module Uid exposing (..)

import Json.Decode as Decode

type Uid
    = Uid String

toString : Uid -> String
toString (Uid uid) =
    uid

orNullDecoder field =
    Decode.oneOf
        [ Decode.map Uid (Decode.at [ field ] Decode.string)
        , Decode.at [ field ] (Decode.null (Uid ""))
        ] 
