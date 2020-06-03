module Uid exposing (Uid(..), orNullDecoder, toString)

import Json.Decode as Decode



-- Type representing a user id.
-- The user id string is wrapped in a type for
-- the same reasons as Email is.


type Uid
    = Uid String



-- Converts a Uid type to a string.


toString : Uid -> String
toString (Uid uid) =
    uid



-- Decoder for converting a JSON value to a Uid type.


orNullDecoder : String -> Decode.Decoder Uid
orNullDecoder field =
    Decode.oneOf
        [ Decode.map Uid (Decode.at [ field ] Decode.string)
        , Decode.at [ field ] (Decode.null (Uid ""))
        ]
