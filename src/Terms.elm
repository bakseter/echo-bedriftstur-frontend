module Terms exposing (..)

import Array exposing (Array(..))
import Json.Decode as Decode



-- Type representing the terms accepted by the user.
-- Even though this is just a boolean value,
-- it is wrapped in its own type for the same reason the Email type is.


type Terms
    = Terms Bool



-- Converts a Terms type to a boolean value.


toBool : Terms -> Bool
toBool (Terms bool) =
    bool



-- Decoder for converting a JSON value to a Terms type.


orNullDecoder : String -> Decode.Decoder Terms
orNullDecoder field =
    Decode.oneOf
        [ Decode.map Terms (Decode.at [ field ] Decode.bool)
        , Decode.at [ field ] (Decode.null (Terms False))
        , Decode.succeed (Terms False)
        ]
