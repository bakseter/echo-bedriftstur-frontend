module Terms exposing (Terms(..), fromAbsolute, fst, orNullDecoder, snd, thd, toBool)

import Array exposing (Array(..))
import Json.Decode as Decode



-- Type representing the terms accepted by the user.
-- Even though this is just a boolean value,
-- it is wrapped in its own type for the same reason the Email type is.


type Terms
    = Terms Bool Bool Bool


fromAbsolute : Bool -> Terms
fromAbsolute bool =
    if bool then
        Terms True True True

    else
        Terms False False False



-- Converts a Terms type to a boolean value.


toBool : Terms -> Bool
toBool (Terms b1 b2 b3) =
    b1 && b2 && b3


fst : Terms -> Bool
fst (Terms b _ _) =
    b


snd : Terms -> Bool
snd (Terms _ b _) =
    b


thd : Terms -> Bool
thd (Terms _ _ b) =
    b



-- Decoder for converting a JSON value to a Terms type.


orNullDecoder : String -> Decode.Decoder Terms
orNullDecoder field =
    Decode.oneOf
        [ Decode.map fromAbsolute (Decode.at [ field ] Decode.bool)
        , Decode.at [ field ] (Decode.null (Terms False False False))
        , Decode.succeed (Terms False False False)
        ]
