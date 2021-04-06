module Error exposing (Error(..), decode, toString)

import Json.Decode as Decode


type Error
    = GenericError


decode : Decode.Value -> Maybe Error
decode json =
    case Decode.decodeValue decoder json of
        Ok err ->
            err

        Err _ ->
            Nothing


decoder : Decode.Decoder (Maybe Error)
decoder =
    Decode.map fromString Decode.string


fromString : String -> Maybe Error
fromString str =
    case str of
        "generic-error" ->
            Just GenericError

        _ ->
            Nothing


toString : Error -> String
toString error =
    case error of
        GenericError ->
            "generic-error"
