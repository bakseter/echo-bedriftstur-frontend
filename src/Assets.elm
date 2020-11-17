module Assets exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode


type Assets
    = Assets String String


decode : Encode.Value -> Maybe (List Assets)
decode json =
    case Decode.decodeValue assetsDecoder json of
        Ok assets ->
            Just assets

        Err _ ->
            Nothing


get : List Assets -> String -> String
get assets key =
    case assets of
        (Assets k v) :: xs ->
            if key == k then
                v

            else
                get xs key

        [] ->
            ""


assetsDecoder : Decode.Decoder (List Assets)
assetsDecoder =
    Decode.map (List.map (\( x, y ) -> Assets x y)) <| Decode.keyValuePairs Decode.string
