module Api exposing (Key, buildFireStoreEndpoint, decodeKey)

import Api.Endpoint exposing (Endpoint(..))
import Json.Decode as Decode
import Json.Encode as Encode
import Url.Builder as Builder


type Key
    = Key String


decodeKey : Encode.Value -> Key
decodeKey json =
    case Decode.decodeValue Decode.string json of
        Ok apiKeyStr ->
            Key apiKeyStr

        Err _ ->
            Key "faen"


buildFireStoreEndpoint : List String -> List Builder.QueryParameter -> Endpoint
buildFireStoreEndpoint path queries =
    Database <|
        Builder.crossOrigin "https://echo-bedriftstur.firebaseio.com" path queries
