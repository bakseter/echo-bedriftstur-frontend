module Session exposing (Session, decodeApiKey, keyToString)

import Browser.Navigation
import Json.Decode as Decode


type ApiKey
    = ApiKey String


type alias Session =
    { navKey : Browser.Navigation.Key
    , apiKey : ApiKey
    }


keyToString : ApiKey -> String
keyToString (ApiKey str) =
    str


decodeApiKey : Decode.Value -> ApiKey
decodeApiKey json =
    case Decode.decodeValue Decode.string json of
        Ok str ->
            ApiKey str

        Err err ->
            ApiKey <| Decode.errorToString err
