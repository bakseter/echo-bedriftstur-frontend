module Session exposing (Session, decodeApiKey, emptyKey, keyToString)

import Browser.Navigation
import Cred exposing (Cred)
import Json.Decode as Decode


type ApiKey
    = ApiKey String


type alias Session =
    { navKey : Browser.Navigation.Key
    , apiKey : ApiKey
    , cred : Maybe Cred
    }


keyToString : ApiKey -> String
keyToString (ApiKey str) =
    str


emptyKey : ApiKey
emptyKey =
    ApiKey ""


decodeApiKey : Decode.Value -> Maybe ApiKey
decodeApiKey json =
    case Decode.decodeValue Decode.string json of
        Ok str ->
            Just <| ApiKey str

        Err err ->
            Just <| ApiKey <| Decode.errorToString err
