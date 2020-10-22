module Session exposing (ApiKey, Session, decodeApiKey)

import Browser.Navigation
import Json.Decode as Decode
import Json.Encode as Encode


type ApiKey
    = ApiKey String


decodeApiKey : Encode.Value -> ApiKey
decodeApiKey json =
    case Decode.decodeValue Decode.string json of
        Ok apiKeyStr ->
            ApiKey apiKeyStr

        Err _ ->
            ApiKey "faen"


type alias Session =
    { navKey : Browser.Navigation.Key
    , apiKey : ApiKey
    }
