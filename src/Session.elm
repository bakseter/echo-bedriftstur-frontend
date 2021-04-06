module Session exposing (Session, decodeSession, keyToString)

import Browser.Navigation
import Element exposing (Device, DeviceClass(..), Orientation(..), classifyDevice)
import Json.Decode as Decode


type ApiKey
    = ApiKey String


type alias Session =
    { navKey : Browser.Navigation.Key
    , apiKey : ApiKey
    , device : Device
    }


keyToString : ApiKey -> String
keyToString (ApiKey str) =
    str


decodeSession : Decode.Value -> ( ApiKey, Device )
decodeSession json =
    case Decode.decodeValue decoder json of
        Ok str ->
            str

        Err err ->
            ( ApiKey <| Decode.errorToString err, { class = Desktop, orientation = Landscape } )


decoder : Decode.Decoder ( ApiKey, Device )
decoder =
    Decode.map3 (\a w h -> ( a, classifyDevice { width = w, height = h } ))
        (Decode.map ApiKey <| Decode.field "apiKey" Decode.string)
        (Decode.field "width" Decode.int)
        (Decode.field "height" Decode.int)
