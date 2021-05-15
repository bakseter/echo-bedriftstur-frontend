module Session exposing (Session, decodeSession)

import Browser.Navigation
import Element exposing (Device, DeviceClass(..), Orientation(..), classifyDevice)
import Json.Decode as Decode


type alias Session =
    { navKey : Browser.Navigation.Key
    , device : Device
    }


decodeSession : Decode.Value -> Device
decodeSession json =
    case Decode.decodeValue decoder json of
        Ok str ->
            str

        Err _ ->
            { class = Desktop, orientation = Landscape }


decoder : Decode.Decoder Device
decoder =
    Decode.map2 (\w h -> classifyDevice { width = w, height = h })
        (Decode.field "width" Decode.int)
        (Decode.field "height" Decode.int)
