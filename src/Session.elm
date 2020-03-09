module Session exposing (Session, isSignedIn, encode, decode)

import Json.Encode as Encode
import Json.Decode as Decode

import Uid exposing (Uid(..))
import Email exposing (Email(..))

type alias Session =
    { uid : Uid
    , email : Email
    }

isSignedIn : Session -> Bool
isSignedIn session =
    (Uid.toString session.uid) /= "" &&
    (Email.toString session.email) /= ""

encode : Session -> Encode.Value
encode user =
    Encode.object
        [ ("collection", Encode.string "users")
        , ("uid", Encode.string (Uid.toString user.uid))
        , ("email", Encode.string (Email.toString user.email))
        ]
                      
-- Uses the userDecoder function to turn
-- a JSON object into a Session record.
decode : Encode.Value -> Session
decode json =
    let jsonStr = Encode.encode 0 json
    in 
        case Decode.decodeString sessionDecoder jsonStr of
            Ok val ->
                val
            Err err ->
                { uid = Uid ""
                , email = Email ""
                }

-- Decoder that turns a JSON object into a Session record,
-- if the object is formatted correctly.
-- Fails if not all the fields required for
-- a User record are present in the JSON.
sessionDecoder : Decode.Decoder Session
sessionDecoder =
    Decode.map2 Session
        (Uid.orNullDecoder "uid")
        (Email.orNullDecoder "email")
