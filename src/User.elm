module User exposing (..)

import Json.Encode as Encode
import Json.Decode as Decode

import Session exposing (..)
import Degree exposing (..)
import Email exposing (..)
import Uid exposing (..)

type alias User =
    { email : Email
    , firstName : String
    , lastName : String
    , degree : Degree
    }

empty =
    { email = Email ""
    , firstName = ""
    , lastName = ""
    , degree = None
    }

encode : Session -> User -> Encode.Value
encode session user =
    Encode.object
        [ ("collection", Encode.string "users")
        , ("uid", Encode.string (Uid.toString session.uid))
        , ("email", Encode.string (Email.toString user.email))
        , ("firstName", Encode.string user.firstName)
        , ("lastName", Encode.string user.lastName)
        , ("degree", Encode.string (Degree.toString False user.degree))  
        ]

-- Uses the contentDecoder function to turn
-- a JSON object into a User record.
decode : Encode.Value -> User
decode json =
    let jsonStr = Encode.encode 0 json
    in
        case Decode.decodeString userDecoder jsonStr of
            Ok val ->
                val
            Err err ->
                { email = Email ""
                , firstName = ""
                , lastName = ""
                , degree = None
                }

-- Decoder that turns a JSON object into a Content record,
-- if the object is formatted correctly.
-- Fails if not all fields required for
-- a User record are present in the JSON.
userDecoder : Decode.Decoder User
userDecoder =
    Decode.map4 User
        (Email.orNullDecoder "email")
        (stringOrNullDecoder "firstName")
        (stringOrNullDecoder "lastName")
        (Degree.orNullDecoder "degree")

-- Decoder that either decodes the string at the given field,
-- or returns en empty string if the field is null.
stringOrNullDecoder : String -> Decode.Decoder String
stringOrNullDecoder field =
    Decode.oneOf
        [ (Decode.at [ field ] Decode.string)
        , (Decode.at [ field ] (Decode.null ""))
        ]
