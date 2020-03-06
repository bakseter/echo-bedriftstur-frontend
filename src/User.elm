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

userDecoder : Decode.Decoder User
userDecoder =
    Decode.map4 User
        (Email.orNullDecoder "email")
        (stringOrNullDecoder "firstName")
        (stringOrNullDecoder "lastName")
        (Degree.orNullDecoder "degree")

stringOrNullDecoder : String -> Decode.Decoder String
stringOrNullDecoder field =
    Decode.oneOf
        [ (Decode.at [ field ] Decode.string)
        , (Decode.at [ field ] (Decode.null ""))
        ]
