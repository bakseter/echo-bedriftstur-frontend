module User exposing (User, decode)

import Json.Encode as Encode
import Json.Decode as Decode

import Session exposing (Session)
import Degree exposing (Degree(..))
import Email exposing (Email(..))
import Uid exposing (Uid(..))
import Ticket exposing (Ticket(..))

type alias User =
    { email : Email
    , firstName : String
    , lastName : String
    , degree : Degree
    , hasTicket : Ticket
    }

-- Uses the contentDecoder function to turn
-- a JSON object into a User record.
decode : Encode.Value -> Maybe User
decode json =
    let jsonStr = Encode.encode 0 json
    in
        case Decode.decodeString userDecoder jsonStr of
            Ok user ->
                Just user
            Err _ ->
                Nothing

userDecoder : Decode.Decoder User
userDecoder =
    Decode.map5 User
        (Email.orNullDecoder "email")
        (stringOrNullDecoder "firstName")
        (stringOrNullDecoder "lastName")
        (Degree.orNullDecoder "degree")
        (Ticket.orNullDecoder "hasTicket")

stringOrNullDecoder : String -> Decode.Decoder String
stringOrNullDecoder field =
    Decode.oneOf
        [ (Decode.at [ field ] Decode.string)
        , (Decode.at [ field ] (Decode.null ""))
        ]
