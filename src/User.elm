module User exposing (User, decode)

import Content exposing (Content)
import Degree exposing (Degree(..))
import Email exposing (Email(..))
import Json.Decode as Decode
import Json.Encode as Encode
import Terms exposing (Terms(..))
import Ticket exposing (Ticket(..))
import Uid exposing (Uid(..))
import Util


type alias User =
    { email : Email
    , content : Content
    , hasTicket : Ticket
    , submittedTicket : Bool
    , ticketNumber : Int
    }



-- Uses the contentDecoder function to turn
-- a JSON object into a User record.


decode : Encode.Value -> Maybe User
decode json =
    let
        jsonStr =
            Encode.encode 0 json
    in
    case Decode.decodeString userDecoder jsonStr of
        Ok user ->
            Just user

        Err _ ->
            Nothing



-- Decoder for converting a JSON object to a User record


userDecoder : Decode.Decoder User
userDecoder =
    Decode.map5 User
        (Email.orNullDecoder "email")
        Content.contentDecoder
        (Ticket.orNullDecoder "hasTicket")
        (Util.boolOrNullDecoder "submittedTicket")
        (Util.intOrNulllDecoder "ticketNumber")
