module User exposing (User, decode, userDecoder)

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


userDecoder : Decode.Decoder User
userDecoder =
    Decode.map5 User
        (Decode.map Email <| Decode.field "email" Decode.string)
        Content.contentDecoder
        (Decode.map Ticket <| Decode.map Just <| Decode.field "hasTicket" Decode.bool)
        (Decode.field "submittedTicket" Decode.bool)
        (Decode.field "ticketNumber" Decode.int)
