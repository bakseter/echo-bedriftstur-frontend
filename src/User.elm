module User exposing (User, decode, empty)

import Json.Encode as Encode
import Json.Decode as Decode

import Session exposing (Session)
import Degree exposing (Degree(..))
import Terms exposing (..)
import Email exposing (Email(..))
import Uid exposing (Uid(..))
import Ticket exposing (Ticket(..))

type alias User =
    { email : Email
    , firstName : String
    , lastName : String
    , degree : Degree
    , terms : Terms
    , hasTicket : Ticket
    , submittedTicket : Bool
    }

empty : User
empty =
    User (Email "") "" "" None (Terms False) (Ticket Nothing) False

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
    Decode.map7 User
        (Email.orNullDecoder "email")
        (stringOrNullDecoder "firstName")
        (stringOrNullDecoder "lastName")
        (Degree.orNullDecoder "degree")
        (Terms.orNullDecoder "terms")
        (Ticket.orNullDecoder "hasTicket")
        (boolOrNullDecoder "submittedTicket")

stringOrNullDecoder : String -> Decode.Decoder String
stringOrNullDecoder field =
    Decode.oneOf
        [ Decode.at [ field ] Decode.string
        , Decode.at [ field ] (Decode.null "")
        , Decode.succeed ""
        ]

boolOrNullDecoder : String -> Decode.Decoder Bool
boolOrNullDecoder field =
    Decode.oneOf
        [ Decode.at [ field ] Decode.bool
        , Decode.at [ field ] (Decode.null False)
        , Decode.succeed False
        ]
