module User exposing (User, decode, empty)

import Degree exposing (Degree(..))
import Email exposing (Email(..))
import Json.Decode as Decode
import Json.Encode as Encode
import Terms exposing (Terms(..))
import Ticket exposing (Ticket(..))
import Uid exposing (Uid(..))



{-
   Type representing a user.
   Contains all the important information such as
       email : the email of the user
       firstName : the first name of the user
       lastName : the lastName of the user
       degree : the current academic course the user is enrolled in
       hasTicket : if the user has a ticket to the trip or not
       submittedTicket : if the user has "submitted a ticket", aka if the user has signed up for the trip
       ticketNumber : if the user is on the waiting list for a ticket, this is the place the user has in that list

   This type is encoded as a JSON object and sent to Firestore when the users updates their information,
   and decoded from a JSON object when the user is logged in (when the users info is retrieved from Firestore).
-}


type alias User =
    { email : Email
    , firstName : String
    , lastName : String
    , degree : Degree
    , terms : Terms
    , hasTicket : Ticket
    , submittedTicket : Bool
    , ticketNumber : Int
    }



-- Returns an empty User record


empty : User
empty =
    User (Email "") "" "" None (Terms False) (Ticket Nothing) False -1



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
    Decode.map8 User
        (Email.orNullDecoder "email")
        (stringOrNullDecoder "firstName")
        (stringOrNullDecoder "lastName")
        (Degree.orNullDecoder "degree")
        (Terms.orNullDecoder "terms")
        (Ticket.orNullDecoder "hasTicket")
        (boolOrNullDecoder "submittedTicket")
        (intOrNulllDecoder "ticketNumber")


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


intOrNulllDecoder : String -> Decode.Decoder Int
intOrNulllDecoder field =
    Decode.oneOf
        [ Decode.at [ field ] Decode.int
        , Decode.at [ field ] (Decode.null -1)
        , Decode.succeed -1
        ]
