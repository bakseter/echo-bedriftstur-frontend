module Ticket exposing (Ticket(..), encode, orNullDecoder, toBool)

import Cred exposing (Cred)
import Email exposing (toString)
import Json.Decode as Decode
import Json.Encode as Encode
import Uid exposing (toString)



-- Type representing if the user has gotten a "ticket" to the trip or not.


type Ticket
    = Ticket (Maybe Bool)



-- Converts a Ticket type to either a boolean value, or nothing.


toBool : Ticket -> Maybe Bool
toBool (Ticket bool) =
    bool



-- Encodes a ticket as a JSON value.
-- This is used when the user is signed up for the trip.
-- The JSON value is sent to Firestore when createTicket in the Verified module is called.


encode : Cred -> Encode.Value
encode cred =
    Encode.object
        [ ( "collection", Encode.string "users" )
        , ( "uid", Encode.string (Uid.toString cred.uid) )
        , ( "submittedTicket", Encode.bool True )
        ]



-- Decoder for converting a JSON value to a Ticket type.


orNullDecoder : String -> Decode.Decoder Ticket
orNullDecoder field =
    Decode.oneOf
        [ Decode.map Ticket (Decode.at [ field ] (Decode.nullable Decode.bool))
        , Decode.at [ field ] (Decode.null (Ticket Nothing))
        , Decode.succeed (Ticket Nothing)
        ]
