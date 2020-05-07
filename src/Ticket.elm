module Ticket exposing (Ticket(..), orNullDecoder, encode, toBool)

import Session exposing (Session)
import Uid exposing (toString)
import Email exposing (toString)

import Json.Encode as Encode
import Json.Decode as Decode

-- Type representing if the user has gotten a "ticket" to the trip or not.
type Ticket
    = Ticket (Maybe Bool)

-- Converts a Ticket type to either a boolean value, or nothing.
toBool : Ticket -> (Maybe Bool)
toBool (Ticket bool) = bool

-- Encodes a ticket as a JSON value.
-- This is used when the user is signed up for the trip.
-- The JSON value is sent to Firestore when createTicket in the Verified module is called.
encode : Session -> Encode.Value
encode session =
    Encode.object
        [ ("collection", Encode.string "users")
        , ("uid", Encode.string (Uid.toString session.uid))
        , ("submittedTicket", Encode.bool True)
        ]

-- Decodes a Ticket type from a JSON value.
-- If the decoder succeeds, returns a Ticket type.
-- If not, returns Nothing.
decode : Encode.Value -> Maybe Ticket
decode json =
    let jsonStr = Encode.encode 0 json
    in
        case Decode.decodeString (orNullDecoder "hasTicket") jsonStr of
            Ok ticket ->
                Just ticket
            Err _ ->
                Nothing

-- Decoder for converting a JSON value to a Ticket type.
orNullDecoder : String -> Decode.Decoder Ticket
orNullDecoder field =
    Decode.oneOf
        [ Decode.map Ticket (Decode.at [ field ] (Decode.nullable Decode.bool))
        , Decode.at [ field ] (Decode.null (Ticket Nothing))
        , Decode.succeed (Ticket Nothing)
        ]
