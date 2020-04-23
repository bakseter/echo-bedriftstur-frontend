module Ticket exposing (Ticket(..), orNullDecoder, encode, toBool)

import Session exposing (Session)
import Uid exposing (toString)
import Email exposing (toString)

import Json.Encode as Encode
import Json.Decode as Decode

type Ticket =
    Ticket Bool

toBool : Ticket -> Bool
toBool (Ticket bool) = bool

encode : Session -> Encode.Value
encode session =
    Encode.object
        [ ("collection", Encode.string "tickets")
        , ("uid", Encode.string (Uid.toString session.uid))
        , ("email", Encode.string (Email.toString session.email))
        ]

decode : Encode.Value -> Maybe Ticket
decode json =
    let jsonStr = Encode.encode 0 json
    in
        case Decode.decodeString (orNullDecoder "hasTicket") jsonStr of
            Ok ticket ->
                Just ticket
            Err _ ->
                Nothing

orNullDecoder : String -> Decode.Decoder Ticket
orNullDecoder field =
    Decode.oneOf
        [ Decode.map Ticket (Decode.at [ field ] Decode.bool)
        , Decode.at [ field ] (Decode.null (Ticket False))
        ]
