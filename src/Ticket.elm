module Ticket exposing (..)

import Json.Encode as Encode

import Session exposing (..)
import Uid exposing (toString)
import Email exposing (toString)

encode : Session -> Encode.Value
encode session =
    Encode.object
        [ ("collection", Encode.string "tickets")
        , ("uid", Encode.string (Uid.toString session.uid))
        , ("email", Encode.string (Email.toString session.email))
        ]
