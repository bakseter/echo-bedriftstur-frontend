module Content exposing (..)

import Json.Encode as Encode

import Degree exposing (..)
import Session exposing (..)
import Uid exposing (..)
import Email exposing (..)

type alias Content =
    { firstName : String
    , lastName : String
    , degree : Degree
    }

empty =
    { firstName = ""
    , lastName = ""
    , degree = None
    }

updateAll : Content -> String -> String -> Degree -> Content
updateAll content firstName lastName degree =
    updateFirstName firstName content
        |> updateLastName lastName
        |> updateDegree degree

updateFirstName : String -> Content -> Content
updateFirstName firstName content =
    { content | firstName = firstName }

updateLastName : String -> Content -> Content
updateLastName lastName content =
    { content | lastName = lastName }

updateDegree : Degree -> Content -> Content
updateDegree degree content =
    { content | degree = degree }

encode : Session -> Content -> Encode.Value
encode session content =
    Encode.object
        [ ("collection", Encode.string "users")
        , ("uid", Encode.string (Uid.toString session.uid))
        , ("email", Encode.string (Email.toString session.email))
        , ("firstName", Encode.string content.firstName)
        , ("lastName", Encode.string content.lastName)
        , ("degree", Encode.string (Degree.toString False content.degree))
        ]
