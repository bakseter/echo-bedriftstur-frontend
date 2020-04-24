module Content exposing (..)

import Json.Encode as Encode

import Degree exposing (..)
import Terms exposing (..)
import Session exposing (..)
import Uid exposing (..)
import Email exposing (..)

type alias Content =
    { firstName : String
    , lastName : String
    , degree : Degree
    , terms : Terms
    }

empty : Content
empty =
    Content "" "" None (Terms False)

updateAll : String -> String -> Degree -> Terms -> Content -> Content
updateAll firstName lastName degree terms content =
    updateFirstName firstName content
        |> updateLastName lastName
        |> updateDegree degree
        |> updateTerms terms

updateFirstName : String -> Content -> Content
updateFirstName firstName content =
    { content | firstName = firstName }

updateLastName : String -> Content -> Content
updateLastName lastName content =
    { content | lastName = lastName }

updateDegree : Degree -> Content -> Content
updateDegree degree content =
    { content | degree = degree }

updateTerms : Terms -> Content -> Content
updateTerms terms content =
    { content | terms = terms }

encode : Session -> Content -> Encode.Value
encode session content =
    Encode.object
        [ ("collection", Encode.string "users")
        , ("uid", Encode.string (Uid.toString session.uid))
        , ("email", Encode.string (Email.toString session.email))
        , ("firstName", Encode.string content.firstName)
        , ("lastName", Encode.string content.lastName)
        , ("degree", Encode.string (Degree.toString False content.degree))
        , ("terms", Encode.bool (Terms.toBool content.terms))
        , ("hasTicket", Encode.null)
        ]
