module Page.NotFound exposing (Model, init, title, toSession, view)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class, id)
import Session exposing (Session)


type Model
    = Model Session


init : Session -> Model
init =
    Model


view : Html msg
view =
    div [ class "not-found" ]
        [ div [ id "not-found-header" ] [ text "404" ]
        , div [ id "not-found-text" ] [ text "Siden du leter etter eksisterer ikke." ]
        ]


title : String
title =
    "404"


toSession : Model -> Session
toSession (Model session) =
    session
