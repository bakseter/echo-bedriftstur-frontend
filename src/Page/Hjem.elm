module Page.Hjem exposing (Model, init, title, toSession, view)

import Element exposing (Element, el, text)
import Session exposing (Session)


type Model
    = Model Session


init : Session -> Model
init =
    Model


view : Element msg
view =
    el [] <| text "Hjem"


title : String
title =
    "echo bedriftstur"


toSession : Model -> Session
toSession (Model session) =
    session
