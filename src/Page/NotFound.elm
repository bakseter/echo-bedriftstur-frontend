module Page.NotFound exposing (Model, init, title, view)

import Element exposing (Element, el, text)
import Session exposing (Session)


type Model
    = Model Session


init : Session -> Model
init =
    Model


view : Element msg
view =
    el [] <| text "404 not found"


title : String
title =
    "404"
