module Page.NotFound exposing (title, view)

import Element exposing (Element, text)


view : Element msg
view =
    text "404 Not Found"


title : String
title =
    "404"
