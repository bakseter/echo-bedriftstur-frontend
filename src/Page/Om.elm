module Page.Om exposing (route, title, view)

import Element exposing (Element, text)


view : Element msg
view =
    text "Om"


route : String
route =
    "om"


title : String
title =
    "Om oss"
