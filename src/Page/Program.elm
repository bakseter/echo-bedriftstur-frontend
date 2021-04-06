module Page.Program exposing (route, title, view)

import Element exposing (Element, centerX, column, padding)
import Theme


view : Element msg
view =
    column [ centerX, padding 100 ]
        [ Theme.h1 [] "Kommer snart!" ]


route : String
route =
    "program"


title : String
title =
    "Program"
