module Page.Hjem exposing (title, view)

import Element exposing (Element, above, alignBottom, below, centerX, column, el, fill, height, inFront, padding)
import Element.Font as Font
import Element.Region as Region
import Svg
import Svg.Attributes
import Theme
import Util


view : Element msg
view =
    column
        [ centerX
        , height fill
        ]
        [ Theme.h1 [ padding 50 ] "Bedriftstur til Oslo for informatikkstudenter ved UiB"
        , Theme.h3 [ centerX, padding 10, Font.regular ] "Er du nysgjerrig på jobbmulighetene i Oslo etter endt utdanning?"
        , Theme.h3 [ centerX, padding 10, Font.regular ] "Bedriftsturkomitéen arrangerer besøk hos bedrifter i hovedstaden til høsten!"
        , el [ padding 10, centerX, alignBottom ] barcode
        ]


barcode : Element msg
barcode =
    Element.html <|
        Svg.svg
            [ Svg.Attributes.viewBox "0 0 800 350"
            , Svg.Attributes.width "800px"
            , Svg.Attributes.preserveAspectRatio "xMidYMid meet"
            ]
        <|
            List.map
                (\props ->
                    Svg.rect
                        [ Svg.Attributes.fill (Util.toHexColor props.color)
                        , Svg.Attributes.x props.x
                        , Svg.Attributes.y props.y
                        , Svg.Attributes.width props.w
                        , Svg.Attributes.height props.h
                        , Svg.Attributes.style "hover: sale(1.2);"
                        ]
                        []
                )
                [ { x = "0", y = "153", w = "66", h = "207", color = Theme.yellow1 }
                , { x = "91", y = "84", w = "59", h = "276", color = Theme.blue1 }
                , { x = "150", y = "46", w = "56", h = "314", color = Theme.yellow2 }
                , { x = "238", y = "93", w = "49", h = "367", color = Theme.blue2 }
                , { x = "346", y = "44", w = "66", h = "316", color = Theme.yellow1 }
                , { x = "425", y = "143", w = "44", h = "217", color = Theme.blue1 }
                , { x = "469", y = "130", w = "46", h = "230", color = Theme.yellow2 }
                , { x = "542", y = "58", w = "108", h = "302", color = Theme.blue2 }
                , { x = "660", y = "116", w = "65", h = "244", color = Theme.yellow1 }
                , { x = "725", y = "75", w = "50", h = "285", color = Theme.blue1 }
                ]


title : String
title =
    "echo bedriftstur"
