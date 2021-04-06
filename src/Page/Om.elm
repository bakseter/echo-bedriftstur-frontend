module Page.Om exposing (route, title, view)

import Element exposing (..)
import Element.Font as Font
import Html
import Html.Attributes as HtmlA
import Theme
import Util


type alias Member =
    { name : String
    , title : String
    , email : String
    , image : String
    }


elias : Member
elias =
    { name = "Elias Djupesland"
    , title = "Leder"
    , email = "elias.djupesland@echo.uib.no"
    , image = "elias"
    }


andreas : Member
andreas =
    { name = "Andreas Salhus Bakseter"
    , title = "Webansvarlig"
    , email = "andreas.bakseter@echo.uib.no"
    , image = "andreas"
    }


tuva : Member
tuva =
    { name = "Tuva Kvalsøren"
    , title = "PR-ansvarlig"
    , email = "tuva.kvalsoeren@echo.uib.no"
    , image = "tuva"
    }


view : Device -> Element msg
view device =
    let
        members =
            if device.class == Desktop || device.class == BigDesktop then
                [ viewMember elias
                , row [ centerX, spacing 50 ]
                    [ viewMember andreas, viewMember tuva ]
                ]

            else
                List.map viewMember [ elias, andreas, tuva ]
    in
    wrappedRow [ spaceEvenly ]
        [ column [ centerX, spacing 50, width fill ]
            members
        , textColumn [ width fill, spacing 50, padding 40 ]
            [ paragraph [ spacing 20 ]
                [ text "Etter å ha blitt inspirert av andre linjeforeninger og etter dialog med vår hovedsamarbeidspartner, "
                , text "begynte vi å smått å planlegge echo bedriftstur høsten 2019. "
                , text "En bedriftstur til Oslo var et helt nytt konsept for informatikkstudentene i Bergen, "
                , text "men vi var gira på å få det til. "
                , text "Etter mye arbeid åpnet vi påmeldingen til echo sin første bedriftstur i april 2020, "
                , text "her på denne nettsiden. "
                , text "De 47 plassene ble fylt opp på "
                , el [ Font.bold, Font.underline ] <| text "under 4 sekunder"
                , text "!"
                ]
            , paragraph [ spacing 20 ]
                [ text "Selv med pandemien hengende over oss, "
                , text "gjorde vi tilpasninger og vi var forsatt innstilt på at turen var mulig å arrangere. "
                , text "Dessverre ble situasjonen verre, og vi måtte avlyse turen i august 2020, "
                , text "bare få uker før den skulle finne sted. "
                , text ""
                ]
            , paragraph [ spacing 20 ]
                [ text "Per våren 2021 ser vi fortsatt på muligheten for å arrangere en tur til høsten 2021 eller våren 2022. "
                , text "Dersom dere er en bedrift som vil være en del av vår neste bedriftstur, "
                , text "ta gjerne kontakt med oss på "
                , link [ Font.underline, Font.bold ]
                    { url = "mailto:kontakt@echobedriftstur.no"
                    , label = text "kontakt@echobedriftstur.no"
                    }
                , text " for mer informasjon!"
                ]
            ]
        ]


viewMember : Member -> Element msg
viewMember member =
    column [ centerX, spacing 10 ]
        [ el [ centerX, width (fill |> maximum 220) ] <|
            Element.html <|
                Html.img
                    [ HtmlA.src (Util.getPng member.image)
                    , HtmlA.alt member.name
                    , HtmlA.style "border-radius" "50%"
                    ]
                    []
        , Theme.h4 [ centerX ] <| member.name
        , Theme.h5 [ centerX ] <| member.title

        --, Theme.h5 [ centerX ] <| member.email
        ]


route : String
route =
    "om"


title : String
title =
    "Om oss"
