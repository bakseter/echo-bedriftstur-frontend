module Main exposing (..)

import Browser
import Browser.Dom exposing (Viewport)
import Browser.Navigation as Nav
import Html exposing (Html, div, span, h1, h2, h3, text, br, a, img, button, i)
import Html.Attributes exposing (type_, value, id, class, href, style, src, alt, rel, target)
import Html.Events exposing (onClick, onInput, onMouseEnter, onMouseLeave)
import Url
import Url.Builder exposing (crossOrigin)
import String exposing (fromInt, append, concat, length)
import Time exposing (..)
import Tuple exposing (first, second)
import List exposing (map, map2)
import Animation exposing (none, block, inline, color, px, deg)
import Animation.Messenger exposing (send)
import Svg exposing (svg, line)
import Svg.Attributes exposing (x1, x2, y1, y2, width, height)

main =
    Browser.application {
        init = init,
        subscriptions = subscriptions,
        update = update,
        view = view,
        onUrlChange = UrlChanged,
        onUrlRequest = LinkClicked
    }

type Page
    = Hjem
    | Program 
    | Bedrifter
    | Om

type Name
    = Initial
    | Bekk
    | Computas
    | Mnemonic
    | Knowit
    | Dnb

type Msg
    = UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
    | Tick Time.Posix
    | Animate Animation.Msg
    | Animate2 Animation.Msg
    | Transition
    | LoadNav
    | NavBtnTransition


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , page : Page
    , time : Time.Posix
    , titleAnimation : Animation.Messenger.State Msg
    , navbarAnimation : Animation.Messenger.State Msg
    , navBtnAnimation : (Animation.Messenger.State Msg, Animation.Messenger.State Msg)
    , nav : Bool
    , name : Name
    , removeLineNavBtn : Bool
    }

init : () -> Url.Url -> Nav.Key -> (Model, Cmd Msg)
init _ url key =
    ({  key = key
     ,  url = url
     ,  page = Hjem
     ,  time = (millisToPosix 0)
     ,  titleAnimation = Animation.interrupt 
                            [ Animation.loop 
                                [ Animation.wait (millisToPosix 4000)
                                , Animation.to [ Animation.opacity 0 ]
                                , Animation.Messenger.send Transition
                                , Animation.wait (millisToPosix 1500)
                                , Animation.to [ Animation.opacity 1 ]
                                ] 
                            ] (Animation.style [ Animation.opacity 1 ])
     ,  navbarAnimation = Animation.style [ Animation.top (px -50) ]
     ,  navBtnAnimation = (Animation.style 
                            [ Animation.rotate (deg 0)
                            , Animation.translate (px 0) (px 0)
                            , Animation.scale 1.0 
                            ]
                         , Animation.style 
                            [ Animation.rotate (deg 0)
                            , Animation.translate (px 0) (px 0)
                            , Animation.scale 1.0 
                            ])
     ,  nav = False
     ,  name = Initial
     ,  removeLineNavBtn = False
    },
    Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch 
    [ Time.every 1000 Tick
    , Animation.subscription Animate [model.titleAnimation, model.navbarAnimation]
    , Animation.subscription Animate2 [first model.navBtnAnimation, second model.navBtnAnimation]
    ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    (model, Nav.pushUrl model.key (Url.toString url))
                Browser.External href ->
                    (model, Nav.load href) 
        UrlChanged url ->
            case url.path of
                "/hjem" ->
                    ({ model | url = url, page = Hjem }, Cmd.none)
                "/program" ->
                    ({ model | url = url, page = Program }, Cmd.none)
                "/bedrifter" ->
                    ({ model | url = url, page = Bedrifter }, Cmd.none) 
                "/om" ->
                    ({ model | url = url, page = Om }, Cmd.none)
                _ ->
                    ({ model | url = url, page = Hjem }, Cmd.none)
        Tick time ->
            ({ model | time = time }, Cmd.none)
        Animate anim ->
            let (newStyleTitleAnim, titleCmds) = Animation.Messenger.update anim model.titleAnimation
                (newStyleNavbarAnim, navbarCmds) = Animation.Messenger.update anim model.navbarAnimation
            in
               ({ model | titleAnimation = newStyleTitleAnim, navbarAnimation = newStyleNavbarAnim }, (titleCmds))
        Animate2 anim ->
            let (newStyleNavBtnAnim, navBtnCmds) = Animation.Messenger.update anim (first model.navBtnAnimation)
                (newStyleNavBtnAnim2, _) = Animation.Messenger.update anim (second model.navBtnAnimation)
            in
                ({ model | navBtnAnimation = (newStyleNavBtnAnim, newStyleNavBtnAnim2) }, navBtnCmds)
        Transition ->
            ({ model | name = getNextName model.name }, Cmd.none) 
        LoadNav ->
            if model.nav == False then
                ({ model
                    | nav = True
                    , navBtnAnimation = (Animation.interrupt 
                                             [ Animation.Messenger.send NavBtnTransition
                                             , Animation.to 
                                                [ Animation.translate (px 34) (px -8)
                                                ,  Animation.rotate (deg 45)
                                                ,  Animation.scale 0.7 
                                                ]
                                            ] 
                                            (first model.navBtnAnimation)
                                            , Animation.interrupt 
                                            [ Animation.to 
                                                [ Animation.translate (px -35) (px 6)
                                                , Animation.rotate (deg -45)
                                                , Animation.scale 0.7
                                                ]
                                            ] 
                                            (second model.navBtnAnimation)
                                        )
                }
                , Cmd.none)
            else
                ({ model
                    | nav = False
                    , navBtnAnimation = (Animation.interrupt 
                                            [ Animation.Messenger.send NavBtnTransition
                                            , Animation.to 
                                                [ Animation.translate (px 0) (px 0)
                                                ,  Animation.rotate (deg 0)
                                                ,  Animation.scale 1.0 
                                                ]
                                            ] 
                                            (first model.navBtnAnimation)
                                            , Animation.interrupt 
                                            [ Animation.to 
                                                [ Animation.translate (px 0) (px 0)
                                                , Animation.rotate (deg 0)
                                                , Animation.scale 1.0
                                                ]
                                            ] 
                                            (second model.navBtnAnimation)
                                        )
                    } 
                    , Cmd.none)
        NavBtnTransition ->
            if model.removeLineNavBtn
            then
                ({ model | removeLineNavBtn = False }, Cmd.none)
            else
                ({ model | removeLineNavBtn = True }, Cmd.none)

view : Model -> Browser.Document Msg
view model =
    {
        title = "echo bedriftstur",
        body =
            [
            div [ class "site" ] 
                [ div [ class "menu" ]
                    [ span [ id "hjem" ] 
                        [ a [ href "/" ] 
                            [ img [ id "logo", alt "logo", src "img/echo-logo-very-wide.png" ] [] ] 
                        ]
                    , span [ class "navbar", onClick LoadNav ]
                    [ svg [ width "100", height "100" ]
                        [ line (Animation.render (first model.navBtnAnimation)
                        ++ [ x1 "0", x2 "50", y1 "35", y2 "35", Svg.Attributes.style "stroke:rgb(125,125,125);stroke-width:4;" ]) []
                        , (getMiddleLine model.removeLineNavBtn)
                        , line (Animation.render (second model.navBtnAnimation) 
                            ++ [ x1 "0", x2 "50", y1 "65", y2 "65", Svg.Attributes.style "stroke:rgb(125,125,125);stroke-width:4;" ]) []
                        ] 
                    ]
                    , span [ class "menuItem", id "program" ] [ a [ href "/program" ] [ text "Program" ] ]
                    , span [ class "menuItem", id "bedrifter" ] [ a [ href "/bedrifter" ] [ text "Bedrifter" ] ]
                    , span [ class "menuItem", id "om" ] [ a [ href "/om" ] [ text "Om oss" ] ]
                ]
                , loadNav model
                , div [] (getPages model)
            ]
        ]
    }

getPages : Model -> List (Html Msg)
getPages model =
    case model.page of
        Hjem ->
            [getHjem model False] ++ [getProgram True] ++ [getBedrifter model True] ++ [getOm True]
        Program ->
            [getHjem model True] ++ [getProgram False] ++ [getBedrifter model True] ++ [getOm True]
        Bedrifter ->
            [getHjem model True] ++ [getProgram True] ++ [getBedrifter model False] ++ [getOm True]
        Om ->
            [getHjem model True] ++ [getProgram True] ++ [getBedrifter model True] ++ [getOm False]

getHjem : Model -> Bool -> Html Msg 
getHjem model hide =
    div [ if hide then class "hidden" else class "hjem" ] [
        div [ class "content" ] [
            div [ id "anim" ] [
                h1 [ id "anim-2" ] [ text "echo | " ],
                h1
                    (Animation.render model.titleAnimation ++ [ id "anim-text" ]) [ text (getNameString model.name) ]
            ],
            br [] [],
            div [ class "text" ] [ text "echo har startet en komité for å arrangere bedriftstur til Oslo høsten 2020." ],
            div [ class "text" ] [ text "Formålet med arrangementet er å gjøre våre informatikkstudenter kjent med karrieremulighetene i Oslo." ],
            br [] [],
            div [ class "text" ] [ text "Informasjon kommer fortløpende!" ],
            br [] [],
            br [] []
        ],
        getClock model
    ]

getClock : Model -> Html msg
getClock model =
    div [ class "clock" ] ([
        span [ id "days" ] [ text "D" ],
        span [ id "hours" ] [ text "H" ],
        span [ id "minutes" ] [ text "M" ],
        span [ id "seconds" ] [ text "S" ]
    ] ++ getCountDown model.time)

getProgram : Bool -> Html msg 
getProgram hide =
    div [ if hide then class "hidden" else  class "program" ]
        [ div [ id "onsdag" ]
            [ h1 [] [ text "Onsdag" ]
            , div [ class "program-item" ]
                [ div [ class "program-tab", id "mnemonic-tab" ] [ br [] [] ]
                , div [ class "program-content" ]
                    [ h1 [] [ text "mnemonic" ]
                    , h3 [] [ text "11:00 - 15:00" ]
                    , h3 [] [ a [ target "_blank", rel "noopener noreferrer", href "" ] [ text "Henrik Ibsens gate 100" ] ]
                    ]
                ]
            , div [ class "program-item" ]
                [ div [ class "program-tab", id "computas-tab" ] [ br [] [] ]
                , div [ class "program-content" ]
                    [ h1 [] [ text "Computas" ]
                    , h3 [] [ text "17:00 - 21:00" ]
                    , h3 [] [ a [ target "_blank", rel "noopener noreferrer", href "" ] [ text "Akersgata 35" ] ]
                    ]
                ]
            ]
        , div [ id "torsdag" ]
            [ h1 [] [ text "Torsdag" ]
            , div [ class "program-item" ]
                [ div [ class "program-tab", id "tba-tab" ] [ br [] [] ]
                , div [ class "program-content" ]
                    [ h1 [] [ text "To be announced" ]
                    , h3 [] [ text "11:00 - 15:00" ]
                    , h3 [] [ a [ target "_blank", rel "noopener noreferrer", href "" ] [ text "" ] ] 
                    , br [] []
                    , br [] []
                    ]
                ]
            , div [ class "program-item" ]
                [ div [ class "program-tab", id "knowit-tab" ] [ br [] [] ]
                , div [ class "program-content" ]
                    [ h1 [] [ text "Knowit" ]
                    , h3 [] [ text "17:00 - 21:00" ]
                    , h3 [] [  a [ target "_blank", rel "noopener noreferrer", href "" ] [ text "Lakkegata 53" ] ] 
                    ]
                ]
            ]
        , div [ id "fredag" ] 
            [ h1 [] [ text "Fredag" ]
            , div [ class "program-item" ]
                [ div [ class "program-tab", id "dnb-tab" ] [ br [] [] ]
                , div [ class "program-content" ]
                    [ h1 [] [ text "DNB" ] 
                    , h3 [] [ text "11:00 - 15:00" ]
                    , h3 [] [ a [ target "_blank", rel "noopener noreferrer", href "" ] [ text "Dronning Eufemias gate 30" ] ] 
                    ]
                ]
            , div [ class "program-item" ]
                [ div [ class "program-tab", id "bekk-tab" ] [ br [] [] ]
                , div [ class "program-content" ]
                    [ h1 [] [ text "Bekk" ]
                    , h3 [] [ text "17:00 - 21:00" ]
                    , h3 [] [ a [ target "_blank", rel "noopener noreferrer", href "" ] [ text "Skur 39 Akershusstranda 21" ] ] 
                    ]
                ]
            ]
        ]

getBedrifter : Model -> Bool -> Html Msg 
getBedrifter model hide =
    div [ if hide then class "hidden" else class "logos" ] [
        span [ class "logo-item", id "bekk" ] [ 
            a [ target "_blank", rel "noopener noreferrer", href "https://www.bekk.no" ] [
                img  [ class "bed-logo", src "img/bekk.png", alt "Bekk" ] [] 
            ]
         ],
        span [ class "logo-item", id "mnemonic" ] [
            a [ target "_blank", rel "noopener noreferrer", href "https://www.mnemonic.no" ] [
                img  [ class "bed-logo", src "img/mnemonic.png", alt "mnemonic" ] [] 
            ]
        ],
        span [ class "logo-item", id "DNB" ] [
            a [ target "_blank", rel "noopener noreferrer", href "https://www.dnb.no" ] [
                img  [ class "bed-logo", src "img/dnb.png", alt "DNB" ] [] 
            ]
        ],
        span [ class "logo-item", id "computas" ] [
            a [ target "_blank", rel "noopener noreferrer", href "https://computas.com" ] [
                img  [ class "bed-logo", src "img/computas.png", alt "Computas" ] [] 
            ]
        ],
        span [ class "logo-item", id "knowit" ] [
            a [ target "_blank", rel "noopener noreferrer", href "https://www.knowit.no" ] [
                img  [ class "bed-logo", src "img/knowit.png", alt "Knowit" ] [] 
            ]
        ],
        span [ class "logo-item", id "TBD" ] [
            a [ target "_blank", rel "noopener noreferrer", href "" ] [
                text "To be announced"
            ]
        ]
    ]

getOm : Bool -> Html msg 
getOm hide =
    div [ if hide then class "hidden" else class "om" ] [
        div [ id "om-tekst" ] [
            div [ class "text" ] [ text "echo består av 12 demokratisk valgte studenter. Vi er fagutvalget/linjeforeningen for informatikk ved Universitetet i Bergen, men har også et overordnet ansvar for studentsaker som angår det faglige ved instituttet. Vi jobber utelukkende med å gjøre studiehverdagen for oss informatikere bedre og er studentenes stemme opp mot instituttet, fakultetet og arbeidsmarkedet." ], 
            br [] [],
            div [ class "text" ] [ text "Vi representerer studenter under følgende bachelor- og masterprogram: Datateknologi, Data Science, Datasikkerhet, Bioinformatikk, Kognitiv Vitenskap, Informasjonsteknologi, Informatikk (master), Programvareutvikling (master)" ],
            br [] [],
            div [ class "text" ] [ text "Bedriftsturkomitéen består av 3 frivillige studenter." ]
        ],
        div [ id "elias" ] [ img [ class "portrett", src "img/elias.png", alt "elias" ] [] ],
        div [ id "elias-info" ] [
            div [ class "navn" ] [ text "Elias Djupesland" ],
            div [ class "tittel" ] [ text "Leder og bedriftskontakt" ],
            div [ class "mail" ] [ text "elias.djupesland@echo.uib.no" ]
        ],
        div [ id "andreas" ] [ img [ class "portrett", src "img/andreas.png", alt "andreas" ] [] ],
        div [ id "andreas-info" ] [
            div [ class "navn" ] [ text "Andreas Salhus Bakseter" ],
            div [ class "tittel" ] [ text "Web- og transportansvarlig" ],
            div [ class "mail" ] [ text "andreas.bakseter@echo.uib.no" ]
        ],
        div [ id "tuva" ] [ img [ class "portrett", src "img/tuva.png", alt "tuva" ] []],
        div [ id "tuva-info" ] [
            div [ class "navn" ] [ text "Tuva Kvalsøren" ],
            div [ class "tittel" ] [ text "Arrangøransvarlig" ],
            div [ class "mail" ] [ text "tuva.kvalsoren@echo.uib.no" ]
        ]
    ]

getCountDown : Posix -> List (Html msg)
getCountDown dateNow =
    let dateThen = 1598436000 * 1000
        date = dateThen - (posixToMillis dateNow)
    in
        if date == dateThen
        then (map (\x -> div [ class "clockItem", id ("clock" ++ second x) ] [ text (fixNum (fromInt (first x))) ]) [(0,"D"),(0,"H"),(0,"M"),(0,"S")]) 
        else (map (\x -> div [ class "clockItem", id ("clock" ++ second x) ] [ text (fixNum (fromInt (first x))) ]) (calcDate date))

fixNum : String -> String
fixNum str =
    if length str == 1
    then "0" ++ str
    else str

calcDate : Int -> List (Int, String)
calcDate diff =
    let day = diff // (86400 * 1000)
        dayMod = modBy (86400 * 1000) diff
        hour = dayMod // (3600 * 1000)
        hourMod = modBy (3600 * 1000) dayMod
        min = hourMod // (60 * 1000)
        minMod = modBy (60 * 1000) hourMod
        sec = minMod // 1000
    in
        [(day,"D"), (hour,"H"), (min,"M"), (sec,"S")]


loadNav : Model -> Html Msg
loadNav model =
    case model.nav of
        True ->
            div [ id "navbar-content" ] [
                a [ href "/bedrifter", onClick LoadNav ] [ text "Bedrifter" ],
                a [ href "/program", onClick LoadNav ] [ text "Program" ],
                a [ href "/om", onClick LoadNav ] [ text "Om oss" ]
            ]
        False ->
            span [] []

getNextName : Name -> Name
getNextName name =
    case name of
        Initial ->
            Bekk
        Bekk ->
            Computas
        Computas ->
            Mnemonic
        Mnemonic ->
            Knowit
        Knowit ->
            Dnb
        Dnb ->
            Initial

getNameString : Name -> String
getNameString name =
    case name of
        Initial ->
            "bedriftstur"
        Bekk ->
            "Bekk"
        Computas ->
            "Computas"
        Mnemonic ->
            "mnemonic"
        Knowit ->
            "Knowit"
        Dnb ->
            "DNB"

getMiddleLine : Bool -> Svg.Svg msg
getMiddleLine remove =
    if remove
    then 
        line [] []
    else
        line [ x1 "0", x2 "50", y1 "50", y2 "50", Svg.Attributes.style "stroke:rgb(125,125,125);stroke-width:4;" ] []
