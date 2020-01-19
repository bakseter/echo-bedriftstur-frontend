module Main exposing (..)

import Browser
import Browser.Dom exposing (Viewport)
import Browser.Navigation as Nav
import Html exposing (Html, div, span, h1, h2, h3, h4, h4, text, br, a, img, button, i)
import Html.Attributes exposing (type_, value, id, class, href, style, src, alt, rel, target)
import Html.Events exposing (onClick, onInput)
import Url
import Url.Builder exposing (crossOrigin)
import String exposing (fromInt, append, concat, length)
import Time exposing (..)
import Tuple exposing (first, second)
import List exposing (map, map2)
import Animation exposing (px)

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

type Msg
    = UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
    | Tick Time.Posix
    | Animate Animation.Msg
    | Fade
    | LoadNav

type alias Model =
    { 
        key : Nav.Key,
        url : Url.Url,
        page : Page,
        time : Time.Posix,
        style : Animation.State,
        nav : Bool
    }

init : () -> Url.Url -> Nav.Key -> (Model, Cmd Msg)
init _ url key =
    ({
        key = key, 
        url = url,
        page = Hjem,
        time = (millisToPosix 0),
        style = Animation.style [ Animation.left (px 0.0), Animation.opacity 1.0 ],
        nav = False
    },
    Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [Time.every 1000 Tick, Animation.subscription Animate [model.style]]

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
            ({ model | style = Animation.update anim model.style}, Cmd.none)
        Fade ->
            ({ model | style = Animation.interrupt [ Animation.to [ Animation.opacity 0 ], Animation.to [ Animation.opacity 1 ] ] model.style }, Cmd.none)
        LoadNav ->
            if model.nav == False then
                ({ model | nav = True }, Cmd.none)
            else
                ({ model | nav = False }, Cmd.none)

view : Model -> Browser.Document Msg
view model =
    {
        title = "echo bedriftstur",
        body =
            [
            div [ class "site" ] [
                {-span [] [
                    text "echo | "
                ],
                span 
                    (Animation.render model.style ++ [ Html.Events.onMouseOver Fade]) [ text "bed" ],
                span 
                    (Animation.render model.style ++ [ Html.Events.onMouseOver Fade]) [ text "rif" ],
                span 
                    (Animation.render model.style ++ [ Html.Events.onMouseOver Fade]) [ text "tst" ],
                span 
                    (Animation.render model.style ++ [ Html.Events.onMouseOver Fade]) [ text "ur" ],
                br [] [],
                -}
                div [ class "menu" ] [
                    span [ id "hjem" ] [ a [ href "/" ] [ img [ id "logo", alt "logo", src "img/echo-logo-very-wide.png" ] [] ] ],
                    span [ class "navbar" ] [ button [ id "navBtn", onClick LoadNav ] [ i [ id "navBtn-icon", class "fas fa-bars" ] [] ] ],
                    span [ class "menuItem", id "program" ] [ a [ href "/program" ] [ text "Program" ] ],
                    span [ class "menuItem", id "bedrifter" ] [ a [ href "/bedrifter" ] [ text "Bedrifter" ] ],
                    span [ class "menuItem", id "om" ] [ a [ href "/om" ] [ text "Om oss" ] ]
                ],
                loadNav model,
                getPages model,
                div [ class "footer" ] [
                    a [ href "https://echo.uib.no" ] [ text "echo - Fagutvalget for Informatikk" ]
                ]
            ]
        ]
    }

getPages : Model -> Html Msg
getPages model =
    case model.page of
        Hjem ->
            getHjem model
        Program ->
            getProgram
        Bedrifter ->
            getBedrifter
        Om ->
            getOm

getHjem : Model -> Html msg 
getHjem model =
    div [ class "page" ] [
        div [ class "content" ] [
            h1 [] [ text "echo bedriftstur" ],
            br [] [],
            div [ class "text" ] [ text "echo har startet en komité for å arrangere bedriftstur til Oslo høsten 2020." ],
            div [ class "text" ] [ text "Tanken med arrangementet er å gjøre våre informatikkstudenter kjent med karrieremulighetene i Oslo." ],
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

getProgram : Html msg 
getProgram =
    div [ class "program" ] [ {-
        div [ id "onsdagMain" ] [ text "onsdag" ],
        div [ id "torsdagMain" ] [ text "torsdag" ],
        div [ id "fredagMain" ] [ text "fredag" ],
        div [ id "time10" ] [ text "10" ],
        div [ id "time11" ] [ text "11" ],
        div [ id "time12" ] [ text "12" ],
        div [ id "time13" ] [ text "13" ],
        div [ id "time14" ] [ text "14" ],
        div [ id "time15" ] [ text "15" ],
        div [ id "time16" ] [ text "16" ],
        div [ id "time17" ] [ text "17" ],
        div [ id "time18" ] [ text "18" ],
        div [ id "time19" ] [ text "19" ],
        div [ id "time20" ] [ text "20" ],
        div [ id "time21" ] [ text "21" ],
        div [ id "time22" ] [ text "22" ],
        div [ class "program-item", id "mnemonic-program" ] [ text "mnemonic" ],
        div [ class "program-item", id "computas-program" ] [ text "computas" ],
        div [ class "program-item", id "sopra-program" ] [ text "TBD" ],
        div [ class "program-item", id "knowit-program" ] [ text "knowit" ],
        div [ class "program-item", id "unk-program" ] [ text "TBD" ],
        div [ class "program-item", id "bekk-program" ] [ text "bekk" ]
    -}
        div [ class "text" ] [ text "Kommer snart!" ]
    ]

getBedrifter : Html msg 
getBedrifter =
    div [ class "logos" ] [
        span [ class "logo-item", id "bekk" ] [ a [ target "_blank", rel "noopener noreferrer", href "https://www.bekk.no" ] [ img [ class "bed-logo", src "img/bekk.png", alt "Bekk" ] [] ] ],
        span [ class "logo-item", id "mnemonic" ] [ a [ target "_blank", rel "noopener noreferrer", href "https://www.mnemonic.no" ] [ img [ class "bed-logo", src "img/mnemonic.png", alt "Mnemonic" ] [] ] ],
        span [ class "logo-item", id "TBD" ] [ a [ target "_blank", rel "noopener noreferrer", href "" ] [ i [ class "fas fa-hourglass-start" ] [] ] ],
        span [ class "logo-item", id "TBD2" ] [ a [ target "_blank", rel "noopener noreferrer", href "" ] [ i [ class "fas fa-hourglass-start" ] [] ] ],
        span [ class "logo-item", id "computas" ] [ a [ target "_blank", rel "noopener noreferrer", href "https://computas.com" ] [ img [ class "bed-logo", src "img/computas.png", alt "Computas" ] [] ] ],
        span [ class "logo-item", id "knowit" ] [ a [ target "_blank", rel "noopener noreferrer", href "https://www.knowit.no" ] [ img [ class "bed-logo", src "img/knowit.png", alt "Knowit" ] [] ] ]
    ]

getOm : Html msg 
getOm =
    div [ class "om" ] [
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
            div [ class "tittel" ] [ text "Leder og kontaktansvarlig" ],
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
            div [ class "tittel" ] [ text "Hotellansvarlig" ],
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
