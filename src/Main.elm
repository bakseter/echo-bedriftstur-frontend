module Main exposing (main)

import Browser
import Browser.Navigation
import Html exposing (Html, div, span, h1, h3, text, a, img)
import Html.Attributes exposing (href, class, id, alt, src)
import Url
import Html exposing (Html)
import Html.Events
import Svg
import Svg.Attributes exposing (x1, x2, y1, y2)
import Json.Encode as Encode
import Json.Decode as Decode

import Page exposing (Page(..))
import Page.Hjem as Hjem
import Page.LoggInn as LoggInn
import Page.Bedrifter as Bedrifter
import Page.Program as Program
import Page.Om as Om
import Page.Verified as Verified
import User
import Session

type Msg
    = UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
    | GotHjemMsg Hjem.Msg
    | GotLoggInnMsg LoggInn.Msg
    | GotBedrifterMsg Bedrifter.Msg
    | GotProgramMsg Program.Msg
    | GotOmMsg Om.Msg
    | GotVerifiedMsg Verified.Msg
    | ShowNavbar Bool

type alias Model =
    { key : Browser.Navigation.Key
    , url : Url.Url
    , route : Page
    , showNavbar : Bool
    , modelHjem : Hjem.Model
    , modelLoggInn : LoggInn.Model
    , modelVerified : Verified.Model
    , modelBedrifter : Bedrifter.Model
    , modelProgram : Program.Model
    , modelOm : Om.Model
    }

init : () -> Url.Url -> Browser.Navigation.Key -> (Model, Cmd Msg)
init _ url key =
    ({ key = key
    , url = url
    , route = Page.urlToPage url
    , showNavbar = False
    , modelHjem = Hjem.init
    , modelLoggInn = LoggInn.init
    , modelVerified = Verified.init url key
    , modelBedrifter = Bedrifter.init
    , modelProgram = Program.init
    , modelOm = Om.init
    }, Cmd.none)            

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ manageSubscriptions GotHjemMsg Hjem.subscriptions model.modelHjem
        , manageSubscriptions GotProgramMsg Program.subscriptions model.modelProgram
        , manageSubscriptions GotLoggInnMsg LoggInn.subscriptions model.modelLoggInn
        , manageSubscriptions GotVerifiedMsg Verified.subscriptions model.modelVerified
        , if model.route == Bedrifter then
            manageSubscriptions GotBedrifterMsg Bedrifter.subscriptions model.modelBedrifter
          else
            Sub.none
        , manageSubscriptions GotOmMsg Om.subscriptions model.modelOm
        ]

manageSubscriptions : (a -> msg) -> (b -> Sub a) -> b -> Sub msg
manageSubscriptions msg subFunc model =
    Sub.map msg (subFunc model)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    let modelLoggInn = model.modelLoggInn
                    in ({ model | modelLoggInn = LoggInn.init }
                        , Browser.Navigation.pushUrl model.key (Url.toString url))
                Browser.External href ->
                    (model, Browser.Navigation.load href) 
        UrlChanged url ->
            let route = Page.urlToPage url
            in ({ model | url = url, route = route }, Cmd.none)
        GotHjemMsg pageMsg ->
            let (newModel, cmd) = updateWithAndSendMsg Hjem.update pageMsg model.modelHjem GotHjemMsg
            in ({ model | modelHjem = newModel }, cmd)
        GotLoggInnMsg pageMsg ->
            let (newModel, cmd) = updateWithAndSendMsg LoggInn.update pageMsg model.modelLoggInn GotLoggInnMsg
            in ({ model | modelLoggInn = newModel }, cmd)
        GotVerifiedMsg pageMsg ->
            let (newModel, cmd) = updateWithAndSendMsg Verified.update pageMsg model.modelVerified GotVerifiedMsg
            in ({ model | modelVerified = newModel }, cmd)
        GotProgramMsg pageMsg ->
            let (newModel, cmd) = updateWithAndSendMsg Program.update pageMsg model.modelProgram GotProgramMsg
            in ({ model | modelProgram = newModel }, cmd)
        GotBedrifterMsg pageMsg ->
            let (newModel, cmd) = updateWithAndSendMsg Bedrifter.update pageMsg model.modelBedrifter GotBedrifterMsg
            in ({ model | modelBedrifter = newModel }, cmd)
        GotOmMsg pageMsg ->
            let (newModel, cmd) = updateWithAndSendMsg Om.update pageMsg model.modelOm GotOmMsg
            in ({ model | modelOm = newModel }, cmd)
        ShowNavbar linksToHome ->
            if model.showNavbar == False then
                if linksToHome then
                    (model, Cmd.none)
                else
                    ({ model | showNavbar = True }, Cmd.none)
            else
                ({ model | showNavbar = False }, Cmd.none)

header : Model -> List (Html Msg)
header model =
    [ div [ class "site" ] 
        [ div [ class "menu" ]
            [ span [ id "hjem" ] 
                [ a [ id "logo-link", href "/", Html.Events.onClick (ShowNavbar True) ] 
                    [ img [ id "logo", alt "logo", src "/img/echo-logo-very-wide.png" ] [] ] 
                ]
            , Svg.svg [ id "navbtn", Html.Events.onClick (ShowNavbar False), Svg.Attributes.width "50", Svg.Attributes.height "40" ]
                [ Svg.line 
                    [ Svg.Attributes.class "navbtn-line"
                    , if model.showNavbar then
                        Svg.Attributes.id "first-line-anim"
                      else
                        Svg.Attributes.id "first-line"
                    , x1 "0"
                    , x2 "50"
                    , y1 "5"
                    , y2 "5"
                    ] []
                , Svg.line
                    [ Svg.Attributes.class "navbtn-line"
                    , if model.showNavbar then
                        Svg.Attributes.id "middle-line-anim"
                      else
                        Svg.Attributes.id "middle-line"
                    , x1 "0"
                    , x2 "50"
                    , y1 "20"
                    , y2 "20"
                    ] []
                , Svg.line
                    [ Svg.Attributes.class "navbtn-line"
                    , if model.showNavbar then
                        Svg.Attributes.id "second-line-anim"
                      else
                        Svg.Attributes.id "second-line"
                    , x1 "0"
                    , x2 "50"
                    , y1 "35"
                    , y2 "35"
                    ] []
                ]
            , if Session.isSignedIn model.modelVerified.session then 
                span [ class "menu-item", id "user-status" ] [ a [ href "/verified" ] [ text "Min profil" ] ]
              else
                span [ class "menu-item", id "user-status" ] [ a [ href "/logg-inn" ] [ text "Logg inn" ] ]
            , span [ class "menu-item", id "program" ] [ a [ href "/program" ] [ text "Program" ] ]
            , span [ class "menu-item", id "bedrifter" ] [ a [ href "/bedrifter" ] [ text "Bedrifter" ] ]
            , span [ class "menu-item", id "om" ] [ a [ href "/om" ] [ text "Om oss" ] ]
            ]
            , span [ id "navbar" ] [ getNavbar model ]
        ]
    ]

view : Model -> Browser.Document Msg
view model =
    showPage model

showPage : Model -> Browser.Document Msg
showPage model =
    case model.route of
        Hjem ->
            { title = "echo bedriftstur"
            , body = (header model) ++ [ translateHtmlMsg GotHjemMsg Hjem.view model.modelHjem ]
            }
        LoggInn ->
            { title = "Logg inn"
            , body = (header model) ++ [ translateHtmlMsg GotLoggInnMsg LoggInn.view model.modelLoggInn ]
            }
        Verified ->
            { title = "Min side"
            , body = (header model) ++ [ translateHtmlMsg GotVerifiedMsg Verified.view model.modelVerified ]
            }
        Program ->
            { title = "Program"
            , body = (header model) ++ [ translateHtmlMsg GotProgramMsg Program.view model.modelProgram ]
            }
        Bedrifter ->
            { title = "Bedrifter"
            , body = (header model) ++ [ translateHtmlMsg GotBedrifterMsg Bedrifter.view model.modelBedrifter ]
            }
        Om ->
            { title = "Om oss"
            , body = (header model) ++ [ translateHtmlMsg GotOmMsg Om.view model.modelOm ]
            }
        NotFound ->
            { title = "Fant ikke siden"
            , body = (header model) ++
                [ div [ class "not-found" ]
                    [ h1 [ id "not-found-header" ] [ text "404" ]
                    , h3 [ id "not-found-text" ] [ text "Siden du leter etter eksisterer ikke" ]
                    ]
                ]
            }

translateHtmlMsg : (a -> msg) -> (b -> Html.Html a) -> b -> Html.Html msg
translateHtmlMsg msg viewFunc model =
    Html.map msg (viewFunc model)

updateWithAndSendMsg : (b -> c -> (d, Cmd a)) -> b -> c -> (a -> msg) -> (d, Cmd msg)
updateWithAndSendMsg updateFunc msg model msg2 =
    let (newModel, cmd) = updateFunc msg model
    in (newModel, Cmd.map msg2 cmd)

getNavbar : Model -> Html Msg
getNavbar model =
    if model.showNavbar then
        div [ id "navbar-content" ] 
            [ if Session.isSignedIn model.modelVerified.session then
                a [ href "/verified", Html.Events.onClick (ShowNavbar False) ] [ text "Min side" ]
              else
                a [ href "/logg-inn", Html.Events.onClick (ShowNavbar False) ] [ text "Påmelding" ]
            , a [ href "/program", Html.Events.onClick (ShowNavbar False) ] [ text "Program" ]
            , a [ href "/bedrifter", Html.Events.onClick (ShowNavbar False) ] [ text "Bedrifter" ]
            , a [ href "/om", Html.Events.onClick (ShowNavbar False) ] [ text "Om oss" ]
            ]
    else
        span [] []

main =
    Browser.application 
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }
