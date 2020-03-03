port module Main exposing (main)

import Browser
import Browser.Navigation
import Html exposing (Html, div, span, h1, h3, text, a, img)
import Html.Attributes exposing (href, class, id, alt, src)
import Url
import Page exposing (..)
import Page.Hjem as Hjem
import Page.LoggInn as LoggInn
import Page.Bedrifter as Bedrifter
import Page.Program as Program
import Page.Om as Om
import Page.Verified as Verified
import Html exposing (Html)
import Html.Events
import Svg
import Svg.Attributes exposing (x1, x2, y1, y2)
import Json.Encode

main =
    Browser.application 
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }

type Msg
    = UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
    | GotHjemMsg Hjem.Msg
    | GotLoggInnMsg LoggInn.Msg
    | GotBedrifterMsg Bedrifter.Msg
    | GotProgramMsg Program.Msg
    | GotOmMsg Om.Msg
    | GotVerifiedMsg Verified.Msg
    | AttemptSignOut
    | SignOutSucceeded Json.Encode.Value
    | SignOutError Json.Encode.Value
    | ShowNavbar Bool

type alias Model =
    { key : Browser.Navigation.Key
    , url : Url.Url
    , route : Route
    , showNavbar : Bool
    , modelHjem : Hjem.Model
    , modelLoggInn : LoggInn.Model
    , modelVerified : Verified.Model
    , modelBedrifter : Bedrifter.Model
    , modelProgram : Program.Model
    , modelOm : Om.Model
    }

port attemptSignOut : Json.Encode.Value -> Cmd msg
port signOutError : (Json.Encode.Value -> msg) -> Sub msg
port signOutSucceeded : (Json.Encode.Value -> msg) -> Sub msg

init : () -> Url.Url -> Browser.Navigation.Key -> (Model, Cmd Msg)
init _ url key =
    ({ key = key
    , url = url
    , route = Page.urlToRoute url
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
        , if model.route == Bedrifter then
            manageSubscriptions GotBedrifterMsg Bedrifter.subscriptions model.modelBedrifter
          else
            Sub.none
        , manageSubscriptions GotProgramMsg Program.subscriptions model.modelProgram
        , manageSubscriptions GotOmMsg Om.subscriptions model.modelOm
        , manageSubscriptions GotLoggInnMsg LoggInn.subscriptions model.modelLoggInn
        , manageSubscriptions GotVerifiedMsg Verified.subscriptions model.modelVerified
        , signOutSucceeded SignOutSucceeded
        , signOutError SignOutError
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
                    in ({ model | modelLoggInn = { modelLoggInn | currentSubPage = LoggInn.countdown } }, Browser.Navigation.pushUrl model.key (Url.toString url))
                Browser.External href ->
                    (model, Browser.Navigation.load href) 
        UrlChanged url ->
            let route = Page.urlToRoute url
            in ({ model | url = url, route = route }, Cmd.none)
        GotHjemMsg pageMsg ->
            let (newModel, cmd) = updateWithAndSendMsg Hjem.update pageMsg model.modelHjem GotHjemMsg
            in ({ model | modelHjem = newModel }, cmd)
        GotBedrifterMsg pageMsg ->
            let (newModel, cmd) = updateWithAndSendMsg Bedrifter.update pageMsg model.modelBedrifter GotBedrifterMsg
            in ({ model | modelBedrifter = newModel }, cmd)
        GotProgramMsg pageMsg ->
            let (newModel, cmd) = updateWithAndSendMsg Program.update pageMsg model.modelProgram GotProgramMsg
            in ({ model | modelProgram = newModel }, cmd)
        GotOmMsg pageMsg ->
            let (newModel, cmd) = updateWithAndSendMsg Om.update pageMsg model.modelOm GotOmMsg
            in ({ model | modelOm = newModel }, cmd)
        GotLoggInnMsg pageMsg ->
            let (newModel, cmd) = updateWithAndSendMsg LoggInn.update pageMsg model.modelLoggInn GotLoggInnMsg
            in ({ model | modelLoggInn = newModel }, cmd)
        GotVerifiedMsg pageMsg ->
            let (newModel, cmd) = updateWithAndSendMsg Verified.update pageMsg model.modelVerified GotVerifiedMsg
            in ({ model | modelVerified = newModel }, cmd)
        ShowNavbar linksToHome ->
            if model.showNavbar == False then
                if linksToHome then
                    (model, Cmd.none)
                else
                    ({ model | showNavbar = True }, Cmd.none)
            else
                ({ model | showNavbar = False }, Cmd.none)
        AttemptSignOut ->
            (model, attemptSignOut (Verified.encode "requestedLogOut" True))
        SignOutSucceeded _ ->
            let modelVerified = model.modelVerified
                user = model.modelVerified.user
            in ({ model | 
                    modelVerified = { modelVerified |
                                      currentSubPage = Verified.verified
                                    , user = { user | isSignedIn = False }
                                    }
                    , route = (Page.Hjem) }
               , Browser.Navigation.pushUrl model.key Verified.redirectToHome )
        SignOutError json ->
            (model, Cmd.none)

view : Model -> Browser.Document Msg
view model =
    { title = "echo bedriftstur"
    , body = 
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
                , if model.modelVerified.user.isSignedIn then 
                    span [ class "menu-item", id "user-status" ] [ a [ Html.Events.onClick AttemptSignOut ] [ text "Logg ut" ] ]
                  else
                    span [ class "menu-item", id "user-status" ] [ a [ href "/logg-inn" ] [ text "Logg inn" ] ]
                , span [ class "menu-item", id "program" ] [ a [ href "/program" ] [ text "Program" ] ]
                , span [ class "menu-item", id "bedrifter" ] [ a [ href "/bedrifter" ] [ text "Bedrifter" ] ]
                , span [ class "menu-item", id "om" ] [ a [ href "/om" ] [ text "Om oss" ] ]
                ]
                , span [ id "navbar" ] [ getNavbar model ]
            ]
        ] ++
        case model.route of
            Hjem ->
                [ showPage GotHjemMsg Hjem.view model.modelHjem ]
            LoggInn ->
                [ showPage GotLoggInnMsg LoggInn.view model.modelLoggInn ]
            Bedrifter ->
                [ showPage GotBedrifterMsg Bedrifter.view model.modelBedrifter ]
            Program ->
                [ showPage GotProgramMsg Program.view model.modelProgram ]
            Om ->
                [ showPage GotOmMsg Om.view model.modelOm ]
            Verified ->
                [ showPage GotVerifiedMsg Verified.view model.modelVerified ]
            NotFound  ->
                [ div [ class "not-found" ]
                    [ h1 [ id "not-found-header" ] [ text "404" ]
                    , h3 [ id "not-found-text" ] [ text "Siden du leter etter eksisterer ikke" ]
                    ]
                ]
    }

showPage : (a -> msg) -> (b -> Html.Html a) -> b -> Html.Html msg
showPage msg viewFunc model =
    Html.map msg (viewFunc model)

updateWithAndSendMsg : (b -> c -> (d, Cmd a)) -> b -> c -> (a -> msg) -> (d, Cmd msg)
updateWithAndSendMsg updateFunc msg model msg2 =
    let (newModel, cmd) = updateFunc msg model
    in (newModel, Cmd.map msg2 cmd)

getNavbar : Model -> Html Msg
getNavbar model =
    if model.showNavbar then
        div [ id "navbar-content" ] 
            [ if model.modelVerified.user.isSignedIn then
                a [ Html.Events.onClick AttemptSignOut ] [ text "Logg ut" ]
              else
                a [ href "/logg-inn", Html.Events.onClick (ShowNavbar False) ] [ text "Logg inn" ]
            , a [ href "/program", Html.Events.onClick (ShowNavbar False) ] [ text "Program" ]
            , a [ href "/bedrifter", Html.Events.onClick (ShowNavbar False) ] [ text "Bedrifter" ]
            , a [ href "/om", Html.Events.onClick (ShowNavbar False) ] [ text "Om oss" ]
            ]
    else
        span [] []
