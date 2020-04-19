module Main exposing (main, typeWriterAnim)

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
import Animation
import Animation.Messenger
import Time

import Page exposing (Page(..))
import Page.Hjem as Hjem
import Page.Info as Info
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
    | GotInfoMsg Info.Msg
    | GotLoggInnMsg LoggInn.Msg
    | GotBedrifterMsg Bedrifter.Msg
    | GotProgramMsg Program.Msg
    | GotOmMsg Om.Msg
    | GotVerifiedMsg Verified.Msg
    | ShowNavbar Bool
    | AnimateLogoText Animation.Msg
    | LogoTextCursor Bool
    | RemoveCharLogoText
    | AddCharLogoText String

type Name
    = Initial
    | Mnemonic
    | Computas
    | Cisco
    | Knowit
    | Dnb
    | Bekk

type alias Model =
    { key : Browser.Navigation.Key
    , url : Url.Url
    , route : Page
    , showNavbar : Bool
    , logoTextNameAnim : String
    , logoTextStyle : Animation.Messenger.State Msg
    , modelHjem : Hjem.Model
    , modelInfo : Info.Model
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
    , logoTextNameAnim = ""
    , logoTextStyle = Animation.interrupt
                        [ Animation.loop
                            (typeWriterAnim Initial ++
                            typeWriterAnim Mnemonic ++
                            typeWriterAnim Computas ++
                            typeWriterAnim Cisco ++
                            typeWriterAnim Knowit ++
                            typeWriterAnim Dnb ++
                            typeWriterAnim Bekk)
                        ] (Animation.style [])
    , modelHjem = Hjem.init
    , modelInfo = Info.init
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
        , manageSubscriptions GotInfoMsg Info.subscriptions model.modelInfo
        , manageSubscriptions GotProgramMsg Program.subscriptions model.modelProgram
        , manageSubscriptions GotLoggInnMsg LoggInn.subscriptions model.modelLoggInn
        , manageSubscriptions GotVerifiedMsg Verified.subscriptions model.modelVerified
        , if model.route == Bedrifter then
            manageSubscriptions GotBedrifterMsg Bedrifter.subscriptions model.modelBedrifter
          else
            Sub.none
        , manageSubscriptions GotOmMsg Om.subscriptions model.modelOm
        , Animation.subscription AnimateLogoText [ model.logoTextStyle ]  
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
        GotInfoMsg pageMsg ->
            let (newModel, cmd) = updateWithAndSendMsg Info.update pageMsg model.modelInfo GotInfoMsg
            in ({ model | modelInfo = newModel }, cmd)
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
        AnimateLogoText anim ->
            let (newLogoTextStyle, logoTextCmds) = Animation.Messenger.update anim model.logoTextStyle
            in ({ model | logoTextStyle = newLogoTextStyle }, logoTextCmds)
        LogoTextCursor show ->
            if show then
                ({ model | logoTextNameAnim = (model.logoTextNameAnim ++ "_") }, Cmd.none)
            else
                ({ model | logoTextNameAnim = (String.dropRight 1 model.logoTextNameAnim) }, Cmd.none)
        RemoveCharLogoText ->
            let newText = if String.endsWith "_" model.logoTextNameAnim then
                            (String.dropRight 2 model.logoTextNameAnim) ++ "_"
                          else
                            (String.dropRight 1 model.logoTextNameAnim) ++ "_"
            in ({ model | logoTextNameAnim = newText }, Cmd.none)
        AddCharLogoText char ->
            let newText = if String.endsWith "_" model.logoTextNameAnim then
                            (String.dropRight 1 model.logoTextNameAnim) ++ char ++ "_"
                          else
                            model.logoTextNameAnim ++ char ++ "_"
            in ({ model | logoTextNameAnim = newText }, Cmd.none)

header : Model -> List (Html Msg)
header model =
    let navbtnId = if model.showNavbar then "-anim" else ""
        name = model.logoTextNameAnim
    in
        [ div [ class "site" ] 
            [ div [ class "menu" ]
                [ span [ id "hjem" ] 
                    [ a [ id "logo-stack", href "/", Html.Events.onClick (ShowNavbar True) ] 
                        [ img [ id "logo", alt "logo", src "/img/echo-small.png" ] []
                        ]
                    ]
                , span [ id "logo-text" ]
                  [ span [] [ text "echo " ]
                  , span
                      (Animation.render model.logoTextStyle) [ text name ]
                  ]
                , Svg.svg [ id "navbtn", Html.Events.onClick (ShowNavbar False), Svg.Attributes.width "50", Svg.Attributes.height "40" ]
                    [ Svg.line 
                        [ Svg.Attributes.class "navbtn-line", Svg.Attributes.id ("first-line" ++ navbtnId), x1 "0", x2 "50", y1 "5", y2 "5" ] []
                    , Svg.line
                        [ Svg.Attributes.class "navbtn-line", Svg.Attributes.id ("middle-line" ++ navbtnId), x1 "0", x2 "50", y1 "20", y2 "20" ] []
                    , Svg.line
                        [ Svg.Attributes.class "navbtn-line", Svg.Attributes.id ("second-line" ++ navbtnId), x1 "0", x2 "50", y1 "35", y2 "35" ] []
                    ]
                , if Session.isSignedIn model.modelVerified.session then 
                    span [ class "menu-item", id "user-status" ] [ a [ href "/verified" ] [ text "Min profil" ] ]
                  else
                    span [ class "menu-item", id "user-status" ] [ a [ href "/logg-inn" ] [ text "Logg inn" ] ]
                , span [ class "menu-item", id "info" ] [ a [ href "/info" ] [ text "Informasjon" ] ]
                , span [ class "menu-item", id "program" ] [ a [ href "/program" ] [ text "Program" ] ]
                , span [ class "menu-item", id "bedrifter" ] [ a [ href "/bedrifter" ] [ text "Bedrifter" ] ]
                , span [ class "menu-item", id "om" ] [ a [ href "/om" ] [ text "Om oss" ] ]
                ]
                , div [ class "navbar-blur" ]
                    [ div [ if model.showNavbar then class "navbar" else class "navbar-hidden" ] (getNavbar model) ]
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
        Info ->
            { title = "Info"
            , body = (header model) ++ [ translateHtmlMsg GotInfoMsg Info.view model.modelInfo ]
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

getNavbar : Model -> List (Html Msg)
getNavbar model =
        [ span [] []
        , if Session.isSignedIn model.modelVerified.session then
            a [ class "navbar-item", href "/verified", Html.Events.onClick (ShowNavbar False) ] [ text "Min side" ]
          else
            a [ class "navbar-item", href "/logg-inn", Html.Events.onClick (ShowNavbar False) ] [ text "Påmelding" ]
        , a [ class "navbar-item", href "/info", Html.Events.onClick (ShowNavbar False) ] [ text "Info" ]
        , a [ class "navbar-item", href "/program", Html.Events.onClick (ShowNavbar False) ] [ text "Program" ]
        , a [ class "navbar-item", href "/bedrifter", Html.Events.onClick (ShowNavbar False) ] [ text "Bedrifter" ]
        , a [ class "navbar-item", href "/om", Html.Events.onClick (ShowNavbar False) ] [ text "Om oss" ]
        ]

nameToString : Name -> String
nameToString name =
    let result = List.filter (\(x,y) -> x == name) namesList
    in
        case result of
            [ (_, string) ] ->
                string
            _ ->
                ""

namesList : List (Name, String)
namesList = 
    [ (Initial, "bedriftstur")
    , (Mnemonic, "mnemonic")
    , (Computas, "Computas")
    , (Cisco, "Cisco")
    , (Knowit, "Knowit")
    , (Dnb, "DNB")
    , (Bekk, "Bekk")
    ]

prevName : Name -> Name
prevName name =
    case name of
        Initial -> Bekk
        Mnemonic -> Initial
        Computas -> Mnemonic
        Cisco -> Computas
        Knowit -> Cisco
        Dnb -> Knowit
        Bekk -> Dnb

typeWriterAnim : Name -> List (Animation.Messenger.Step Msg)
typeWriterAnim transitionToName =
        let stylizedName = if transitionToName /= Initial then
                            "+ " ++ (nameToString transitionToName)
                           else
                            nameToString transitionToName
            stylizedPrevName = if (prevName transitionToName) /= Initial then
                                "+ " ++ (nameToString (prevName transitionToName))
                               else
                                nameToString (prevName transitionToName)
        in
            [ Animation.repeat 4
                [ Animation.wait (Time.millisToPosix 500)
                , Animation.Messenger.send (LogoTextCursor True)
                , Animation.wait (Time.millisToPosix 500)
                , Animation.Messenger.send (LogoTextCursor False)
                ] 
            , Animation.repeat 1
                [ Animation.wait (Time.millisToPosix 500)
                , Animation.Messenger.send (LogoTextCursor True)
                , Animation.wait (Time.millisToPosix 500)
                ]
            , Animation.repeat 1
                ((Animation.wait (Time.millisToPosix 120)) ::
                (List.intersperse (Animation.wait (Time.millisToPosix 120))
                    (List.map (Animation.Messenger.send)
                        (List.map (AddCharLogoText)
                            (List.map Tuple.second 
                                ((List.indexedMap 
                                    (\x y -> (x, String.dropLeft x (String.left (x+1) y))) 
                                    (List.repeat (String.length stylizedName) stylizedName)
                                ))
                            )
                        )
                    )
                ))
            , Animation.repeat 1
                [ Animation.Messenger.send (LogoTextCursor False)
                , Animation.wait (Time.millisToPosix 500)
                , Animation.Messenger.send (LogoTextCursor False)
                , Animation.wait (Time.millisToPosix 500)
                ]
            , Animation.repeat 4
                [ Animation.wait (Time.millisToPosix 500)
                , Animation.Messenger.send (LogoTextCursor True)
                , Animation.wait (Time.millisToPosix 500)
                , Animation.Messenger.send (LogoTextCursor False)
                ] 
            , Animation.repeat 1
                [ Animation.wait (Time.millisToPosix 500)
                , Animation.Messenger.send (LogoTextCursor True)
                , Animation.wait (Time.millisToPosix 500)
                ]
            , Animation.repeat ((String.length stylizedName) + 1)
                [ Animation.Messenger.send RemoveCharLogoText
                , Animation.wait (Time.millisToPosix 120)
                ]
            , Animation.repeat 1
                [ Animation.wait (Time.millisToPosix 500)
                , Animation.Messenger.send (LogoTextCursor False)
                ]
            ]

main =
    Browser.application 
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }
