port module Main exposing (main)

import Browser
import Browser.Navigation
import Html exposing (Html, div, span, h1, h3, text, a, img, i)
import Html.Attributes exposing (href, class, id, alt, src)
import Url
import Page.Hjem as Hjem
import Page.LoggInn as LoggInn
import Page.Bedrifter as Bedrifter
import Page.Program as Program
import Page.Om as Om
import Page.Verified as Verified
import Html exposing (Html)
import Html.Events
import Animation exposing (deg, px)
import Animation.Messenger
import Svg
import Svg.Attributes exposing (x1, x2, y1, y2)
import Json.Encode
import Json.Decode

main =
    Browser.application 
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }

type Page 
    = Hjem
    | LoggInn
    | Bedrifter
    | Program
    | Om
    | Verified
    | NotFound

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
    | NavBtnTransition
    | AnimateNavBtn Animation.Msg

type alias Model =
    { key : Browser.Navigation.Key
    , url : Url.Url
    , currentPage : Page
    , showNavbar : Bool
    , hideLineNavBtn : Bool  
    , navBtnAnimation : (Animation.Messenger.State Msg, Animation.Messenger.State Msg)
    , modelHjem : Hjem.Model
    , modelLoggInn : LoggInn.Model
    , modelBedrifter : Bedrifter.Model
    , modelProgram : Program.Model
    , modelOm : Om.Model
    , modelVerified : Verified.Model
    }

port attemptSignOut : Json.Encode.Value -> Cmd msg
port signOutError : (Json.Encode.Value -> msg) -> Sub msg
port signOutSucceeded : (Json.Encode.Value -> msg) -> Sub msg

init : Maybe String -> Url.Url -> Browser.Navigation.Key -> (Model, Cmd Msg)
init path url key =
    let model = { key = key
                , url = url
                , currentPage = Hjem
                , showNavbar = False
                , hideLineNavBtn = False
                , navBtnAnimation = startNavBtnStyle
                , modelHjem = Hjem.init
                , modelLoggInn = LoggInn.init
                , modelBedrifter = Bedrifter.init
                , modelProgram = Program.init
                , modelOm = Om.init
                , modelVerified = Verified.init url key
                }
    in
        case path of
            Just str ->
                case str of
                    "/" ->
                        ({ model | currentPage = Hjem }, Cmd.none)
                    "/logg-inn" ->
                        ({ model | currentPage = LoggInn }, Cmd.none)
                    "/bedrifter" ->
                        ({ model | currentPage = Bedrifter }, Cmd.none)
                    "/program" ->
                       ({ model | currentPage = Program }, Cmd.none)
                    "/om" ->
                        ({ model | currentPage = Om }, Cmd.none)
                    "/verified" ->
                        ({ model | currentPage = Verified }, Cmd.none)
                    _ ->
                        ({ model | currentPage = NotFound }, Cmd.none)
            Nothing ->
                ({ model | currentPage = Hjem }, Cmd.none)
                

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ manageSubscriptions GotHjemMsg Hjem.subscriptions model.modelHjem
        , if model.currentPage == Bedrifter then
            manageSubscriptions GotBedrifterMsg Bedrifter.subscriptions model.modelBedrifter
          else
            Sub.none
        , manageSubscriptions GotProgramMsg Program.subscriptions model.modelProgram
        , manageSubscriptions GotOmMsg Om.subscriptions model.modelOm
        , manageSubscriptions GotVerifiedMsg Verified.subscriptions model.modelVerified
        , manageSubscriptions GotLoggInnMsg LoggInn.subscriptions model.modelLoggInn
        , signOutSucceeded SignOutSucceeded
        , signOutError SignOutError
        , Animation.subscription AnimateNavBtn
            [ Tuple.first model.navBtnAnimation
            , Tuple.second model.navBtnAnimation
            ]
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
                    in ({ model | modelLoggInn = { modelLoggInn | currentSubPage = LoggInn.countdown model.modelLoggInn } }, Browser.Navigation.pushUrl model.key (Url.toString url))
                Browser.External href ->
                    (model, Browser.Navigation.load href) 
        UrlChanged url ->
            case url.path of
                "/" ->
                    ({ model | url = url, currentPage = Hjem }, Cmd.none)
                "/logg-inn" ->
                    ({ model | url = url, currentPage = LoggInn }, Cmd.none)
                "/bedrifter" ->
                    ({ model | url = url, currentPage = Bedrifter }, Cmd.none) 
                "/program" ->
                    ({ model | url = url, currentPage = Program }, Cmd.none)
                "/om" ->
                    ({ model | url = url, currentPage = Om }, Cmd.none)
                "/verified" ->
                    ({ model | url = url, currentPage = Verified }, Cmd.none)
                "/minside" ->
                    (model, Cmd.none)
                _ ->
                    ({ model | url = url, currentPage = NotFound}, Cmd.none)
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
                    ({ model | showNavbar = True, navBtnAnimation = newNavBtnStyle False model.navBtnAnimation }, Cmd.none)
            else
                ({ model | showNavbar = False, navBtnAnimation = newNavBtnStyle True model.navBtnAnimation }, Cmd.none)
        NavBtnTransition ->
            if model.hideLineNavBtn then
                ({ model | hideLineNavBtn = False }, Cmd.none)
            else
                ({ model | hideLineNavBtn = True }, Cmd.none)
        AnimateNavBtn anim ->
            let (styleFirstLineNavBtn, navBtnCmd) = Animation.Messenger.update anim (Tuple.first model.navBtnAnimation)
                (styleSecondLineNavBtn, _) = Animation.Messenger.update anim (Tuple.second model.navBtnAnimation)
            in ({ model | navBtnAnimation = (styleFirstLineNavBtn, styleSecondLineNavBtn) }, navBtnCmd)
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
                    , currentPage = Hjem }
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
                , span [ id "navBtn", Html.Events.onClick (ShowNavbar False) ]
                    [ Svg.svg [ Svg.Attributes.width "100", Svg.Attributes.height "100" ]
                        [ Svg.line (Animation.render (Tuple.first model.navBtnAnimation)
                            ++ [ x1 "0", x2 "50", y1 "35", y2 "35", Svg.Attributes.style "stroke:rgb(125,125,125);stroke-width:4;" ]) []
                        , (getMiddleLine model.hideLineNavBtn)
                        , Svg.line (Animation.render (Tuple.second model.navBtnAnimation) 
                            ++ [ x1 "0", x2 "50", y1 "65", y2 "65", Svg.Attributes.style "stroke:rgb(125,125,125);stroke-width:4;" ]) []
                        ] 
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
        case model.currentPage of
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
            NotFound ->
                [ div [ class "not-found" ]
                    [ h1 [] [ text "404" ]
                    , h3 [] [ text "Siden du leter etter eksisterer ikke" ]
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

getMiddleLine : Bool -> Svg.Svg msg
getMiddleLine hide =
    if hide then 
        Svg.line [] []
    else
        Svg.line [ x1 "0", x2 "50", y1 "50", y2 "50", Svg.Attributes.style "stroke:rgb(125,125,125);stroke-width:4;" ] []

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

startNavBtnStyle : (Animation.Messenger.State Msg, Animation.Messenger.State Msg)  
startNavBtnStyle =
    (Animation.style 
        [ Animation.rotate (deg 0)
        , Animation.translate (px 0) (px 0)
        , Animation.scale 1.0 
        ]
    , Animation.style 
         [ Animation.rotate (deg 0)
        , Animation.translate (px 0) (px 0)
       , Animation.scale 1.0 
    ])

newNavBtnStyle : Bool -> 
                (Animation.Messenger.State Msg, Animation.Messenger.State Msg) -> 
                (Animation.Messenger.State Msg, Animation.Messenger.State Msg)
newNavBtnStyle menuActive style =
    if menuActive then
        (Animation.interrupt 
            [ Animation.Messenger.send NavBtnTransition
            , Animation.to 
                [ Animation.translate (px 0) (px 0)
                ,  Animation.rotate (deg 0)
                ,  Animation.scale 1.0 
                ]
            ] 
            (Tuple.first style)
            , Animation.interrupt 
            [ Animation.to 
                [ Animation.translate (px 0) (px 0)
                , Animation.rotate (deg 0)
                , Animation.scale 1.0
                ]
            ] 
            (Tuple.second style)
        )
    else
        (Animation.interrupt 
             [ Animation.Messenger.send NavBtnTransition
             , Animation.to 
                [ Animation.translate (px 34) (px -8)
                ,  Animation.rotate (deg 45)
                ,  Animation.scale 0.7 
                ]
            ] 
            (Tuple.first style)
            , Animation.interrupt 
            [ Animation.to 
                [ Animation.translate (px -35) (px 6)
                , Animation.rotate (deg -45)
                , Animation.scale 0.7
                ]
            ] 
            (Tuple.second style)
        )

