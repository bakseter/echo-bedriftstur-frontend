module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, div, span, h1, h2, h3, text, br, a, img)
import Html.Attributes exposing (href, class, id, alt, src)
import Url
import Page.Hjem as Hjem
import Page.Bedrifter as Bedrifter
import Page.Program as Program
import Page.Om as Om
import Html exposing (Html)
import Html.Events
import Animation exposing (deg, px)
import Animation.Messenger
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

type Msg
    = UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
    | GotHjemMsg Hjem.Msg
    | GotBedrifterMsg Bedrifter.Msg
    | GotProgramMsg Program.Msg
    | GotOmMsg Om.Msg
    | ShowNavbar Bool
    | NavBtnTransition
    | AnimateNavBtn Animation.Msg

type Page 
    = Hjem
    | Bedrifter
    | Program
    | Om
    | NotFound

type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , currentPage : Page
    , showNavbar : Bool
    , hideLineNavBtn : Bool  
    , navBtnAnimation : (Animation.Messenger.State Msg, Animation.Messenger.State Msg)
    , modelCmdHjem : (Hjem.Model, Cmd Hjem.Msg)
    , modelCmdBedrifter :(Bedrifter.Model, Cmd Bedrifter.Msg)
    , modelCmdProgram : (Program.Model, Cmd Program.Msg)
    , modelCmdOm : (Om.Model, Cmd Om.Msg)
    }

init : String -> Url.Url -> Nav.Key -> (Model, Cmd Msg)
init _ url key =
    ({ key = key
     , url = url
     , currentPage = Hjem
     , showNavbar = False
     , hideLineNavBtn = False
     , navBtnAnimation = (Animation.style 
                               [ Animation.rotate (deg 0)
                               , Animation.translate (px 0) (px 0)
                               , Animation.scale 1.0 
                               ]
                            , Animation.style 
                               [ Animation.rotate (deg 0)
                               , Animation.translate (px 0) (px 0)
                               , Animation.scale 1.0 
                               ])
     , modelCmdHjem = Hjem.init
     , modelCmdBedrifter = Bedrifter.init
     , modelCmdProgram = Program.init
     , modelCmdOm = Om.init
     }, Cmd.none)
                

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map GotHjemMsg (Hjem.subscriptions (Tuple.first model.modelCmdHjem))
        , Animation.subscription AnimateNavBtn [ Tuple.first model.navBtnAnimation, Tuple.second model.navBtnAnimation ]
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
                "/" ->
                    ({ model | url = url, currentPage = Hjem }, Cmd.none)
                "/program" ->
                    ({ model | url = url, currentPage = Program }, Cmd.none)
                "/bedrifter" ->
                    ({ model | url = url, currentPage = Bedrifter }, Cmd.none) 
                "/om" ->
                    ({ model | url = url, currentPage = Om }, Cmd.none)
                _ ->
                    ({ model | url = url, currentPage = NotFound}, Cmd.none)
        GotHjemMsg msg_ ->
            let (newModelCmd, cmd) = updateWithAndSendMsg Hjem.update msg_ model.modelCmdHjem GotHjemMsg
            in ({ model | modelCmdHjem = newModelCmd }, cmd)
        GotBedrifterMsg msg_ ->
            let (newModelCmd, cmd) = updateWithAndSendMsg Bedrifter.update msg_ model.modelCmdBedrifter GotBedrifterMsg
            in ({ model | modelCmdBedrifter = newModelCmd }, cmd)
        GotProgramMsg msg_ ->
            let (newModelCmd, cmd) = updateWithAndSendMsg Program.update msg_ model.modelCmdProgram GotProgramMsg
            in ({ model | modelCmdProgram = newModelCmd }, cmd)
        GotOmMsg msg_ ->
            let (newModelCmd, cmd) = updateWithAndSendMsg Om.update msg_ model.modelCmdOm GotOmMsg
            in ({ model | modelCmdOm = newModelCmd }, cmd)
        ShowNavbar linksToHome ->
            if model.showNavbar == False then
                case linksToHome of
                    True ->
                        (model, Cmd.none)
                    False ->
                        ({ model
                            | showNavbar = True
                            , navBtnAnimation = (Animation.interrupt 
                                                     [ Animation.Messenger.send NavBtnTransition
                                                     , Animation.to 
                                                        [ Animation.translate (px 34) (px -8)
                                                        ,  Animation.rotate (deg 45)
                                                        ,  Animation.scale 0.7 
                                                        ]
                                                    ] 
                                                    (Tuple.first model.navBtnAnimation)
                                                    , Animation.interrupt 
                                                    [ Animation.to 
                                                        [ Animation.translate (px -35) (px 6)
                                                        , Animation.rotate (deg -45)
                                                        , Animation.scale 0.7
                                                        ]
                                                    ] 
                                                    (Tuple.second model.navBtnAnimation)
                                                )
                        }
                        , Cmd.none)
            else
                ({ model
                    | showNavbar = False
                    , navBtnAnimation = (Animation.interrupt 
                                            [ Animation.Messenger.send NavBtnTransition
                                            , Animation.to 
                                                [ Animation.translate (px 0) (px 0)
                                                ,  Animation.rotate (deg 0)
                                                ,  Animation.scale 1.0 
                                                ]
                                            ] 
                                            (Tuple.first model.navBtnAnimation)
                                            , Animation.interrupt 
                                            [ Animation.to 
                                                [ Animation.translate (px 0) (px 0)
                                                , Animation.rotate (deg 0)
                                                , Animation.scale 1.0
                                                ]
                                            ] 
                                            (Tuple.second model.navBtnAnimation)
                                        )
                    } 
                    , Cmd.none)
        NavBtnTransition ->
            if model.hideLineNavBtn
            then
                ({ model | hideLineNavBtn = False }, Cmd.none)
            else
                ({ model | hideLineNavBtn = True }, Cmd.none)
        AnimateNavBtn anim ->
            let (styleFirstLineNavBtn, navBtnCmd) = Animation.Messenger.update anim (Tuple.first model.navBtnAnimation)
                (styleSecondLineNavBtn, _) = Animation.Messenger.update anim (Tuple.second model.navBtnAnimation)
            in ({ model | navBtnAnimation = (styleFirstLineNavBtn, styleSecondLineNavBtn) }, navBtnCmd)

view : Model -> Browser.Document Msg
view model =
    { title = "echo bedriftstur"
    , body = 
        [ div [ class "site" ] 
            [ div [ class "menu" ]
                [ span [ id "hjem" ] 
                    [ a [ href "/", Html.Events.onClick (ShowNavbar True) ] 
                        [ img [ id "logo", alt "logo", src "/img/echo-logo-very-wide.png" ] [] ] 
                    ]
                , span [ id "navBtn", Html.Events.onClick (ShowNavbar False) ]
                [ svg [ width "100", height "100" ]
                    [ line (Animation.render (Tuple.first model.navBtnAnimation)
                    ++ [ x1 "0", x2 "50", y1 "35", y2 "35", Svg.Attributes.style "stroke:rgb(125,125,125);stroke-width:4;" ]) []
                    , (getMiddleLine model.hideLineNavBtn)
                    , line (Animation.render (Tuple.second model.navBtnAnimation) 
                        ++ [ x1 "0", x2 "50", y1 "65", y2 "65", Svg.Attributes.style "stroke:rgb(125,125,125);stroke-width:4;" ]) []
                    ] 
                ]
                , span [ class "menuItem", id "program" ] [ a [ href "/program" ] [ text "Program" ] ]
                , span [ class "menuItem", id "bedrifter" ] [ a [ href "/bedrifter" ] [ text "Bedrifter" ] ]
                , span [ class "menuItem", id "om" ] [ a [ href "/om" ] [ text "Om oss" ] ]
                ]
            , span [ id "navbar" ] [ getNavbar model.showNavbar ]
            ]
         ] ++
        case model.currentPage of
            Hjem ->
                [ showPage GotHjemMsg Hjem.view model.modelCmdHjem ]
            Bedrifter ->
                [ showPage GotBedrifterMsg Bedrifter.view model.modelCmdBedrifter ]
            Program ->
                [ showPage GotProgramMsg Program.view model.modelCmdProgram ]
            Om ->
                [ showPage GotOmMsg Om.view model.modelCmdOm ]
            NotFound ->
                [ div [] [ text "404" ] ]
    }

showPage : (a1 -> msg) -> (a -> Html.Html a1) -> ( a, b ) -> Html.Html msg
showPage msg viewFunc model =
    Html.map msg (viewFunc (Tuple.first model))

getMiddleLine : Bool -> Svg.Svg msg
getMiddleLine hide =
    if hide 
    then 
        line [] []
    else
        line [ x1 "0", x2 "50", y1 "50", y2 "50", Svg.Attributes.style "stroke:rgb(125,125,125);stroke-width:4;" ] []


getNavbar : Bool -> Html Msg
getNavbar show =
    case show of
        True ->
            div [ id "navbar-content" ] 
                [ a [ href "/bedrifter", Html.Events.onClick (ShowNavbar False) ] [ text "Bedrifter" ]
                , a [ href "/program", Html.Events.onClick (ShowNavbar False) ] [ text "Program" ]
                , a [ href "/om", Html.Events.onClick (ShowNavbar False) ] [ text "Om oss" ]
                ]
        False ->
            span [] []

updateWithAndSendMsg : (c -> a2 -> (a1, Cmd a)) -> c -> (a2, b) -> (a -> msg) -> ((a1, Cmd a), Cmd msg)
updateWithAndSendMsg updateFunc msg modelCmd msg2 =
    let newModelCmd = updateFunc msg (Tuple.first modelCmd)
    in (newModelCmd, Cmd.map msg2 (Tuple.second newModelCmd))
