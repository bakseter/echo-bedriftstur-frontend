module Main exposing (main)

import Browser
import Browser.Events
import Browser.Navigation
import Element exposing (..)
import Element.Background as Background
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import FontAwesome.Brands
import FontAwesome.Icon as Icon
import FontAwesome.Regular
import FontAwesome.Solid
import Html
import Html.Attributes as HtmlA
import Json.Encode as Encode
import Page exposing (Page(..))
import Page.Bedrifter as Bedrifter
import Page.Hjem as Hjem
import Page.NotFound as NotFound
import Page.Om as Om
import Page.Profil as Profil
import Page.Program as Program
import Session exposing (Session)
import Svg
import Svg.Attributes
import Theme
import Url
import Util exposing (edges)


type Msg
    = UrlChanged Url.Url
    | UrlRequested Browser.UrlRequest
    | GotProfilMsg Profil.Msg
    | WindowResize Int Int
    | ToggleDrawer


type alias Model =
    { page : Page
    , session : Session
    , profilModel : Profil.Model
    , showDrawer : Bool
    }


init : Encode.Value -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init json url navKey =
    let
        page =
            Page.fromUrl url

        ( apiKey, device ) =
            Session.decodeSession json

        session =
            Session navKey apiKey device

        ( profilModel, profilCmd ) =
            Profil.init session
    in
    ( { page = page
      , session = session
      , profilModel = profilModel
      , showDrawer = False
      }
    , Cmd.map GotProfilMsg profilCmd
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize WindowResize


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlRequested request ->
            case request of
                Browser.Internal url ->
                    ( model, Browser.Navigation.pushUrl model.session.navKey (Url.toString url) )

                Browser.External href ->
                    ( model, Browser.Navigation.load href )

        UrlChanged url ->
            ( { model | page = Page.fromUrl url }, Cmd.none )

        GotProfilMsg profilMsg ->
            let
                ( newModel, cmd ) =
                    Profil.update profilMsg model.profilModel
            in
            ( { model | profilModel = newModel }, Cmd.map GotProfilMsg cmd )

        WindowResize w h ->
            let
                newSession =
                    { navKey = model.session.navKey
                    , apiKey = model.session.apiKey
                    , device = classifyDevice { width = w, height = h }
                    }
            in
            ( { model | session = newSession }, Cmd.none )

        ToggleDrawer ->
            ( { model | showDrawer = not model.showDrawer }, Cmd.none )


drawer : Model -> Element Msg
drawer model =
    let
        attr =
            htmlAttribute <|
                if model.showDrawer then
                    HtmlA.style "left" "65%"

                else
                    HtmlA.style "left" "100%"
    in
    column
        [ htmlAttribute <| HtmlA.style "transition" "0.5s ease"
        , Background.color Theme.drawer
        , attr
        , width fill
        , height fill
        , spacing 50
        , paddingEach { edges | top = 150, left = 25 }
        , alignBottom
        ]
        [ link [ Events.onClick ToggleDrawer ]
            { url = "/" ++ Program.route
            , label = text Program.title
            }
        , link [ Events.onClick ToggleDrawer ]
            { url = "/" ++ Bedrifter.route
            , label = text Bedrifter.title
            }
        , link [ Events.onClick ToggleDrawer ]
            { url = "/" ++ Om.route
            , label = text Om.title
            }
        , link [ Events.onClick ToggleDrawer ]
            { url = "/" ++ Profil.route
            , label = el [ Font.bold ] <| text Profil.title
            }
        ]


header : Model -> Element Msg
header model =
    let
        ( line1Attr, line2Attr, line3Attr ) =
            if model.showDrawer then
                ( Svg.Attributes.style "transition: 0.5s ease; transform: rotate(45deg) translate(14px, -7px) scale(0.7);"
                , Svg.Attributes.style "transition: 0.5s ease; transform: translate(50px);"
                , Svg.Attributes.style "transition: 0.5s ease; transform: rotate(-45deg) translate(-15px, 6px) scale(0.7);"
                )

            else
                ( Svg.Attributes.style "transition: 0.5s ease;"
                , Svg.Attributes.style "transition: 0.5s ease;"
                , Svg.Attributes.style "transition: 0.5s ease;"
                )
    in
    case model.session.device.class of
        Desktop ->
            column [ width fill, paddingEach { edges | bottom = 100 } ]
                [ row
                    [ centerX
                    , spacing 120
                    , paddingEach { edges | bottom = 30, top = 50 }
                    , Region.navigation
                    ]
                    [ link []
                        { url = "/"
                        , label =
                            image [ width (fill |> maximum 120) ]
                                { src = Util.getPng "echoicon"
                                , description = "echo logo"
                                }
                        }
                    , link []
                        { url = "/" ++ Program.route
                        , label = el [ Font.bold, Font.size 26 ] <| text Program.title
                        }
                    , link []
                        { url = "/" ++ Bedrifter.route
                        , label = el [ Font.bold, Font.size 26 ] <| text Bedrifter.title
                        }
                    , link []
                        { url = "/" ++ Om.route
                        , label = el [ Font.bold, Font.size 26 ] <| text Om.title
                        }
                    , link []
                        { url = "/" ++ Profil.route
                        , label = html <| Icon.viewStyled [ HtmlA.width 30 ] FontAwesome.Regular.user
                        }
                    ]
                , html <|
                    Html.hr
                        [ HtmlA.style "color" "black"
                        , HtmlA.style "background-color" "black"
                        , HtmlA.style "height" "1px"
                        , HtmlA.style "width" "60%"
                        ]
                        []
                ]

        _ ->
            column [ paddingEach { edges | bottom = 50 }, width fill ]
                [ row
                    [ centerX
                    , Region.navigation
                    , spacing 200
                    , paddingEach { edges | top = 25, bottom = 15 }
                    , width fill
                    , Background.color Theme.foreground
                    ]
                    [ link [ width fill, alignLeft ]
                        { url = "/"
                        , label =
                            image [ width (fill |> maximum 80) ]
                                { src = Util.getPng "echoicon"
                                , description = "echo logo"
                                }
                        }
                    , Input.button
                        [ paddingEach { edges | right = 20 }
                        , htmlAttribute (HtmlA.style "z-index" "1")
                        , width fill
                        , alignRight
                        ]
                        { onPress = Just <| ToggleDrawer
                        , label =
                            html <|
                                Svg.svg [ Svg.Attributes.width "50", Svg.Attributes.height "40" ]
                                    [ Svg.line
                                        [ Svg.Attributes.stroke "#555555"
                                        , Svg.Attributes.strokeWidth "4"
                                        , Svg.Attributes.x1 "0"
                                        , Svg.Attributes.y1 "5"
                                        , Svg.Attributes.x2 "50"
                                        , Svg.Attributes.y2 "5"
                                        , line1Attr
                                        ]
                                        []
                                    , Svg.line
                                        [ Svg.Attributes.stroke "#555555"
                                        , Svg.Attributes.strokeWidth "4"
                                        , Svg.Attributes.x1 "0"
                                        , Svg.Attributes.y1 "20"
                                        , Svg.Attributes.x2 "50"
                                        , Svg.Attributes.y2 "20"
                                        , line2Attr
                                        ]
                                        []
                                    , Svg.line
                                        [ Svg.Attributes.stroke "#555555"
                                        , Svg.Attributes.strokeWidth "4"
                                        , Svg.Attributes.x1 "0"
                                        , Svg.Attributes.y1 "35"
                                        , Svg.Attributes.x2 "50"
                                        , Svg.Attributes.y2 "35"
                                        , line3Attr
                                        ]
                                        []
                                    ]
                        }
                    ]
                ]


footer : Model -> Element msg
footer model =
    row
        [ centerX
        , alignBottom
        , padding 20
        , width fill
        , spacing 20
        , padding 50
        ]
        [ el
            [ Font.size 36
            , centerX
            ]
            (text "echo bedriftstur")
        , text <| Session.keyToString model.session.apiKey
        , column []
            [ row []
                [ html <| Icon.viewStyled [ HtmlA.width 80 ] FontAwesome.Brands.githubSquare
                , html <| Icon.viewStyled [ HtmlA.width 80 ] FontAwesome.Brands.linkedin
                ]
            , row []
                [ html <| Icon.viewStyled [ HtmlA.width 80 ] FontAwesome.Solid.envelope ]
            ]
        ]


view : Model -> Browser.Document Msg
view model =
    let
        ( title, body ) =
            case model.page of
                Hjem ->
                    ( Hjem.title
                    , Hjem.view model.session.device
                    )

                Profil ->
                    ( Profil.title
                    , Element.map GotProfilMsg <| Profil.view model.profilModel
                    )

                Program ->
                    ( Program.title
                    , Program.view
                    )

                Bedrifter ->
                    ( Bedrifter.title
                    , Bedrifter.view
                    )

                Om ->
                    ( Om.title
                    , Om.view model.session.device
                    )

                NotFound ->
                    ( NotFound.title
                    , NotFound.view
                    )
    in
    { title = title
    , body =
        [ layout
            [ Font.family [ Font.typeface "IBM Plex Sans" ], Background.color Theme.background ]
            (column
                [ height fill
                , width fill
                , htmlAttribute <| HtmlA.style "overflow" "hidden"
                , inFront <| drawer model
                ]
                [ header model, body ]
            )
        ]
    }


main : Program Encode.Value Model Msg
main =
    Browser.application
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        , onUrlChange = UrlChanged
        , onUrlRequest = UrlRequested
        }
