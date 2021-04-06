module Main exposing (main)

import Browser
import Browser.Navigation
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Region as Region
import FontAwesome.Brands
import FontAwesome.Icon as Icon
import FontAwesome.Regular
import FontAwesome.Solid
import Html.Attributes
import Json.Encode as Encode
import Page exposing (Page(..))
import Page.Bedrifter as Bedrifter
import Page.Hjem as Hjem
import Page.NotFound as NotFound
import Page.Om as Om
import Page.Profil as Profil
import Page.Program as Program
import Session exposing (Session)
import Theme
import Url
import Util exposing (edges)


type Msg
    = UrlChanged Url.Url
    | UrlRequested Browser.UrlRequest
    | GotProfilMsg Profil.Msg


type alias Model =
    { page : Page
    , session : Session
    , profilModel : Profil.Model
    }


init : Encode.Value -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init json url navKey =
    let
        page =
            Page.fromUrl url

        apiKey =
            Session.decodeApiKey json

        session =
            Session navKey apiKey

        ( profilModel, profilCmd ) =
            Profil.init session
    in
    ( { page = page
      , session = session
      , profilModel = profilModel
      }
    , Cmd.map GotProfilMsg profilCmd
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


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


header : Model -> Element msg
header _ =
    row
        [ centerX
        , spacing 120
        , paddingEach { edges | top = 50, bottom = 100 }
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
            , label = html <| Icon.viewStyled [ Html.Attributes.width 30 ] FontAwesome.Regular.user
            }
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
                [ html <| Icon.viewStyled [ Html.Attributes.width 80 ] FontAwesome.Brands.githubSquare
                , html <| Icon.viewStyled [ Html.Attributes.width 80 ] FontAwesome.Brands.linkedin
                ]
            , row []
                [ html <| Icon.viewStyled [ Html.Attributes.width 80 ] FontAwesome.Solid.envelope ]
            ]
        ]


view : Model -> Browser.Document Msg
view model =
    let
        ( title, body ) =
            case model.page of
                Hjem ->
                    ( Hjem.title
                    , Hjem.view
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
                    , Om.view
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
            (column [ height fill, width fill ]
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
