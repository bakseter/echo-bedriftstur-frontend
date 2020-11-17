module Main exposing (main)

import Assets exposing (Assets)
import Browser
import Browser.Navigation
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
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


type Msg
    = UrlChanged Url.Url
    | UrlRequested Browser.UrlRequest
    | GotHjemMsg Hjem.Msg
    | GotProfilMsg Profil.Msg
    | GotBedrifterMsg Bedrifter.Msg
    | GotProgramMsg Program.Msg
    | GotOmMsg Om.Msg


type alias Model =
    { hjem : Hjem.Model
    , profil : Profil.Model
    , bedrifter : Bedrifter.Model
    , program : Program.Model
    , om : Om.Model
    , page : Page
    , session : Session
    , assets : List Assets
    }


init : Encode.Value -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init json url navKey =
    let
        page =
            Page.fromUrl url

        session =
            Session navKey Nothing

        assets =
            case Assets.decode json of
                Just a ->
                    a

                Nothing ->
                    []

        ( hjem, hjemCmd ) =
            Hjem.init session assets

        ( profil, profilCmd ) =
            Profil.init session assets

        ( bedrifter, bedrifterCmd ) =
            Bedrifter.init session assets

        ( program, programCmd ) =
            Program.init session assets

        ( om, omCmd ) =
            Om.init session assets
    in
    ( { hjem = hjem
      , profil = profil
      , bedrifter = bedrifter
      , program = program
      , om = om
      , page = page
      , session = session
      , assets = assets
      }
    , Cmd.batch <|
        [ Cmd.map GotHjemMsg hjemCmd
        , Cmd.map GotProfilMsg profilCmd
        , Cmd.map GotBedrifterMsg bedrifterCmd
        , Cmd.map GotProgramMsg programCmd
        , Cmd.map GotOmMsg omCmd
        ]
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map GotProfilMsg <| Profil.subscriptions model.profil
        , Sub.map GotBedrifterMsg <| Bedrifter.subscriptions model.bedrifter
        , Sub.map GotProgramMsg <| Program.subscriptions model.program
        , Sub.map GotOmMsg <| Om.subscriptions model.om
        ]


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

        GotHjemMsg pageMsg ->
            let
                ( newModel, cmd ) =
                    Hjem.update pageMsg model.hjem
            in
            ( { model | hjem = newModel }, Cmd.map GotHjemMsg cmd )

        GotProfilMsg pageMsg ->
            let
                ( newModel, cmd ) =
                    Profil.update pageMsg model.profil
            in
            ( { model | profil = newModel }, Cmd.map GotProfilMsg cmd )

        GotBedrifterMsg pageMsg ->
            let
                ( newModel, cmd ) =
                    Bedrifter.update pageMsg model.bedrifter
            in
            ( { model | bedrifter = newModel }, Cmd.map GotBedrifterMsg cmd )

        GotProgramMsg pageMsg ->
            let
                ( newModel, cmd ) =
                    Program.update pageMsg model.program
            in
            ( { model | program = newModel }, Cmd.map GotProgramMsg cmd )

        GotOmMsg pageMsg ->
            let
                ( newModel, cmd ) =
                    Om.update pageMsg model.om
            in
            ( { model | om = newModel }, Cmd.map GotOmMsg cmd )


header : Model -> Element msg
header model =
    row
        [ centerX
        , spacing 120
        ]
        [ link []
            { url = "/"
            , label =
                image [ width (fill |> maximum 120) ]
                    { src = Assets.get model.assets "echoicon", description = "echo logo" }
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


footer : Model -> Element Msg
footer _ =
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
                    , Element.map GotHjemMsg <| Hjem.view model.hjem
                    )

                Profil ->
                    ( Profil.title
                    , Element.map GotProfilMsg <| Profil.view model.profil
                    )

                Program ->
                    ( Program.title
                    , Element.map GotProgramMsg <| Program.view model.program
                    )

                Bedrifter ->
                    ( Bedrifter.title
                    , Element.map GotBedrifterMsg <| Bedrifter.view model.bedrifter
                    )

                Om ->
                    ( Om.title
                    , Element.map GotOmMsg <| Om.view model.om
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
