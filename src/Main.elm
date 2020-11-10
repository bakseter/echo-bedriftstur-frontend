port module Main exposing (main)

import Api
import Browser
import Browser.Navigation
import Cred
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Error
import FontAwesome.Brands
import FontAwesome.Icon as Icon
import FontAwesome.Regular
import FontAwesome.Solid
import Html.Attributes
import Json.Decode as Decode
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


port userStatusChanged : (Encode.Value -> msg) -> Sub msg


type Msg
    = UrlChanged Url.Url
    | UrlRequested Browser.UrlRequest
    | UserStatusChanged Decode.Value
    | GotProfilMsg Profil.Msg
    | GotBedrifterMsg Bedrifter.Msg
    | GotProgramMsg Program.Msg
    | GotOmMsg Om.Msg


type Model
    = Hjem Hjem.Model
    | Profil Profil.Model
    | Bedrifter Bedrifter.Model
    | Program Program.Model
    | Om Om.Model
    | NotFound NotFound.Model


init : Encode.Value -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init apiKeyJson url navKey =
    let
        apiKey =
            Api.decodeKey apiKeyJson
    in
    changeRouteTo url <| Session navKey apiKey Nothing


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        pageSubs =
            case model of
                Profil profil ->
                    Sub.map GotProfilMsg <| Profil.subscriptions profil

                Bedrifter bedrifter ->
                    Sub.map GotBedrifterMsg <| Bedrifter.subscriptions bedrifter

                Program program ->
                    Sub.map GotProgramMsg <| Program.subscriptions program

                Om om ->
                    Sub.map GotOmMsg <| Om.subscriptions om

                _ ->
                    Sub.none

        globalSubs =
            Sub.batch
                [ userStatusChanged UserStatusChanged ]
    in
    Sub.batch [ pageSubs ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( UrlRequested request, _ ) ->
            case request of
                Browser.Internal url ->
                    ( model, Browser.Navigation.pushUrl (.navKey <| toSession model) (Url.toString url) )

                Browser.External href ->
                    ( model, Browser.Navigation.load href )

        ( UrlChanged url, _ ) ->
            changeRouteTo url (toSession model)

        ( UserStatusChanged json, _ ) ->
            let
                session =
                    toSession model

                newSession =
                    case Cred.decode json of
                        Just cred ->
                            { session | cred = Just cred }

                        Nothing ->
                            { session | cred = Nothing }
            in
            ( updateSession model newSession, Cmd.none )

        ( GotProfilMsg pageMsg, Profil profil ) ->
            let
                ( newModel, cmd ) =
                    Profil.update pageMsg profil
            in
            ( Profil newModel, Cmd.map GotProfilMsg cmd )

        ( GotBedrifterMsg pageMsg, Bedrifter bedrifter ) ->
            let
                ( newModel, cmd ) =
                    Bedrifter.update pageMsg bedrifter
            in
            ( Bedrifter newModel, Cmd.map GotBedrifterMsg cmd )

        ( GotProgramMsg pageMsg, Program program ) ->
            let
                ( newModel, cmd ) =
                    Program.update pageMsg program
            in
            ( Program newModel, Cmd.map GotProgramMsg cmd )

        ( GotOmMsg pageMsg, Om om ) ->
            let
                ( newModel, cmd ) =
                    Om.update pageMsg om
            in
            ( Om newModel, Cmd.map GotOmMsg cmd )

        _ ->
            ( model, Cmd.none )


toSession : Model -> Session
toSession model =
    case model of
        Hjem hjem ->
            Hjem.toSession hjem

        Profil profil ->
            Profil.toSession profil

        Program program ->
            Program.toSession program

        Bedrifter bedrifter ->
            Bedrifter.toSession bedrifter

        Om om ->
            Om.toSession om

        NotFound notFound ->
            NotFound.toSession notFound


updateSession : Model -> Session -> Model
updateSession model session =
    case model of
        Hjem hjem ->
            Hjem <| Hjem.updateSession hjem session

        Profil profil ->
            Profil <| Profil.updateSession profil session

        Program program ->
            Program <| Program.updateSession program session

        Bedrifter bedrifter ->
            Bedrifter <| Bedrifter.updateSession bedrifter session

        Om om ->
            Om <| Om.updateSession om session

        NotFound notFound ->
            NotFound <| NotFound.updateSession notFound session


changeRouteTo : Url.Url -> Session -> ( Model, Cmd Msg )
changeRouteTo url session =
    case Page.fromUrl url of
        Page.Hjem ->
            ( Hjem (Hjem.init session), Cmd.none )

        Page.Profil ->
            ( Profil (Profil.init session)
            , Cmd.map GotProfilMsg <|
                Cmd.batch []
            )

        Page.Program ->
            ( Program (Program.init session), Cmd.none )

        Page.Bedrifter ->
            ( Bedrifter (Bedrifter.init session)
            , Cmd.map GotBedrifterMsg <|
                Cmd.batch []
            )

        Page.Om ->
            ( Om (Om.init session)
            , Cmd.map GotOmMsg <|
                Cmd.batch []
            )

        Page.NotFound ->
            ( NotFound (NotFound.init session)
            , Cmd.none
            )


header : Element msg
header =
    row
        [ Background.color Theme.foreground
        , width fill
        , centerX
        , paddingEach { top = 75, left = 0, right = 0, bottom = 75 }
        ]
        [ row
            [ centerX
            , spacing 100
            ]
            [ link []
                { url = "/"
                , label =
                    image [ width (fill |> maximum 100) ]
                        { src = "/img/echoicon.png", description = "echo logo" }
                }
            , link []
                { url = "/" ++ Program.route
                , label = text Program.title
                }
            , link []
                { url = "/" ++ Bedrifter.route
                , label = text Bedrifter.title
                }
            , link []
                { url = "/" ++ Om.route
                , label = text Om.title
                }
            , link []
                { url = "/" ++ Profil.route
                , label = html <| Icon.viewStyled [ Html.Attributes.width 20 ] FontAwesome.Regular.user
                }
            ]
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
            case model of
                Hjem _ ->
                    ( Hjem.title
                    , Hjem.view
                    )

                Profil profil ->
                    ( Profil.title
                    , Element.map GotProfilMsg <| Profil.view profil
                    )

                Program program ->
                    ( Program.title
                    , Element.map GotProgramMsg <| Program.view program
                    )

                Bedrifter bedrifter ->
                    ( Bedrifter.title
                    , Element.map GotBedrifterMsg <| Bedrifter.view bedrifter
                    )

                Om om ->
                    ( Om.title
                    , Element.map GotOmMsg <| Om.view om
                    )

                NotFound _ ->
                    ( NotFound.title
                    , NotFound.view
                    )
    in
    { title = title
    , body =
        [ layout
            [ Font.family [ Font.typeface "IBM Plex Sans" ], Background.color Theme.background ]
            (column [ height fill, width fill ]
                [ header, body ]
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
