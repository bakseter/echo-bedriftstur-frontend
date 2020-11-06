module Main exposing (main)

import Api
import Browser
import Browser.Navigation
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Html.Attributes
import Json.Encode as Encode
import Page exposing (Page(..))
import Page.Bedrifter as Bedrifter
import Page.Hjem as Hjem
import Page.Info as Info
import Page.LoggInn as LoggInn
import Page.NotFound as NotFound
import Page.Om as Om
import Page.Program as Program
import Session exposing (Session)
import Url


type Msg
    = UrlChanged Url.Url
    | UrlRequested Browser.UrlRequest
    | GotLoggInnMsg LoggInn.Msg
    | GotBedrifterMsg Bedrifter.Msg
    | GotProgramMsg Program.Msg
    | GotOmMsg Om.Msg


type Model
    = Hjem Hjem.Model
    | Info Info.Model
    | LoggInn LoggInn.Model
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
    changeRouteTo url <| Session navKey apiKey


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        LoggInn loggInn ->
            Sub.map GotLoggInnMsg <| LoggInn.subscriptions loggInn

        Bedrifter bedrifter ->
            Sub.map GotBedrifterMsg <| Bedrifter.subscriptions bedrifter

        Program program ->
            Sub.map GotProgramMsg <| Program.subscriptions program

        Om om ->
            Sub.map GotOmMsg <| Om.subscriptions om

        _ ->
            Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( UrlRequested request, anyModel ) ->
            case request of
                Browser.Internal url ->
                    ( model, Browser.Navigation.pushUrl (.navKey <| toSession anyModel) (Url.toString url) )

                Browser.External href ->
                    ( model, Browser.Navigation.load href )

        ( UrlChanged url, anyModel ) ->
            changeRouteTo url (toSession anyModel)

        ( GotLoggInnMsg pageMsg, LoggInn loggInn ) ->
            let
                ( newModel, cmd ) =
                    LoggInn.update pageMsg loggInn
            in
            ( LoggInn newModel, Cmd.map GotLoggInnMsg cmd )

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

        Info info ->
            Info.toSession info

        LoggInn loggInn ->
            LoggInn.toSession loggInn

        Program program ->
            Program.toSession program

        Bedrifter bedrifter ->
            Bedrifter.toSession bedrifter

        Om om ->
            Om.toSession om

        NotFound notFound ->
            NotFound.toSession notFound


changeRouteTo : Url.Url -> Session -> ( Model, Cmd Msg )
changeRouteTo url session =
    case Page.fromUrl url of
        Page.Hjem ->
            ( Hjem (Hjem.init session), Cmd.none )

        Page.Info ->
            ( Info (Info.init session), Cmd.none )

        Page.LoggInn ->
            ( LoggInn (LoggInn.init session)
            , Cmd.map GotLoggInnMsg <|
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


header : Model -> Element msg
header _ =
    row [ centerX, spacing 100, paddingEach { top = 75, left = 0, right = 0, bottom = 100 } ] <|
        link []
            { url = "/"
            , label =
                image [ width (fill |> maximum 100) ]
                    { src = "/img/echoicon.png"
                    , description = "Logo"
                    }
            }
            :: List.map
                (\( route, title ) ->
                    Element.link []
                        { url = "/" ++ route
                        , label = el [ Font.bold ] <| Element.text title
                        }
                )
                [ ( Info.route, Info.title )
                , ( LoggInn.route, LoggInn.title )
                , ( Program.route, Program.title )
                , ( Bedrifter.route, Bedrifter.title )
                , ( Om.route, Om.title )
                ]


footer : Model -> Element Msg
footer _ =
    row
        [ alignBottom
        , centerX
        , padding 20
        , Background.color (rgb255 128 128 128)
        , width fill
        ]
        [ el [] (text "echo – Fagutvalget for informatikk") ]


view : Model -> Browser.Document Msg
view model =
    let
        ( title, body ) =
            case model of
                Hjem _ ->
                    ( Hjem.title
                    , Hjem.view
                    )

                Info _ ->
                    ( Info.title
                    , Info.view
                    )

                LoggInn loggInn ->
                    ( LoggInn.title
                    , Element.map GotLoggInnMsg <| LoggInn.view loggInn
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
            [ Font.family [ Font.typeface "IBM Plex Sans" ]
            , htmlAttribute <| Html.Attributes.style "overflow-y" "scroll"
            ]
            (column [ centerX, height fill, width fill ]
                [ header model, body, footer model ]
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
