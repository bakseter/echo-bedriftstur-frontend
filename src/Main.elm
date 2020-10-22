module Main exposing (main)

import Browser
import Browser.Navigation
import Html exposing (Html, a, div, img, span, text)
import Html.Attributes exposing (alt, class, href, id, src)
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
import Svg
import Svg.Attributes exposing (x1, x2, y1, y2)
import Url


type Msg
    = UrlChanged Url.Url
    | UrlRequested Browser.UrlRequest
    | GotLoggInnMsg LoggInn.Msg
    | GotBedrifterMsg Bedrifter.Msg
    | GotProgramMsg Program.Msg
    | GotOmMsg Om.Msg



{-
   | AnimateLogoText Animation.Msg
   | LogoTextCursor Bool
   | RemoveCharLogoText
   | AddCharLogoText String
-}
-- The names displayed in the typewriter animation


type Name
    = Bedriftstur
    | Mnemonic
    | Computas
    | Cisco
    | Knowit
    | Dnb
    | Bekk


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
            Session.decodeApiKey apiKeyJson
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

        ( GotProgramMsg pageMsg, Program program ) ->
            let
                ( newModel, cmd ) =
                    Program.update pageMsg program
            in
            ( Program newModel, Cmd.map GotProgramMsg cmd )

        ( GotBedrifterMsg pageMsg, Bedrifter bedrifter ) ->
            let
                ( newModel, cmd ) =
                    Bedrifter.update pageMsg bedrifter
            in
            ( Bedrifter newModel, Cmd.map GotBedrifterMsg cmd )

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
            ( Program (Program.init session)
            , Cmd.map GotProgramMsg <|
                Cmd.batch []
            )

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


header : Model -> Html Msg
header model =
    div [ class "menu" ]
        [ div [ id "logo-wrapper" ]
            [ a [ href "/" ]
                [ img [ id "logo", alt "logo", src "/img/echoicon.png" ] [] ]
            ]
        , div [ id "logo-text" ]
            [ span [] [ text "echo " ]
            , span
                []
                [ text "" ]
            ]
        , Svg.svg [ Svg.Attributes.id "navbtn", Svg.Attributes.width "50", Svg.Attributes.height "40" ]
            [ Svg.line
                [ Svg.Attributes.class "navbtn-line", Svg.Attributes.id ("first-line" ++ ""), x1 "0", x2 "50", y1 "5", y2 "5" ]
                []
            , Svg.line
                [ Svg.Attributes.class "navbtn-line", Svg.Attributes.id ("middle-line" ++ ""), x1 "0", x2 "50", y1 "20", y2 "20" ]
                []
            , Svg.line
                [ Svg.Attributes.class "navbtn-line", Svg.Attributes.id ("second-line" ++ ""), x1 "0", x2 "50", y1 "35", y2 "35" ]
                []
            ]
        , div [ class "" ]
            (navbar model)
        ]


userInfo : Model -> List (Html Msg)
userInfo =
    \_ ->
        []


navbar : Model -> List (Html Msg)
navbar model =
    [ span [] []
    , div [ class "navbar-item" ]
        (userInfo model)
    , span [] []
    , a [ class "navbar-item", href ("/" ++ Info.route) ] [ text "Informasjon" ]
    , a [ class "navbar-item", href ("/" ++ Program.route) ] [ text "Program" ]
    , a [ class "navbar-item", href ("/" ++ Bedrifter.route) ] [ text "Bedrifter" ]
    , a [ class "navbar-item", href ("/" ++ Om.route) ] [ text "Om oss" ]
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

                Info _ ->
                    ( Info.title
                    , Info.view
                    )

                LoggInn loggInn ->
                    ( LoggInn.title
                    , Html.map GotLoggInnMsg <| LoggInn.view loggInn
                    )

                Program program ->
                    ( Program.title
                    , Html.map GotProgramMsg <| Program.view program
                    )

                Bedrifter bedrifter ->
                    ( Bedrifter.title
                    , Html.map GotBedrifterMsg <| Bedrifter.view bedrifter
                    )

                Om om ->
                    ( Om.title
                    , Html.map GotOmMsg <| Om.view om
                    )

                NotFound _ ->
                    ( NotFound.title
                    , NotFound.view
                    )
    in
    { title = title
    , body = [ header model, body ]
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
