port module Page.Profil exposing (Model, Msg, init, route, subscriptions, title, toSession, update, view)

import Cred exposing (Cred)
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input
import Email exposing (Email(..))
import Error exposing (Error)
import Json.Decode as Decode
import Json.Encode as Encode
import Session exposing (Session)
import Theme


port userStatusChanged : (Encode.Value -> msg) -> Sub msg


port signInSucceeded : (Encode.Value -> msg) -> Sub msg


port signInError : (Encode.Value -> msg) -> Sub msg


port sendSignInLink : Encode.Value -> Cmd msg


port sendSignInLinkSucceeded : (Encode.Value -> msg) -> Sub msg


port sendSignInLinkError : (Encode.Value -> msg) -> Sub msg


type Msg
    = UserStatusChanged Decode.Value
    | SendSignInLink
    | SendSignInLinkSucceeded Decode.Value
    | SendSignInLinkError Decode.Value
    | SignInSucceeded Decode.Value
    | SignInError Decode.Value
    | TypedEmail String


type alias Model =
    { session : Session
    , emailInput : Email
    , linkSent : Bool
    , cred : Maybe Cred
    , error : Maybe Error
    }


init : Session -> Model
init session =
    { session = session
    , emailInput = Email ""
    , linkSent = False
    , cred = Nothing
    , error = Nothing
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ userStatusChanged UserStatusChanged
        , sendSignInLinkSucceeded SendSignInLinkSucceeded
        , sendSignInLinkError SendSignInLinkError
        , signInSucceeded SignInSucceeded
        , signInError SignInError
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UserStatusChanged json ->
            case Cred.decode json of
                Just cred ->
                    ( { model | cred = Just cred }, Cmd.none )

                Nothing ->
                    ( { model | cred = Nothing, error = Just Error.JsonParseError }, Cmd.none )

        SendSignInLink ->
            ( model, sendSignInLink (Email.encodeForSignInLink model.emailInput) )

        SendSignInLinkSucceeded json ->
            ( { model | linkSent = True }, Cmd.none )

        SendSignInLinkError json ->
            ( { model | error = Error.decode json }, Cmd.none )

        SignInSucceeded json ->
            update (UserStatusChanged json) model

        SignInError json ->
            ( { model | error = Error.decode json }, Cmd.none )

        TypedEmail str ->
            ( { model | emailInput = Email str }, Cmd.none )


view : Model -> Element Msg
view model =
    column [ centerX, spacing 50, padding 100 ]
        [ Input.email [ Input.focusedOnLoad ]
            { onChange = TypedEmail
            , text = Email.toString model.emailInput
            , placeholder = Just <| Input.placeholder [] (text "Email")
            , label = Input.labelHidden "Email"
            }
        , Input.button
            [ Background.color Theme.button
            , padding 10
            , mouseOver [ Background.color (rgb255 255 0 255) ]
            ]
            { onPress = Just SendSignInLink
            , label = text "Logg inn"
            }
        , el [] <|
            if model.linkSent then
                text <| "Linken ble sent til " ++ Email.toString model.emailInput ++ "."

            else
                none
        ]


route : String
route =
    "profil"


title : String
title =
    "Min profil"


toSession : Model -> Session
toSession model =
    model.session
