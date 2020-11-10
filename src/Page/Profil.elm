port module Page.Profil exposing (Model, Msg, init, route, subscriptions, title, toSession, update, updateSession, view)

import Cred exposing (Cred)
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input
import Email exposing (Email(..))
import Error exposing (Error)
import Json.Decode as Decode
import Json.Encode as Encode
import LocalStorage exposing (LocalStorageData, LocalStorageToken)
import Session exposing (Session)
import Theme


port isSignInWithEmailLink : (Encode.Value -> msg) -> Sub msg


port saveLocalStorage : Encode.Value -> Cmd msg


port retrieveLocalStorage : Encode.Value -> Cmd msg


port retrieveLocalStorageResult : (Encode.Value -> msg) -> Sub msg


port signIn : Encode.Value -> Cmd msg


port signInSucceeded : (Encode.Value -> msg) -> Sub msg


port signInError : (Encode.Value -> msg) -> Sub msg


port sendSignInLink : Encode.Value -> Cmd msg


port sendSignInLinkSucceeded : (Encode.Value -> msg) -> Sub msg


port sendSignInLinkError : (Encode.Value -> msg) -> Sub msg


type Msg
    = IsSignInWithEmailLink Decode.Value
    | SaveLocalStorage LocalStorageData
    | RetrieveLocalStorage LocalStorageToken
    | RetrieveLocalStorageResult Decode.Value
    | SignInSucceeded Decode.Value
    | SignInError Decode.Value
    | SendSignInLink
    | SendSignInLinkSucceeded Decode.Value
    | SendSignInLinkError Decode.Value
    | TypedEmail String


type alias Model =
    { session : Session
    , emailInput : Email
    , linkSent : Bool
    , localStorageResult : String
    , error : Maybe Error
    }


init : Session -> Model
init session =
    { session = session
    , emailInput = Email ""
    , linkSent = False
    , localStorageResult = ""
    , error = Nothing
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ isSignInWithEmailLink IsSignInWithEmailLink
        , retrieveLocalStorageResult RetrieveLocalStorageResult
        , sendSignInLinkSucceeded SendSignInLinkSucceeded
        , sendSignInLinkError SendSignInLinkError
        , signInSucceeded SignInSucceeded
        , signInError SignInError
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        IsSignInWithEmailLink _ ->
            let
                data =
                    Encode.object
                        [ ( "email", Encode.string model.localStorageResult )
                        , ( "link", Encode.string url )
                        ]
            in
            ( model, signIn data )

        SaveLocalStorage localStorageData ->
            ( model, saveLocalStorage (LocalStorage.encodeSave localStorageData) )

        RetrieveLocalStorage localStorageToken ->
            ( model, retrieveLocalStorage (LocalStorage.encodeRetrieval localStorageToken) )

        RetrieveLocalStorageResult json ->
            case Decode.decodeValue Decode.string json of
                Ok result ->
                    ( { model | localStorageResult = result }, Cmd.none )

                Err _ ->
                    ( { model | error = Just Error.JsonParseError }, Cmd.none )

        SendSignInLink ->
            ( model, sendSignInLink (Email.encodeForSignInLink model.emailInput) )

        SendSignInLinkSucceeded json ->
            ( { model | linkSent = True }, Cmd.none )

        SendSignInLinkError json ->
            ( { model | error = Error.decode json }, Cmd.none )

        SignInSucceeded json ->
            let
                session =
                    model.session

                ( newSession, error ) =
                    case Cred.decode json of
                        Just cred ->
                            ( { session | cred = Just cred }, Nothing )

                        Nothing ->
                            ( { session | cred = Nothing }, Just Error.JsonParseError )
            in
            ( { model | session = newSession, error = error }, Cmd.none )

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


updateSession : Model -> Session -> Model
updateSession model session =
    { model | session = session }
