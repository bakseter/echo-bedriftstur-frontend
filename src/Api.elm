module Api exposing (..)

import Cred exposing (Cred, IdToken(..), RefreshToken(..))
import Email exposing (Email(..))
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Password exposing (Password(..))
import Session exposing (Session)
import Uid exposing (Uid(..))
import User exposing (User)


type Endpoint
    = Endpoint String


request :
    { method : String
    , headers : List Http.Header
    , url : Endpoint
    , body : Http.Body
    , expect : Http.Expect msg
    , timeout : Maybe Float
    , tracker : Maybe String
    }
    -> Cmd msg
request req =
    let
        (Endpoint url) =
            req.url
    in
    Http.request
        { method = req.method
        , headers = req.headers
        , url = url
        , body = req.body
        , expect = req.expect
        , timeout = req.timeout
        , tracker = req.tracker
        }


register : (Result Http.Error Cred -> msg) -> Email -> Password -> Session -> Cmd msg
register msg email password session =
    request
        { method = "POST"
        , headers = []
        , url = Endpoint <| "https://identitytoolkit.googleapis.com/v1/accounts:signUp?key=" ++ Session.keyToString session.apiKey
        , body = Http.jsonBody <| encodeForRegister email password
        , expect = Http.expectJson msg Cred.credDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


encodeForRegister : Email -> Password -> Encode.Value
encodeForRegister email password =
    Encode.object
        [ ( "email", Encode.string <| Email.toString email )
        , ( "password", Encode.string <| Password.toString password )
        , ( "returnSecureToken", Encode.bool True )
        ]


signIn : (Result Http.Error Cred -> msg) -> Email -> Password -> Session -> Cmd msg
signIn msg email password session =
    request
        { method = "POST"
        , headers = []
        , url = Endpoint <| "https://identitytoolkit.googleapis.com/v1/accounts:signInWithPassword?key=" ++ Session.keyToString session.apiKey
        , body = Http.jsonBody <| encodeForSignIn email password
        , expect = Http.expectJson msg Cred.credDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


encodeForSignIn : Email -> Password -> Encode.Value
encodeForSignIn email password =
    Encode.object
        [ ( "email", Encode.string <| Email.toString email )
        , ( "password", Encode.string <| Password.toString password )
        , ( "returnSecureToken", Encode.bool True )
        ]


refreshIdToken : (Result Http.Error Cred -> msg) -> Session -> Cmd msg
refreshIdToken msg session =
    case session.cred of
        Just cred ->
            request
                { method = "POST"
                , headers = []
                , url = Endpoint <| "https://securetoken.googleapis.com/v1/token?key=" ++ Session.keyToString session.apiKey
                , body = Http.jsonBody <| encodeForRefreshIdToken cred.refreshToken
                , expect = Http.expectJson msg Cred.credDecoder
                , timeout = Nothing
                , tracker = Nothing
                }

        Nothing ->
            Debug.log "cred is nothing lol" Cmd.none


encodeForRefreshIdToken : RefreshToken -> Encode.Value
encodeForRefreshIdToken (RefreshToken token) =
    Encode.object
        [ ( "grant_type", Encode.string "refresh_token" )
        , ( "refresh_token", Encode.string token )
        ]


getUserData : (Result Http.Error User -> msg) -> Session -> Cmd msg
getUserData msg session =
    case session.cred of
        Just cred ->
            let
                (IdToken token _) =
                    cred.idToken
            in
            request
                { method = "GET"
                , headers = []
                , url = Endpoint <| "https://echo-bedriftstur-dev-60683.firebaseio.com/users/" ++ Uid.toString cred.uid ++ ".json?auth=" ++ token
                , body = Http.emptyBody
                , expect = Http.expectJson msg User.userDecoder
                , timeout = Nothing
                , tracker = Nothing
                }

        Nothing ->
            Debug.log "cred is nothing lol @ getUserData" Cmd.none
