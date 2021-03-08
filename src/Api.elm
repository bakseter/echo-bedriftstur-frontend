module Api exposing (..)

import Cred exposing (Cred, IdToken(..), RefreshToken(..))
import Email exposing (Email(..))
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Password exposing (Password(..))
import Session exposing (Session)
import Uid exposing (Uid(..))
import Url.Builder as Builder
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


type Action
    = RegisterA
    | SignInA
    | RefreshTokenA
    | CreateUserDataA Cred
    | GetUserDataA Cred


buildUrl : Action -> Session -> Endpoint
buildUrl action session =
    let
        accountUrl =
            "https://identitytoolkit.googleapis.com/v1"

        tokenUrl =
            "https://securetoken.googleapis.com/v1"

        databaseUrl =
            "https://echo-bedriftstur-dev-60683.firebaseio.com"
    in
    case action of
        RegisterA ->
            Endpoint <|
                Builder.crossOrigin accountUrl
                    [ "accounts:signUp" ]
                    [ Builder.string "key" <| Session.keyToString session.apiKey ]

        SignInA ->
            Endpoint <|
                Builder.crossOrigin accountUrl
                    [ "accounts:signInWithPassword" ]
                    [ Builder.string "key" <| Session.keyToString session.apiKey ]

        RefreshTokenA ->
            Endpoint <|
                Builder.crossOrigin accountUrl
                    [ "token" ]
                    [ Builder.string "key" <| Session.keyToString session.apiKey ]

        GetUserDataA cred ->
            let
                (IdToken token _) =
                    cred.idToken
            in
            Endpoint <|
                Builder.crossOrigin databaseUrl
                    [ "users", Uid.toString cred.uid ++ ".json" ]
                    [ Builder.string "auth" token ]

        CreateUserDataA cred ->
            let
                (IdToken token _) =
                    cred.idToken
            in
            Endpoint <|
                Builder.crossOrigin databaseUrl
                    [ "users", Uid.toString cred.uid ++ ".json" ]
                    [ Builder.string "auth" token ]


register : (Result Http.Error Cred -> msg) -> Email -> Password -> Session -> Cmd msg
register msg email password session =
    request
        { method = "POST"
        , headers = []
        , url = buildUrl RegisterA session
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
        , url = buildUrl SignInA session
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
                , url = buildUrl RefreshTokenA session
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


createUserData : (Result Http.Error User -> msg) -> Session -> Email -> Cmd msg
createUserData msg session email =
    case session.cred of
        Just cred ->
            request
                { method = "PUT"
                , headers = []
                , url = buildUrl (CreateUserDataA cred) session
                , body = Http.jsonBody <| encodeForCreateUserData email
                , expect = Http.expectJson msg User.userDecoder
                , timeout = Nothing
                , tracker = Nothing
                }

        Nothing ->
            Debug.log "cred is nothing lol @ createUserData" Cmd.none


encodeForCreateUserData : Email -> Encode.Value
encodeForCreateUserData email =
    Encode.object
        [ ( "email", Encode.string <| Email.toString email )
        ]


getUserData : (Result Http.Error User -> msg) -> Session -> Cmd msg
getUserData msg session =
    case session.cred of
        Just cred ->
            request
                { method = "GET"
                , headers = []
                , url = buildUrl (GetUserDataA cred) session
                , body = Http.emptyBody
                , expect = Http.expectJson msg User.userDecoder
                , timeout = Nothing
                , tracker = Nothing
                }

        Nothing ->
            Debug.log "cred is nothing lol @ getUserData" Cmd.none
