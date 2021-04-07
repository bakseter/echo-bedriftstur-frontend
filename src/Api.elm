module Api exposing (..)

import Database.Account as Account exposing (Account)
import Database.Email exposing (Email(..))
import Database.Registration as Registration exposing (Registration)
import Database.UserInfo as UserInfo exposing (UserInfo)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Url.Builder as Builder


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
    = UserAccount
      --
    | UserUserInfo
      --
    | UserRegistration
      --
    | AdminAccount
    | AdminUserInfo
    | AdminRegistration


buildUrl : Action -> Endpoint
buildUrl action =
    let
        databaseUrl =
            "http://localhost:8080"
    in
    case action of
        UserAccount ->
            Endpoint <|
                Builder.crossOrigin databaseUrl
                    [ "user", "account" ]
                    []

        UserUserInfo ->
            Endpoint <|
                Builder.crossOrigin databaseUrl
                    [ "user", "userInfo" ]
                    []

        UserRegistration ->
            Endpoint <|
                Builder.crossOrigin databaseUrl
                    [ "user", "registration" ]
                    []

        AdminAccount ->
            Endpoint <|
                Builder.crossOrigin databaseUrl
                    [ "admin", "account" ]
                    []

        AdminUserInfo ->
            Endpoint <|
                Builder.crossOrigin databaseUrl
                    [ "admin", "userInfo" ]
                    []

        AdminRegistration ->
            Endpoint <|
                Builder.crossOrigin databaseUrl
                    [ "admin", "registration" ]
                    []


createAccount : Account -> UserInfo -> (Result Http.Error () -> msg) -> Cmd msg
createAccount acc userInfo msg =
    request
        { method = "POST"
        , headers = []
        , url = buildUrl UserAccount
        , body = Http.jsonBody <| encodeForCreateAccount acc userInfo
        , expect = Http.expectWhatever msg
        , timeout = Nothing
        , tracker = Nothing
        }


encodeForCreateAccount : Account -> UserInfo -> Encode.Value
encodeForCreateAccount acc userInfo =
    Encode.object
        [ ( "caUnsafeAccount", Account.encode acc )
        , ( "caUserInfo", UserInfo.encode userInfo )
        ]


getUserInfo : (Result Http.Error UserInfo -> msg) -> Cmd msg
getUserInfo msg =
    request
        { method = "GET"
        , headers = []
        , url = buildUrl UserUserInfo
        , body = Http.emptyBody
        , expect = Http.expectJson msg UserInfo.decoder
        , timeout = Nothing
        , tracker = Nothing
        }


updateUserInfo : UserInfo -> (Result Http.Error () -> msg) -> Cmd msg
updateUserInfo userInfo msg =
    request
        { method = "PUT"
        , headers = []
        , url = buildUrl UserUserInfo
        , body = Http.jsonBody <| UserInfo.encode userInfo
        , expect = Http.expectWhatever msg
        , timeout = Nothing
        , tracker = Nothing
        }


getRegistration : (Result Http.Error Registration -> msg) -> Cmd msg
getRegistration msg =
    request
        { method = "GET"
        , headers = []
        , url = buildUrl UserRegistration
        , body = Http.emptyBody
        , expect = Http.expectJson msg Registration.decoder
        , timeout = Nothing
        , tracker = Nothing
        }


submitRegistration : Registration -> (Result Http.Error () -> msg) -> Cmd msg
submitRegistration reg msg =
    request
        { method = "POST"
        , headers = []
        , url = buildUrl UserRegistration
        , body = Http.jsonBody <| Registration.encode reg
        , expect = Http.expectWhatever msg
        , timeout = Nothing
        , tracker = Nothing
        }


getAllAccounts : (Result Http.Error (List Account) -> msg) -> Cmd msg
getAllAccounts msg =
    request
        { method = "GET"
        , headers = []
        , url = buildUrl AdminAccount
        , body = Http.emptyBody
        , expect = Http.expectJson msg <| Decode.list Account.decoder
        , timeout = Nothing
        , tracker = Nothing
        }


getAllUserInfo : (Result Http.Error (List UserInfo) -> msg) -> Cmd msg
getAllUserInfo msg =
    request
        { method = "GET"
        , headers = []
        , url = buildUrl AdminUserInfo
        , body = Http.emptyBody
        , expect = Http.expectJson msg <| Decode.list UserInfo.decoder
        , timeout = Nothing
        , tracker = Nothing
        }


getAllRegistrations : (Result Http.Error (List Registration) -> msg) -> Cmd msg
getAllRegistrations msg =
    request
        { method = "GET"
        , headers = []
        , url = buildUrl AdminRegistration
        , body = Http.emptyBody
        , expect = Http.expectJson msg <| Decode.list Registration.decoder
        , timeout = Nothing
        , tracker = Nothing
        }
