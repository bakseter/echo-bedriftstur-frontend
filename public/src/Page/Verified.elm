port module Page.Verified exposing (init, subscriptions, update, view, Model, Msg)

import Html exposing (Html, div, span, br, text, p)
import Html.Attributes exposing (class, id)
import Url
import Url.Builder
import Url.Parser exposing ((<?>))
import Url.Parser.Query as Query
import Json.Encode
import Json.Decode
import Browser.Navigation

type Msg
    = LogInSucceeded Json.Encode.Value
    | LogInFailed Json.Encode.Value

type AuthCode
    = AttemptedLogin String
    | InvalidQuery

type Error
    = None
    | InvalidEmail
    | ExpiredActionCode
    | InvalidActionCode
    | UserDisabled

type alias Model =
    { url : Url.Url
    , authCode : AuthCode
    , userInfo : String
    , error : Error
    }

port verifyUser : Json.Encode.Value -> Cmd msg
port logInSucceeded : (Json.Encode.Value -> msg) -> Sub msg
port signInWithLinkError : (Json.Encode.Value -> msg) -> Sub msg

init : Url.Url -> Model
init url =
    { url = url
    , authCode = getAuthCode url
    , userInfo = ""
    , error = None
    }

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ logInSucceeded LogInSucceeded
        , signInWithLinkError LogInFailed
        ]

update : Msg  -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        LogInSucceeded userInfo ->
            let info = Json.Encode.encode 2 userInfo
            in ({ model | userInfo = info }, Browser.Navigation.load (redirectUrl model.url))
        LogInFailed json ->
            let jsonStr = Json.Encode.encode 0 json
                error = errorFromString (getErrorCode jsonStr)
            in
                ({ model | error = error }, Cmd.none)

view : Model -> Html Msg
view model =
    div [ class "verified" ]
        [ p [] 
            [ text (handleQuery model) ]
        , p []
            [ text model.userInfo ]
        ]

handleQuery : Model -> String
handleQuery model =
    case model.error of
        None ->
            case (getAuthCode model.url) of
                AttemptedLogin str ->
                    "Du er nå verifisert og blir logget inn straks."
                InvalidQuery ->
                    "Innlogginslinken er ikke gyldig. Prøv igjen"
        _ ->
            errorMessageToUser model.error

getAuthCode : Url.Url -> AuthCode
getAuthCode url =
    let code = Url.Parser.s "verified" <?> Query.string "apiKey"
    in
        case (Url.Parser.parse code url) of
            Just auth ->
                case auth of
                    Just query ->
                        AttemptedLogin query
                    Nothing ->
                        InvalidQuery
            Nothing ->
                InvalidQuery

getErrorCode : String -> String
getErrorCode json =
    case Json.Decode.decodeString (Json.Decode.at ["code"] Json.Decode.string) json of
        Ok code ->
            code
        Err _ ->
            ""

errorFromString : String -> Error
errorFromString str =
    case str of
        "auth/invalid-email" ->
            InvalidEmail
        "auth/expired-action-code" ->
            ExpiredActionCode
        "auth/invalid-action-code" ->
            InvalidActionCode
        "auth/user-disabled" ->
            UserDisabled
        _ ->
            None

errorMessageToUser : Error -> String
errorMessageToUser error =
    case error of
        InvalidEmail ->
            "Mailen du har skrevet inn har ikke riktig format. Prøv igjen"
        ExpiredActionCode ->
            "Innlogginslinken har utløpt. Prøv å send en ny link"
        InvalidActionCode ->
            "Innlogginslinken er ikke gyldig. Dette kan skje om den allerede har blitt brukt"
        UserDisabled ->
            "Brukeren din har blitt deaktivert."
        None ->
            ""

redirectUrl : Url.Url -> String
redirectUrl url =
    Url.Builder.crossOrigin ("https://" ++ url.host) [ "minside" ] []
