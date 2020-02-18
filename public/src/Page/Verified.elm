port module Page.Verified exposing (init, subscriptions, update, view, Model, Msg)

import Html exposing (Html, div, span, br, text, p)
import Html.Attributes exposing (class, id)
import Url
import Url.Parser exposing ((<?>))
import Url.Parser.Query as Query
import Json.Encode

type Msg
    = Start Json.Encode.Value
    | LogInSucceeded Json.Encode.Value

type AuthCode
    = Init
    | AttemptedLogin String
    | InvalidQuery

type alias Model =
    { url : Url.Url
    , authCode : AuthCode
    , userInfo : String
    }

port verifyUser : Json.Encode.Value -> Cmd msg
port logInSucceeded : (Json.Encode.Value -> msg) -> Sub msg
port start : (Json.Encode.Value -> msg) -> Sub msg

init : Url.Url -> Model
init url =
    { url = url
    , authCode = Init
    , userInfo = ""
    }

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch 
        [ logInSucceeded LogInSucceeded
        , start Start
        ]

update : Msg  -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Start _ ->
            let authCode = getAuthCode model.url
            in
                case authCode of
                    AttemptedLogin _ ->
                        let cmd = verifyUser
                                    (Json.Encode.object
                                        [ ( "doLogIn", Json.Encode.bool True ) ]
                                    )
                        in (model, cmd)
                    _ ->
                        (model, Cmd.none)
        LogInSucceeded userInfo ->
            let info = Json.Encode.encode 2 userInfo
            in ({ model | userInfo = info }, Cmd.none)

view : Model -> Html Msg
view model =
    div [ class "verified" ]
        [ p [] 
            [ text (handleQuery (getAuthCode model.url)) ]
        , p []
            [ text model.userInfo ]
        ]

handleQuery : AuthCode -> String
handleQuery msg =
    case msg of
        AttemptedLogin str ->
            "Du er nå verifisert og blir logget inn straks."
        InvalidQuery ->
            "Innlogginslinken er ikke gyldig. Prøv igjen"
        Init ->
            ""

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
