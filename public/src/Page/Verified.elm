port module Page.Verified exposing (init, subscriptions, update, view, Model, Msg)

import Html exposing (Html, div, span, br, text, p, input, button, select, option, h3)
import Html.Attributes exposing (class, id, type_, value, placeholder, disabled)
import Html.Events
import Url
import Url.Builder
import Url.Parser exposing ((<?>))
import Url.Parser.Query as Query
import Json.Encode
import Json.Decode
import Browser.Navigation

type Msg
    = SignInSucceeded Json.Encode.Value
    | SignInFailed Json.Encode.Value
    | SignOutSucceeded Json.Encode.Value
    | SignOutFailed Json.Encode.Value
    | RequestedUserInfo Json.Encode.Value
    | GotUserInfo Json.Encode.Value
    | AttemptSignOut
    | UpdateUserInfo
    | TypedFirstName String
    | TypedLastName String
    | TypedDegree String

type AuthCode
    = AttemptedSignIn String
    | InvalidQuery

type Error
    = NoError
    | InvalidEmail
    | ExpiredActionCode
    | InvalidActionCode
    | UserDisabled

type SubPage
    = Verified
    | MinSide

type Degree
    = None
    | DTEK
    | DVIT
    | DSIK
    | BINF
    | IMØ
    | IKT
    | KOGNI
    | INF
    | PROG

type alias Model =
    { url : Url.Url
    , key : Browser.Navigation.Key
    , authCode : AuthCode
    , userInfo : String
    , error : Error
    , currentSubPage : SubPage
    , email : String
    , firstName : String
    , lastName : String
    , degree : Degree
    }

port signInSucceeded : (Json.Encode.Value -> msg) -> Sub msg
port signInWithLinkError : (Json.Encode.Value -> msg) -> Sub msg

port getUserInfo : Json.Encode.Value -> Cmd msg
port gotUserInfo : (Json.Encode.Value -> msg) -> Sub msg
port updateUserInfo : Json.Encode.Value -> Cmd msg

port attemptSignOut : Json.Encode.Value -> Cmd msg
port signOutError : (Json.Encode.Value -> msg) -> Sub msg
port signOutSucceeded : (Json.Encode.Value -> msg) -> Sub msg

init : Url.Url -> Browser.Navigation.Key -> Model
init url key =
    { url = url
    , key = key
    , authCode = getAuthCode url
    , userInfo = ""
    , error = NoError
    , currentSubPage = Verified
    , email = ""
    , firstName = ""
    , lastName = ""
    , degree = None
    }

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ signInSucceeded SignInSucceeded
        , signInWithLinkError SignInFailed
        , signOutSucceeded SignOutSucceeded
        , signOutError SignOutFailed
        , gotUserInfo GotUserInfo
        ]

update : Msg  -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SignInSucceeded userInfo ->
            let info = Json.Encode.encode 0 userInfo
            in ({ model | userInfo = info, currentSubPage = MinSide }, getUserInfo (encode "getUserInfo" True))
        SignInFailed json ->
            let jsonStr = Json.Encode.encode 0 json
                error = errorFromString (getErrorCode jsonStr)
            in ({ model | error = error }, Cmd.none)
        RequestedUserInfo _ ->
            (model, getUserInfo (encode "requestedUserInfo" True))
        GotUserInfo json ->
            let email = decodeUserInfo json "email"
                firstName = decodeUserInfo json "firstName"
                lastName = decodeUserInfo json "lastName"
                degree = stringToDegree (decodeUserInfo json "degree")
            in
                ({ model | email = email, firstName = firstName, lastName = lastName, degree = degree }, Cmd.none) 
        UpdateUserInfo ->
            (model, updateUserInfo (encodeUserInfo model))
        AttemptSignOut ->
            (model, attemptSignOut (encode "requestedLogOut" True))
        SignOutSucceeded _ ->
            ({ model | currentSubPage = Verified }, Browser.Navigation.load "https://echobedriftstur-userauth.firebaseapp.com" )
        SignOutFailed error ->
            ({ model | currentSubPage = Verified }, Cmd.none)
        TypedFirstName str ->
            ({ model | firstName = str }, Cmd.none)
        TypedLastName str ->
            ({ model | lastName = str }, Cmd.none)
        TypedDegree str ->
            let degree = stringShorthandToDegree str
            in
                ({ model | degree = degree }, Cmd.none)

view : Model -> Html Msg
view model =
    showPage model

showPage : Model -> Html Msg
showPage model =
    case model.currentSubPage of
        Verified ->
            div [ class "verified" ]
                [ p [] 
                    [ text (handleQuery model) ]
                , p []
                    [ text model.userInfo ]
                ]
        MinSide ->
            div [ class "min-side" ]
                [ div [ id "min-side-content" ]
                    [ input [ class "min-side-item", id "email", type_ "text", value (model.email), disabled True ] [ text "Mail" ]
                    , input [ class "min-side-item", id "firstName", type_ "text", placeholder "Fornavn", Html.Events.onInput TypedFirstName, value (model.firstName) ] [ text "Fornavn" ]
                    , br [] []
                    , input [ class "min-side-item", id "lastName", type_ "text", placeholder "Etternavn", Html.Events.onInput TypedLastName, value (model.lastName) ] [ text "Etternavn" ]
                    , br [] []
                    , div [ class "min-side-item", id "degree" ]
                        [ select [ value (degreeToStringShorthand model.degree), Html.Events.onInput TypedDegree ]
                            [ option [ value "None" ] [ text "" ]
                            , option [ value "DTEK" ] [ text (degreeToString DTEK) ]
                            , option [ value "DVIT" ] [ text (degreeToString DVIT) ]
                            , option [ value "DSIK" ] [ text (degreeToString DSIK) ]
                            , option [ value "BINF" ] [ text (degreeToString BINF) ]
                            , option [ value "IMØ" ] [ text (degreeToString IMØ) ]
                            , option [ value "IKT" ] [ text (degreeToString IKT) ]
                            , option [ value "KOGNI" ] [ text (degreeToString KOGNI) ]
                            , option [ value "INF" ] [ text (degreeToString INF) ]
                            , option [ value "PROG" ] [ text (degreeToString PROG) ]
                            ]
                        ]
                    , button [ class "min-side-item", type_ "button", Html.Events.onClick UpdateUserInfo ] [ text "Lagre endringer" ]
                    , button [ class "min-side-item", type_ "button", Html.Events.onClick AttemptSignOut ] [ text "Logg ut" ]
                    , h3 [] [ text (model.email ++ ", " ++ model.firstName ++ ", " ++ model.lastName ++ ", " ++ (degreeToString model.degree)) ]
                    ]
                ]

handleQuery : Model -> String
handleQuery model =
    case model.error of
        NoError ->
            case (getAuthCode model.url) of
                AttemptedSignIn str ->
                    "Du vil bli videresendt straks..."
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
                        AttemptedSignIn query
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
            NoError

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
        NoError ->
            ""

encode : String -> Bool ->  Json.Encode.Value
encode string var =
    Json.Encode.object [ (string, Json.Encode.bool var) ]

encodeUserInfo : Model -> Json.Encode.Value
encodeUserInfo model =
    Json.Encode.object  [ ("firstName", Json.Encode.string model.firstName)
                        , ("lastName", Json.Encode.string model.lastName)
                        , ("degree", Json.Encode.string (degreeToString model.degree))
                        ]
                    

degreeToString : Degree -> String
degreeToString degree =
    case degree of
        DTEK ->
            "Datateknologi, 3. året"
        DVIT ->
            "Datavitenskap, 3. året"
        DSIK ->
            "Datasikkerhet, 3. året"
        BINF ->
            "Bioinformatikk, 3. året"
        IMØ ->
            "Informatikk-matematikk-økonomi, 3. året"
        IKT ->
            "Informasjons- og kommunikasjonsteknologi, 3. året"
        KOGNI ->
            "Kognitiv vitenskap med spesialisering i informatikk, 3. året"
        INF ->
            "Master i informatikk, 4. året"
        PROG ->
            "Master i programutvkling, 4. året"
        None ->
            ""

stringToDegree : String -> Degree
stringToDegree str =
    case str of
        "Datateknologi, 3. året" ->
            DTEK
        "Datavitenskap, 3. året" ->
            DVIT
        "Datasikkerhet, 3. året" ->
            DSIK
        "Bioinformatikk, 3. året" ->
            BINF
        "Informatikk-matematikk-økonomi, 3. året" ->
            IMØ
        "Informasjons- og kommunikasjonsteknologi, 3. året" ->
            IKT
        "Kognitiv vitenskap med spesialisering i informatikk, 3. året" ->
            KOGNI
        "Master i informatikk, 4. året" ->
            INF
        "Master i programutvkling, 4. året" ->
            PROG
        _ ->
            None

stringShorthandToDegree : String -> Degree
stringShorthandToDegree str =
    case str of
        "DTEK" ->
            DTEK
        "DVIT" ->
            DVIT
        "DSIK" ->
            DSIK
        "BINF" ->
            BINF
        "IMØ" ->
            IMØ
        "IKT" ->
            IKT
        "KOGNI" ->
            KOGNI
        "INF" ->
            INF
        "PROG" ->
            PROG
        _ ->
            None

degreeToStringShorthand : Degree -> String
degreeToStringShorthand degree =
    case degree  of
        DTEK ->
            "DTEK"
        DVIT ->
            "DVIT"
        DSIK ->
            "DSIK"
        BINF ->
            "BINF"
        IMØ ->
            "IMØ"
        IKT ->
            "IKT"
        KOGNI ->
            "KOGNI"
        INF ->
            "INF"
        PROG ->
            "PROG"
        None ->
            ""
redirectUrl : Url.Url -> String
redirectUrl url =
    Url.Builder.crossOrigin ("https://" ++ url.host) [ "minside" ] []

decodeUserInfo : Json.Encode.Value -> String -> String
decodeUserInfo json field =
    let jsonStr = Json.Encode.encode 0 json
    in
        case Json.Decode.decodeString (Json.Decode.at [ field ] Json.Decode.string) jsonStr of
            Ok info ->
                info 
            Err _ ->
                "error"
