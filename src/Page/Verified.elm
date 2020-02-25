port module Page.Verified exposing (init, subscriptions, update, view, Model, Msg, encode, verified, redirectToHome)

import Html exposing (Html, div, span, br, text, p, input, button, select, option, h3, h2)
import Html.Attributes exposing (class, id, type_, value, placeholder, disabled)
import Html.Events
import Url
import Url.Builder
import Url.Parser exposing ((<?>))
import Url.Parser.Query as Query
import Json.Encode
import Json.Decode
import Browser.Navigation

redirectToHome : String
redirectToHome =
    "https://echobedriftstur.no"

type Msg
    = UserStatusChanged Json.Encode.Value
    | SignInSucceeded Json.Encode.Value
    | SignInFailed Json.Encode.Value
    | RequestedUserInfo Json.Encode.Value
    | GetUserInfoError Json.Encode.Value
    | GetUserInfoSucceeded Json.Encode.Value
    | UpdateUserInfoSucceeded Json.Encode.Value
    | UpdateUserInfoError Json.Encode.Value
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
    | PermissionDenied
    | Unauthenticated

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

port userStatusChanged : (Json.Encode.Value -> msg) -> Sub msg
port signInSucceeded : (Json.Encode.Value -> msg) -> Sub msg
-- Errors: ExpiredActionCode, InvalidEmail, UserDisabled
port signInWithLinkError : (Json.Encode.Value -> msg) -> Sub msg

port getUserInfo : Json.Encode.Value -> Cmd msg
port userNotSignedIn : (Json.Encode.Value -> msg) -> Sub msg
-- Errors: PermissionDenied, Unauthenticated
port getUserInfoError : (Json.Encode.Value -> msg) -> Sub msg
port getUserInfoSucceeded : (Json.Encode.Value -> msg) -> Sub msg

port updateUserInfo : Json.Encode.Value -> Cmd msg
port updateUserInfoError : (Json.Encode.Value -> msg) -> Sub msg
port updateUserInfoSucceeded : (Json.Encode.Value -> msg) -> Sub msg

type alias Model =
    { url : Url.Url
    , key : Browser.Navigation.Key
    , authCode : AuthCode
    , error : Error
    , currentSubPage : SubPage
    , email : String
    , firstName : String
    , lastName : String
    , degree : Degree
    , submittedUserInfo : SubmittedUserInfo
    , user : User
    }

type alias SubmittedUserInfo =
    { firstName : String
    , lastName : String
    , degree : Degree
    }

type alias User =
    { uid : String
    , isSignedIn : Bool
    }

init : Url.Url -> Browser.Navigation.Key -> Model
init url key =
    { url = url
    , key = key
    , authCode = getAuthCode url
    , error = NoError
    , currentSubPage = Verified
    , email = ""
    , firstName = ""
    , lastName = ""
    , degree = None
    , submittedUserInfo =
        { firstName = ""
        , lastName = ""
        , degree = None
        }
    , user =
        { uid = ""
        , isSignedIn = False
        }
    }

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ userStatusChanged UserStatusChanged
        , signInSucceeded SignInSucceeded
        , signInWithLinkError SignInFailed
        , getUserInfoError GetUserInfoError
        , getUserInfoSucceeded GetUserInfoSucceeded
        , updateUserInfoSucceeded UpdateUserInfoSucceeded
        , updateUserInfoError UpdateUserInfoError
        ]

update : Msg  -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        UserStatusChanged json ->
            let user = decodeUser json
                newModel = { model | user = user }
            in
                if not <| user.isSignedIn then
                    ({ newModel | currentSubPage = Verified }, Browser.Navigation.pushUrl model.key redirectToHome)
                else
                    (newModel, Cmd.none)
        SignInSucceeded _ ->
            ({ model | currentSubPage = MinSide }, getUserInfo (encode "getUserInfo" True))
        SignInFailed json ->
            let error = getErrorCode (Json.Encode.encode 0 json)
            in ({ model | error = error }, Cmd.none)
        RequestedUserInfo _ ->
            (model, getUserInfo (encode "requestedUserInfo" True))
        GetUserInfoError json ->
            let error = getErrorCode (Json.Encode.encode 0 json)
            in ({ model | error = error }, Cmd.none)
        GetUserInfoSucceeded json ->
            let email = decodeJsonField json "email"
                firstName = decodeJsonField json "firstName"
                lastName = decodeJsonField json "lastName"
                degree = stringToDegree (decodeJsonField json "degree")
            in
                ({ model | email = email
                           , firstName = firstName
                           , lastName = lastName
                           , degree = degree
                           , submittedUserInfo =
                                { firstName = firstName
                                , lastName = lastName
                                , degree = degree
                                }
                }, Cmd.none) 
        UpdateUserInfo ->
            if hasChangedInfo model then
                ({ model | currentSubPage = Verified }
                , Cmd.batch
                    [ updateUserInfo (encodeUserInfo model)
                    , Browser.Navigation.pushUrl model.key redirectToHome
                    ]
                )
            else
                (model, Cmd.none)
        UpdateUserInfoError json ->
            let error = getErrorCode (Json.Encode.encode 0 json)
            in ({ model | error = error }, Cmd.none)
        UpdateUserInfoSucceeded _ ->
            let oldUserInfo = model.submittedUserInfo
            in ({ model | submittedUserInfo = { oldUserInfo | firstName = model.firstName, lastName = model.lastName, degree = model.degree } }, Cmd.none)
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
                    , div [ class "min-side-item", id "min-side-buttons" ]
                        [ input [ id "save-btn", value "Lagre endringer og logg ut", disabled <| not <| hasChangedInfo model, type_ "button", Html.Events.onClick UpdateUserInfo ] []
                        ]
                    , p [ class "min-side-item", id "error-message" ] [ text (errorMessageToUser model.error) ]
                    , p [ class "min-side-teim", id "model-debug" ]
                        [ text (model.email ++ model.firstName ++ model.lastName) ]
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

getErrorCode : String -> Error
getErrorCode json =
    case Json.Decode.decodeString Json.Decode.string json of
        Ok code ->
            errorFromString code
        Err _ ->
            NoError

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
        "permission-denied" ->
            PermissionDenied
        "unathenticated" ->
            Unauthenticated
        _ ->
            NoError

errorMessageToUser : Error -> String
errorMessageToUser error =
    case error of
        InvalidEmail ->
            "Mailen du har skrevet inn har ikke riktig format. Prøv igjen."
        ExpiredActionCode ->
            "Innlogginslinken har utløpt. Prøv å sende en ny link."
        InvalidActionCode ->
            "Innlogginslinken er ikke gyldig. Dette kan skje om den allerede har blitt brukt."
        UserDisabled ->
            "Brukeren din har blitt deaktivert."
        PermissionDenied ->
            """
            Det skjedde en feil når vi prøvde å hente/oppdatere brukerinformasjonen din. 
            Dette kan skje om du ikke har registrert deg med en gyldig studentmail.
            Vennligst logg ut og logg inn med en gyldig studentmail.
            """
        Unauthenticated ->
            "Du har ikke tilgang til denne siden. Prøv å logg inn på nytt."
        NoError ->
            ""

encode : String -> Bool ->  Json.Encode.Value
encode string var =
    Json.Encode.object [ (string, Json.Encode.bool var) ]

encodeUserInfo : Model -> Json.Encode.Value
encodeUserInfo model =
    Json.Encode.object 
        [ ("firstName", Json.Encode.string model.firstName)
        , ("lastName", Json.Encode.string model.lastName)
        , ("degree", Json.Encode.string (degreeToString model.degree))
        ]

decodeJsonField : Json.Encode.Value -> String -> String
decodeJsonField json field =
    let jsonStr = Json.Encode.encode 0 json
    in
        case Json.Decode.decodeString (Json.Decode.at [ field ] Json.Decode.string) jsonStr of
            Ok info ->
                info 
            Err _ ->
                ""

hasChangedInfo : Model -> Bool
hasChangedInfo model =
    List.foldl (==) True 
        [ model.firstName /= model.submittedUserInfo.firstName
        , model.lastName /= model.submittedUserInfo.lastName
        , model.degree /= model.submittedUserInfo.degree
        ]

decodeUser : Json.Encode.Value -> User
decodeUser json =
    let jsonStr = Json.Encode.encode 0 json
        uid = Json.Decode.decodeString (Json.Decode.at [ "uid" ] Json.Decode.string) jsonStr
    in
        case uid of
            Ok value ->
                { uid = value
                , isSignedIn = True
                }
            Err _ ->
                { uid = ""
                , isSignedIn = False
                }

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

verified : SubPage
verified =
    Verified
