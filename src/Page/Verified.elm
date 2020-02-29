port module Page.Verified exposing (init, subscriptions, update, view, Model, Msg, verified)

import Html exposing (Html, div, span, br, text, p, input, button, select, option, h3, h2)
import Html.Attributes exposing (class, id, type_, value, placeholder, disabled, style)
import Html.Events
import Json.Encode as Encode
import Json.Decode as Decode
import Url
import Url.Parser as Parser exposing ((<?>))
import Url.Parser.Query as Query

-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-- !!!!!!CHANGE IN PRODUCTION!!!!!!
-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
redirectToHome : String
redirectToHome =
    "https://echobedriftstur-userauth.firebaseapp.com"

port userStatusChanged : (Encode.Value -> msg) -> Sub msg
port signInSucceeded : (Encode.Value -> msg) -> Sub msg
-- Errors: ExpiredActionCode, InvalidEmail, UserDisabled
port signInError : (Encode.Value -> msg) -> Sub msg

port getUserInfo : Encode.Value -> Cmd msg
-- Errors: PermissionDenied, Unauthenticated
port getUserInfoError : (Encode.Value -> msg) -> Sub msg
port getUserInfoSucceeded : (Encode.Value -> msg) -> Sub msg

port updateUserInfo : Encode.Value -> Cmd msg
port updateUserInfoError : (Encode.Value -> msg) -> Sub msg
port updateUserInfoSucceeded : (Encode.Value -> msg) -> Sub msg


-- JS needs: collection, uid, content as JSON
-- content (JSON): email, firstName, lastName, degree

type Msg
    = UserStatusChanged Decode.Value
    | SignInSucceeded Decode.Value
    | SignInError Decode.Value
    | GetUserInfo User
    | GetUserInfoSucceeded Decode.Value
    | GetUserInfoError Decode.Value
    | UpdateUserInfo User Content
    | UpdateUserInfoError Decode.Value
    | UpdateUserInfoSucceeded Decode.Value
    | TypedFirstName String
    | TypedLastName String
    | TypedDegree String

type Degree
    = DTEK
    | DSIK
    | DVIT
    | BINF
    | IMØ
    | IKT
    | KOGNI
    | INF
    | PROG
    | None

type SubPage
    = Verified
    | MinSide

type alias Content =
    { email : String
    , firstName : String
    , lastName : String
    , degree : Degree
    }

type alias Model =
    { url : Url.Url
    , email : String
    , firstName : String
    , lastName : String
    , degree : Degree
    , isSignedIn : Bool
    , currentSubPage : SubPage
    }

type alias User =
    { uid : String
    , email : String
    }

init : Url.Url -> Model
init url =
    { url = url
    , email = ""
    , firstName = ""
    , lastName = ""
    , degree = None
    , isSignedIn = False
    , currentSubPage = Verified
    }

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ userStatusChanged UserStatusChanged
        , signInSucceeded SignInSucceeded
        , signInError SignInError
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
            in
                if user.uid == "niet" && user.email == "niet" then
                    ({ model | isSignedIn = False }, Cmd.none)
                else
                    ({ model | isSignedIn = True }, Cmd.none)
        SignInSucceeded userJson ->
            ({ model | currentSubPage = MinSide }, Cmd.none)
        SignInError json ->
            (model, Cmd.none)
        GetUserInfo user ->
            (model, Encode.object [ ("collection", Encode.string "users"),
                                    ("uid", Encode.string user.uid) ] |> getUserInfo) 
        GetUserInfoError json ->
            (model, Cmd.none)
        GetUserInfoSucceeded json ->
            let content = decodeContent json
            in ({ model | email = content.email, firstName = content.firstName, lastName = content.lastName, degree = content.degree }, Cmd.none)
        UpdateUserInfo user content ->
            (model, Cmd.none)
        UpdateUserInfoError json ->
            (model, Cmd.none)
        UpdateUserInfoSucceeded json ->
            (model, Cmd.none)
        TypedFirstName str ->
            (model, Cmd.none)
        TypedLastName str ->
            (model, Cmd.none)
        TypedDegree str ->
            (model, Cmd.none)

decodeUser : Encode.Value -> User
decodeUser json =
    let jsonStr = Encode.encode 0 json
    in 
        case Decode.decodeString userDecoder jsonStr of
            Ok val ->
                val
            Err err ->
                { uid = Decode.errorToString err
                , email = ""
                }

userDecoder : Decode.Decoder User
userDecoder =
    Decode.map2 User 
        (Decode.oneOf [ (Decode.at [ "uid" ] Decode.string), Decode.null "niet" ])
        (Decode.oneOf [ (Decode.at [ "email" ] Decode.string), Decode.null "niet" ])


decodeContent : Encode.Value -> Content
decodeContent json =
    let jsonStr = Encode.encode 0 json
    in
        case Decode.decodeString contentDecoder jsonStr of
            Ok val ->
                { email = val.email
                , firstName = val.firstName
                , lastName = val.lastName
                , degree = val.degree
                }
            Err err ->
                { email = Decode.errorToString err
                , firstName = ""
                , lastName = ""
                , degree = None
                }


contentDecoder : Decode.Decoder Content
contentDecoder =
    Decode.map4 Content
        (decodeStringOrNull "email")
        (decodeStringOrNull "firstName")
        (decodeStringOrNull "lastName")
        (Decode.oneOf (decodeDegreeOrNull "degree"))

decodeDegreeOrNull : String -> List (Decode.Decoder Degree)
decodeDegreeOrNull field =
        [ Decode.map stringToDegree (Decode.at [ field ] Decode.string)
        , Decode.at [ field ] (Decode.null None)
        ]

decodeStringOrNull field =
    [ (Decode.at [ field ] Decode.string)
    , (Decode.at [ field ] (Decode.null ""))
    ] |> Decode.oneOf

view : Model -> Html Msg
view model =
    div []
        [ showPage model ]

showPage : Model -> Html Msg
showPage model =
    case model.currentSubPage of
        Verified ->
            div [ class "verified" ]
                [ p [] 
                    [ if not <| isLinkValid model.url then
                        text "Innlogginslinken er ikke gyldig"
                     else
                        text "Du har nå blitt logget inn. Vennligst vent mens du blir videresendt..."
                    ]
                ]
        MinSide ->
            div [ class "min-side" ]
                [ div [ id "min-side-content" ]
                    [ input [ class "min-side-item", id "email", type_ "text", disabled True ] [ text "Mail" ]
                    , input [ class "min-side-item", id "firstName", type_ "text", placeholder "Fornavn", Html.Events.onInput TypedFirstName ] [ text "Fornavn" ]
                    , br [] []
                    , input [ class "min-side-item", id "lastName", type_ "text", placeholder "Etternavn", Html.Events.onInput TypedLastName ] [ text "Etternavn" ]
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
                        [ input [ id "save-btn", value "Lagre endringer og logg ut", disabled <| not <| hasChangedInfo model, type_ "button", Html.Events.onClick (UpdateUserInfo { uid = "", email = "" } { email = "", firstName = "", lastName = "", degree = None }) ] []
                        ]
                    ]
                ]

verified : SubPage
verified =
    Verified


isLinkValid : Url.Url -> Bool
isLinkValid url =
    let decoder = Parser.s "verified" <?> Query.string "apiKey"
    in
        case Parser.parse decoder url of
            Just code ->
                Maybe.withDefault False (Just True)
            Nothing ->
                False

hasChangedInfo model =
    True

errorMessageToUser model =
    "asd"

degreeToString degree =
    "asd"

degreeToStringShorthand degree =
    "asd"

handleQuery model =
    "asd"

stringToDegree str =
    None
