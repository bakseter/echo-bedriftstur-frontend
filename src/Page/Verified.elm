port module Page.Verified exposing (..)

import Html exposing (Html, div, span, br, text, p, input, button, select, option, h3, h2)
import Html.Attributes exposing (class, id, type_, value, placeholder, disabled, style)
import Html.Events
import Json.Encode as Encode
import Json.Decode as Decode
import Url
import Url.Parser as Parser exposing ((<?>))
import Url.Parser.Query as Query
import Result

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
    = Valid Degrees
    | None

type Degrees
    = DTEK
    | DSIK
    | DVIT
    | BINF
    | IMØ
    | IKT
    | KOGNI
    | INF
    | PROG

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
    , user : User
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
    , user = { uid = "", email = "" }
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
                    ({ model | isSignedIn = True, user = user }, Cmd.none)
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
            in ({ model | email = content.email
                          , firstName = content.firstName
                          , lastName = content.lastName
                          , degree = content.degree 
                          }, Cmd.none)
        UpdateUserInfo user content ->
            let newContent = { email = model.email
                             , firstName = model.firstName
                             , lastName = model.lastName
                             , degree = model.degree
                             } 
                message = encodeUserInfo model.user newContent
            in (model, updateUserInfo message)
        UpdateUserInfoError json ->
            (model, Cmd.none)
        UpdateUserInfoSucceeded json ->
            (model, Cmd.none)
        TypedFirstName str ->
            ({ model | firstName = str }, Cmd.none)
        TypedLastName str ->
            ({ model | lastName = str }, Cmd.none)
        TypedDegree str ->
            ({ model | degree = (stringToDegree True str) }, Cmd.none)


-- ENCODERS

encodeFields : List (String, String) -> List (String, Encode.Value)
encodeFields list =
    case list of
        (x :: xs) ->
            [ (Tuple.first x, Tuple.second x |> Encode.string) ] ++ encodeFields xs
        [] ->
            []

encodeUserInfo : User -> Content -> Encode.Value
encodeUserInfo user content =
    [ ("collection", "users")
    , ("uid", user.uid)
    , ("email", content.email)
    , ("firstName", content.firstName)
    , ("lastName", content.lastName)
    , ("degree", (degreeToString False content.degree))  
    ]
        |> encodeFields
        |> Encode.object
                      

-- DECODERS


-- Uses the userDecoder function to turn
-- a JSON object into a User record.
-- TODO: Fails if?
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

-- Decoder that turns a JSON object into a User record,
-- if the object is formatted correctly
userDecoder : Decode.Decoder User
userDecoder =
    Decode.map2 User 
        (Decode.oneOf [ (Decode.at [ "uid" ] Decode.string), Decode.null "niet" ])
        (Decode.oneOf [ (Decode.at [ "email" ] Decode.string), Decode.null "niet" ])


-- Uses the contentDecoder function to turn
-- a JSON object into a Content record
-- TODO: fails if?
decodeContent : Encode.Value -> Content
decodeContent json =
    let jsonStr = Encode.encode 0 json
    in
        case Decode.decodeString contentDecoder jsonStr of
            Ok val ->
                val
            Err err ->
                { email = Decode.errorToString err
                , firstName = ""
                , lastName = ""
                , degree = None
                }

-- Decoder that turns a JSON object into a Content record,
-- if the object is formatted correctly
contentDecoder : Decode.Decoder Content
contentDecoder =
    Decode.map4 Content
        (stringOrNullDecoder "email")
        (stringOrNullDecoder "firstName")
        (stringOrNullDecoder "lastName")
        (Decode.oneOf (degreeOrNullDecoder "degree"))


-- Decoder that either decodes the string at the given field,
-- turning the string into a degree in the process,
-- or returns None if the degree is not valid or if the field is null
degreeOrNullDecoder : String -> List (Decode.Decoder Degree)
degreeOrNullDecoder field =
        [ Decode.map (stringToDegree False) (Decode.at [ field ] Decode.string)
        , Decode.at [ field ] (Decode.null None)
        ]

-- Decoder that either decodes the string at the given field,
-- or returns en empty string if the field is null
stringOrNullDecoder : String -> Decode.Decoder String
stringOrNullDecoder field =
    [ (Decode.at [ field ] Decode.string)
    , (Decode.at [ field ] (Decode.null ""))
    ] |> Decode.oneOf


view : Model -> Html Msg
view model =
    div []
        [ showPage model ]

-- Shows a subpage
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
                        [ select [ value (degreeToString True model.degree), Html.Events.onInput TypedDegree ]
                            [ option [ value "None" ] [ text (degreeToString False None) ]
                            , option [ value "DTEK" ] [ text (degreeToString False (Valid DTEK)) ]
                            , option [ value "DVIT" ] [ text (degreeToString False (Valid DVIT)) ]
                            , option [ value "DSIK" ] [ text (degreeToString False (Valid DSIK)) ]
                            , option [ value "BINF" ] [ text (degreeToString False (Valid BINF)) ]
                            , option [ value "IMØ" ] [ text (degreeToString False (Valid IMØ)) ]
                            , option [ value "IKT" ] [ text (degreeToString False (Valid IKT)) ]
                            , option [ value "KOGNI" ] [ text (degreeToString False (Valid KOGNI)) ]
                            , option [ value "INF" ] [ text (degreeToString False (Valid INF)) ]
                            , option [ value "PROG" ] [ text (degreeToString False (Valid PROG)) ]
                            ]
                        ]
                    , div [ class "min-side-item", id "min-side-buttons" ]
                        [ input [ id "save-btn", value "Lagre endringer og logg ut", disabled <| not <| hasChangedInfo model, type_ "button", Html.Events.onClick (UpdateUserInfo { uid = "", email = "" } { email = "", firstName = "", lastName = "", degree = None }) ] []
                        ]
                    , div [ class "debug" ] [ text (model.email ++
                                                    model.firstName ++
                                                    model.lastName ++
                                                    (degreeToString True model.degree) ++
                                                    model.user.uid ++
                                                    model.user.email) ]
                    ]
                ]

verified : SubPage
verified =
    Verified

-- Checks if the sign in link is valid
isLinkValid : Url.Url -> Bool
isLinkValid url =
    let decoder = Parser.s "verified" <?> Query.string "apiKey"
    in
        case Parser.parse decoder url of
            Just code ->
                Maybe.withDefault False (Just True)
            Nothing ->
                False

-- Checks if the user has changed their info in the form
hasChangedInfo model =
    True


-- DEGREE PARSERS

-- List of valid degrees with their shorthand and long strings
degreesList : List (Degrees, (String, String))
degreesList =
    [ (DTEK, ("DTEK", "Datateknologi"))
    , (DVIT, ("DVIT", "Datatvitenskap"))
    , (DSIK, ("DSIK", "Datasikkerhet"))
    , (BINF, ("BINF", "Bioinformatikk"))
    , (IMØ, ("IMØ", "Informatikk-matematikk-økonomi"))
    , (IKT, ("IKT", "Informasjons- og kommunikasjonsteknologi"))
    , (KOGNI, ("KOGNI", "Kognitiv vitenskap med spesialisering i informatikk"))
    , (INF, ("INF", "Master i informatikk"))
    , (PROG, ("PROG", "Felles master i programutvkling"))
    ]

-- Convert degree to either shorthand or long string
degreeToString : Bool -> Degree -> String
degreeToString shorthand degree =
    case degree of
        Valid d ->
            case List.filter (\(x,(y,z)) -> x == d) degreesList of
                [ (deg, (short, long)) ] ->
                    if shorthand then
                        short
                    else
                        long
                _ ->
                    ""
        None ->
            ""

-- Convert either shorthand or long string to degree
stringToDegree : Bool -> String -> Degree
stringToDegree shorthand str =
    let filter arg = if shorthand then
                        (\(x,(y,z)) -> y == str)
                     else
                       (\(x,(y,z)) -> z == str)
    in
        case List.filter (filter str) degreesList of
            [ (deg, (short, long)) ] ->
                Valid deg
            _ ->
                None
