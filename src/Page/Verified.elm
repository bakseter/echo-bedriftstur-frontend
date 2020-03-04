port module Page.Verified exposing (..)

import Html exposing (Html, div, span, br, text, p, input, button, select, option, h1, h2, h3, label)
import Html.Attributes exposing (class, id, type_, value, placeholder, disabled, style, for)
import Html.Events
import Json.Encode as Encode
import Json.Decode as Decode
import Url
import Url.Parser as Parser exposing ((<?>))
import Url.Parser.Query as Query
import Browser.Navigation
import Svg
import Svg.Attributes exposing (x, y, rx, ry)
import Svg.Events
import Time
import Countdown

release : Int
release =
    1585908000000

redirectToHome : String
redirectToHome =
    "https://echobedriftstur-userauth.firebaseapp.com"

port userStatusChanged : (Encode.Value -> msg) -> Sub msg
port signInSucceeded : (Encode.Value -> msg) -> Sub msg
-- Errors: ExpiredActionCode, InvalidEmail, UserDisabled
port signInError : (Encode.Value -> msg) -> Sub msg

port getUserInfo : Encode.Value -> Cmd msg
port getUserInfoSucceeded : (Encode.Value -> msg) -> Sub msg
-- Errors: PermissionDenied, Unauthenticated
port getUserInfoError : (Encode.Value -> msg) -> Sub msg

port updateUserInfo : Encode.Value -> Cmd msg
port updateUserInfoError : (Encode.Value -> msg) -> Sub msg
port updateUserInfoSucceeded : (Encode.Value -> msg) -> Sub msg

port createTicket : Encode.Value -> Cmd msg
port createTicketSucceeded : (Encode.Value -> msg) -> Sub msg
port createTicketError : (Encode.Value -> msg) -> Sub msg

port attemptSignOut : Encode.Value -> Cmd msg
port signOutSucceeded : (Encode.Value -> msg) -> Sub msg
port signOutError : (Encode.Value -> msg) -> Sub msg

type Msg
    = Tick Time.Posix
    | UserStatusChanged Decode.Value
    | SignInSucceeded Decode.Value
    | SignInError Decode.Value
    | GetUserInfoSucceeded Decode.Value
    | GetUserInfoError Decode.Value
    | UpdateUserInfo User Content
    | UpdateUserInfoSucceeded Decode.Value
    | UpdateUserInfoError Decode.Value
    | CreateTicket
    | CreateTicketSucceeded Decode.Value
    | CreateTicketError Decode.Value
    | AttemptSignOut
    | SignOutSucceeded Encode.Value
    | SignOutError Encode.Value
    | TypedFirstName String
    | TypedLastName String
    | TypedDegree String
    | CheckedBoxOne
    | CheckedBoxTwo
    | CheckedBoxThree

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
    , key : Browser.Navigation.Key
    , currentTime : Time.Posix
    , email : String
    , firstName : String
    , lastName : String
    , degree : Degree
    , checkedRules : List (Bool)
    , isSignedIn : Bool
    , currentSubPage : SubPage
    , user : User
    , submittedContent : Content
    }

type alias User =
    { uid : String
    , email : String
    }

init : Url.Url -> Browser.Navigation.Key -> Model
init url key =
    { url = url
    , key = key
    , currentTime = Time.millisToPosix 0
    , email = ""
    , firstName = ""
    , lastName = ""
    , degree = None
    , checkedRules = [False, False, False]
    , isSignedIn = False
    , currentSubPage = Verified
    , user = { uid = "", email = "" }
    , submittedContent = { email = "", firstName = "", lastName = "", degree = None }
    }

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 1000 Tick
        , userStatusChanged UserStatusChanged
        , signInSucceeded SignInSucceeded
        , signInError SignInError
        , createTicketSucceeded CreateTicketSucceeded
        , createTicketError CreateTicketError
        , getUserInfoSucceeded GetUserInfoSucceeded
        , getUserInfoError GetUserInfoError
        , updateUserInfoSucceeded UpdateUserInfoSucceeded
        , updateUserInfoError UpdateUserInfoError
        , signOutSucceeded SignOutSucceeded
        , signOutError SignOutError
        ]

update : Msg  -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Tick time ->
            ({ model | currentTime = time }, Cmd.none)
        UserStatusChanged json ->
            let user = decodeUser json
            in
                if user.uid == "" || user.email == "" then
                    ({ model | isSignedIn = False }, Cmd.none)
                else
                    ({ model | isSignedIn = True, user = user }, getUserInfo (encodeUser user))
        SignInSucceeded userJson ->
            ({ model | currentSubPage = MinSide }, getUserInfo (encodeUser (decodeUser userJson)))
        SignInError json ->
            (model, Cmd.none)
        GetUserInfoSucceeded json ->
            let content = decodeContent json
            in ({ model | email = content.email
                          , firstName = content.firstName
                          , lastName = content.lastName
                          , degree = content.degree 
                          , submittedContent = content
                          }, Cmd.none)
        GetUserInfoError json ->
            (model, Cmd.none)
        UpdateUserInfo user content ->
            let newContent = { email = model.email
                             , firstName = model.firstName
                             , lastName = model.lastName
                             , degree = model.degree
                             } 
                message = encodeUserInfo model.user newContent
            in (model, Cmd.batch [ updateUserInfo message, Browser.Navigation.load redirectToHome ])
        UpdateUserInfoSucceeded _ ->
            let subCont = model.submittedContent
                newSubCont = { subCont | firstName = model.firstName, lastName = model.lastName, degree = model.degree }
            in ({ model | submittedContent = newSubCont }, attemptSignOut (Encode.object [ ("requestedSignOut", Encode.bool True) ]))
        UpdateUserInfoError json ->
            (model, Cmd.none)
        CreateTicket ->
            (model, createTicket (encodeTicket model.user))
        CreateTicketSucceeded json ->
            (model, Cmd.none)
        CreateTicketError json ->
            (model, Cmd.none)
        AttemptSignOut ->
            (model, attemptSignOut (Encode.object [ ("requestedSignOut", Encode.bool True) ]))
        SignOutSucceeded _ ->
            ((init model.url model.key)
             , Browser.Navigation.load redirectToHome )
        SignOutError json ->
            (model, Cmd.none)
        TypedFirstName str ->
            ({ model | firstName = str }, Cmd.none)
        TypedLastName str ->
            ({ model | lastName = str }, Cmd.none)
        TypedDegree str ->
            ({ model | degree = (stringToDegree True str) }, Cmd.none)
        CheckedBoxOne ->
            case model.checkedRules of
                [ one, two, three ] ->
                    ({ model | checkedRules = [ (not one), two, three ] }, Cmd.none)
                _ ->
                    (model, Cmd.none)
        CheckedBoxTwo ->
            case model.checkedRules of
                [ one, two, three ] ->
                    ({ model | checkedRules = [ one, (not two), three ] }, Cmd.none)
                _ ->
                    (model, Cmd.none)
        CheckedBoxThree ->
            case model.checkedRules of
                [ one, two, three ] ->
                    ({ model | checkedRules = [ one, two, (not three) ] }, Cmd.none)
                _ ->
                    (model, Cmd.none)

-- ENCODERS

encodeFields : List (String, String) -> List (String, Encode.Value)
encodeFields list =
    case list of
        (x :: xs) ->
            [ (Tuple.first x, Tuple.second x |> Encode.string) ] ++ encodeFields xs
        [] ->
            []

encodeTicket : User -> Encode.Value
encodeTicket user =
    Encode.object <| encodeFields
        [ ("collection", "tickets")
        , ("uid", user.uid)
        , ("email", user.email)
        ]

encodeUser : User -> Encode.Value
encodeUser user =
    [ ("collection", "users")
    , ("uid", user.uid)
    , ("email", user.email)
    ]
        |> encodeFields
        |> Encode.object

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
-- if the object is formatted correctly.
-- Fails if not all the fields required for
-- a User record are present in the JSON.
userDecoder : Decode.Decoder User
userDecoder =
    Decode.map2 User
        (stringOrNullDecoder "uid")
        (stringOrNullDecoder "email")

-- Uses the contentDecoder function to turn
-- a JSON object into a Content record.
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
-- if the object is formatted correctly.
-- Fails if not all fields required for
-- a Content record are present in the JSON.
contentDecoder : Decode.Decoder Content
contentDecoder =
    Decode.map4 Content
        (stringOrNullDecoder "email")
        (stringOrNullDecoder "firstName")
        (stringOrNullDecoder "lastName")
        (Decode.oneOf (degreeOrNullDecoder "degree"))


-- Decoder that either decodes the string at the given field,
-- turning the string into a degree in the process,
-- or returns None if the degree is not valid or if the field is null.
degreeOrNullDecoder : String -> List (Decode.Decoder Degree)
degreeOrNullDecoder field =
        [ Decode.map (stringToDegree False) (Decode.at [ field ] Decode.string)
        , Decode.at [ field ] (Decode.null None)
        ]

-- Decoder that either decodes the string at the given field,
-- or returns en empty string if the field is null.
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
    let (countdown, isRelease) = Countdown.countdownFromTo (Time.posixToMillis model.currentTime) release
    in
        case model.currentSubPage of
            Verified ->
                div [ class "verified" ]
                    [ p [] 
                        [ if isLinkValid model.url then
                            text "Du har nå blitt logget inn. Vennligst vent mens du blir videresendt..."
                         else
                            text "Innlogginslinken er ikke gyldig"
                        ]
                    ]
            MinSide ->
                div [ class "min-side" ]
                    [ div [ id "min-side-content" ]
                        [ h1 [ class "min-side-item", id "min-side-header"] [ text "Registrering" ]
                        , div [ class "min-side-item text" ]
                            [ div [] [ text "Her kan du registrere deg i forkant av påmeldingen." ]
                            , div [] [ text "Påmeldingen vil dukke opp her 3. april kl. 12:00, gitt at du er logget inn og har registrert deg." ]
                            , div [ style "font-weight" "bold"] [ text "Det er ikke nødvendig å refreshe siden for å få påmeldingen til å vises" ]
                            ]
                        , input [ class "min-side-item"
                                , id "email", type_ "text"
                                , disabled True
                                , value (model.email) 
                                ] []
                        , input [ class "min-side-item"
                                , id "firstName"
                                , type_ "text"
                                , placeholder "Fornavn"
                                , value (model.firstName) 
                                , Html.Events.onInput TypedFirstName
                                ] []
                        , br [] []
                        , input [ class "min-side-item"
                                , id "lastName"
                                , type_ "text"
                                , placeholder "Etternavn"
                                , value (model.lastName)
                                , Html.Events.onInput TypedLastName
                                ] []
                        , br [] []
                        , select [ class "min-side-item", id "degree", value (degreeToString True model.degree), Html.Events.onInput TypedDegree ]
                            [ option [ value "" ] [ text (degreeToString False None) ]
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
                        , div [ class "min-side-item checkbox-grid" ]
                            [ Svg.svg [ Svg.Attributes.class (getCheckboxClass 1 model), Svg.Attributes.width "40", Svg.Attributes.height "40", Svg.Events.onClick CheckedBoxOne ]
                                [ Svg.rect [ x "0", y "0", Svg.Attributes.width "40", Svg.Attributes.height "40" ] [] ]
                            , div [ class "text" ]
                                [ text "Jeg bekrefter at ..." ]
                            ]
                        , div [ class "min-side-item checkbox-grid" ]
                            [ Svg.svg [ Svg.Attributes.class (getCheckboxClass 2 model), Svg.Attributes.width "40", Svg.Attributes.height "40", Svg.Events.onClick CheckedBoxTwo ]
                                [ Svg.rect [ x "0", y "0", Svg.Attributes.width "40", Svg.Attributes.height "40" ] [] ]
                            , div [ class "text" ]
                                [ text "Jeg bekrefter også at ..." ]
                            ]
                        , div [ class "min-side-item checkbox-grid" ]
                            [ Svg.svg [ Svg.Attributes.class (getCheckboxClass 3 model), Svg.Attributes.width "40", Svg.Attributes.height "40", Svg.Events.onClick CheckedBoxThree ]
                                [ Svg.rect [ x "0", y "0", Svg.Attributes.width "40", Svg.Attributes.height "40" ] [] ]
                            , div [ class "text" ]
                                [ text "Jeg bekrefter i tillegg at ..." ]
                            ]
                        , div [ class "min-side-item min-side-buttons" ]
                            [ input
                                [ id "save-btn"
                                , type_ "button"
                                , value "Lagre endringer og logg ut"
                                , Html.Events.onClick
                                    (UpdateUserInfo model.user
                                        { email = model.email, firstName = model.firstName, lastName = model.lastName, degree = model.degree }
                                    )
                                , disabled (not (hasChangedInfo model && hasCheckedAllRules model && infoIsNotEmpty model && model.isSignedIn))
                                ] []
                            , input [ id "signout-btn", type_ "button", value "Logg ut", Html.Events.onClick AttemptSignOut ] []
                            ]
                        , if isRelease then
                            input [ class "min-side-item", type_ "button", value "Meld meg på!", Html.Events.onClick CreateTicket ] []
                         else
                             div [ class "min-side-item", id "countdown" ]
                                countdown
                        ]
                    ]

getCheckboxClass : Int -> Model -> String
getCheckboxClass id model =
    case model.checkedRules of
        [ one, two, three ] ->
            case id of
                1 ->
                    if one then
                        "checked-box"
                    else
                        "unchecked-box"
                2 ->
                    if two then
                        "checked-box"
                    else
                        "unchecked-box"
                3 ->
                    if three then
                        "checked-box"
                    else
                        "unchecked-box"
                _ ->
                    ""
        _ ->
            ""

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
hasChangedInfo : Model -> Bool
hasChangedInfo model =
    let con = model.submittedContent
    in
        con.firstName /= model.firstName ||
        con.lastName /= model.lastName ||
        con.degree /= model.degree

infoIsNotEmpty : Model -> Bool
infoIsNotEmpty model =
    model.firstName /= "" &&
    model.lastName /= "" &&
    model.degree /= None

hasCheckedAllRules : Model -> Bool
hasCheckedAllRules model =
    List.all (\x -> x == True) model.checkedRules

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
