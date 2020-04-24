port module Page.Verified exposing (init, subscriptions, view, update, Model, Msg, route)

import Html exposing (Html, div, span, br, text, p, input, select, option, h1, h2, h3, a)
import Html.Attributes exposing (class, id, type_, value, placeholder, disabled, style, autocomplete, href)
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
import Debug

import Countdown
import User exposing (User)
import Uid exposing (Uid(..))
import Email exposing (Email(..))
import Degree exposing (Degree(..), Degrees(..))
import Terms exposing (..)
import Content exposing (Content)
import Session exposing (Session)
import Ticket exposing (Ticket(..), toBool)
import Error exposing (Error(..))

påmeldingUte : Int
påmeldingUte =
    1599994800000

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
    | GetUserInfoSucceeded Decode.Value
    | UpdateUserInfo Session Content
    | UpdateUserInfoSucceeded Decode.Value
    | CreateTicket
    | CreateTicketSucceeded Decode.Value
    | AttemptSignOut
    | SignOutSucceeded Encode.Value
    | TypedFirstName String
    | TypedLastName String
    | TypedDegree String
    | CheckedBoxOne
    | CheckedBoxTwo
    | CheckedBoxThree
    | GotError Encode.Value

type SubPage
    = Verified
    | MinSide

type CheckboxId
    = FirstBox
    | SecondBox
    | ThirdBox

type UpdateUserInfoStage
    = Idle
    | Updating
    | Succeeded

type alias Model =
    { url : Url.Url
    , key : Browser.Navigation.Key
    , user : User
    , currentTime : Time.Posix
    , inputContent : Content
    , submittedContent : Content
    , checkedRules : (Bool, Bool, Bool)
    , updateStage : UpdateUserInfoStage
    , currentSubPage : SubPage
    , session : Session
    , error : Error
    }

route : String
route =
    "verified"

init : Url.Url -> Browser.Navigation.Key -> Model
init url key =
    { url = url
    , key = key
    , user = User.empty
    , currentTime = Time.millisToPosix 0
    , inputContent = Content.empty
    , submittedContent = Content.empty
    , checkedRules = (False, False, False)
    , updateStage = Idle
    , currentSubPage = MinSide
    , session = Session.empty
    , error = NoError
    }

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 100 Tick
        , userStatusChanged UserStatusChanged
        , signInSucceeded SignInSucceeded
        , signInError GotError
        , createTicketSucceeded CreateTicketSucceeded
        , createTicketError GotError
        , getUserInfoSucceeded GetUserInfoSucceeded
        , getUserInfoError GotError
        , updateUserInfoSucceeded UpdateUserInfoSucceeded
        , updateUserInfoError GotError
        , signOutSucceeded SignOutSucceeded
        , signOutError GotError
        ]

update : Msg  -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Tick time ->
            ({ model | currentTime = time }, Cmd.none)
        UserStatusChanged json ->
            case Session.decode json of
                Just session ->
                    if Session.isSignedIn session then
                        ({ model | session = session, currentSubPage = MinSide, error = NoError }, getUserInfo (Session.encode session))
                    else
                        (model, Cmd.none)
                Nothing ->
                    let maybeNull = Decode.decodeString (Decode.null "not-signed-in") (Encode.encode 0 json)
                    in
                        case maybeNull of
                            Ok err ->
                                update (GotError (Error.encode err)) model
                            Err _ ->
                                update (GotError (Error.encode "json-parse-error")) model
        SignInSucceeded userJson ->
            ({ model | currentSubPage = MinSide }, Browser.Navigation.pushUrl model.key (redirectToHome ++ "/" ++ route))
        GetUserInfoSucceeded json ->
            case User.decode json of
                Just content ->
                    let submittedContent = Content content.firstName content.lastName content.degree content.terms
                        newCheckedRules = if Terms.toBool content.terms then (True, True, True) else model.checkedRules
                    in ({ model | user = User content.email content.firstName content.lastName content.degree content.terms content.hasTicket
                        , submittedContent = submittedContent
                        , inputContent = submittedContent 
                        , checkedRules = newCheckedRules }
                        , Cmd.none)
                Nothing ->
                    update (GotError (Error.encode "json-parse-error")) model
        UpdateUserInfo session content ->
            let newContent = Content model.inputContent.firstName model.inputContent.lastName model.inputContent.degree (Terms (hasCheckedAllRules model))
                message = Content.encode session newContent
            in ({ model | updateStage = Updating }, updateUserInfo message)
        UpdateUserInfoSucceeded _ ->
            let subCont = model.submittedContent
                newSubCont = Content.updateAll model.user.firstName model.user.lastName model.user.degree (Terms (hasCheckedAllRules model)) subCont
            in ({ model | submittedContent = newSubCont, updateStage = Succeeded }, Cmd.none)
        CreateTicket ->
            (model, createTicket (Ticket.encode model.session))
        CreateTicketSucceeded json ->
            (model, Cmd.none)
        AttemptSignOut ->
            (model, attemptSignOut (Encode.object [ ("requestedSignOut", Encode.bool True) ]))
        SignOutSucceeded _ ->
            ((init model.url model.key)
             , Browser.Navigation.load redirectToHome )
        TypedFirstName str ->
            let input = Content.updateFirstName str model.inputContent
            in ({ model | inputContent = input }, Cmd.none)
        TypedLastName str ->
            let input = Content.updateLastName str model.inputContent
            in ({ model | inputContent = input }, Cmd.none)
        TypedDegree str ->
            let input = Content.updateDegree (Degree.fromString True str) model.inputContent
            in ({ model | inputContent = input }, Cmd.none)
        CheckedBoxOne ->
            case model.checkedRules of
                (one, two, three) ->
                    ({ model | checkedRules = ((not one), two, three) }, Cmd.none)
        CheckedBoxTwo ->
            case model.checkedRules of
                (one, two, three) ->
                    ({ model | checkedRules = (one, (not two), three) }, Cmd.none)
        CheckedBoxThree ->
            case model.checkedRules of
                (one, two, three)->
                    ({ model | checkedRules = (one, two, (not three)) }, Cmd.none)
        GotError json ->
            ({ model | error = (Error.fromJson json) }, Cmd.none)

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
    let inp = model.inputContent
        sub = model.submittedContent
    in
        inp.firstName /= sub.firstName ||
        inp.lastName /= sub.lastName ||
        inp.degree /= sub.degree

infoIsNotEmpty : Model -> Bool
infoIsNotEmpty model =
    model.inputContent.firstName /= "" &&
    model.inputContent.lastName /= "" &&
    model.inputContent.degree /= None

hasCheckedAllRules : Model -> Bool
hasCheckedAllRules model =
    case model.checkedRules of
        (True, True, True) ->
            True
        _ ->
            False

getCheckboxClass : CheckboxId -> Model -> String
getCheckboxClass id model =
    case model.checkedRules of
        (one, two, three) ->
            case id of
                FirstBox ->
                    if one then
                        "checked-box"
                    else
                        "unchecked-box"
                SecondBox ->
                    if two then
                        "checked-box"
                    else
                        "unchecked-box"
                ThirdBox ->
                    if three then
                        "checked-box"
                    else
                        "unchecked-box"

view : Model -> Html Msg
view model =
    div []
        [ showPage model
--      , text (Debug.toString model)
        ]

-- Shows a subpage
showPage : Model -> Html Msg
showPage model =
    let msgToUser = Error.toString model.error
        isRelease = (Time.posixToMillis model.currentTime) >= påmeldingUte
    in
        case model.currentSubPage of
            Verified ->
                div [ class "verified" ]
                    [ div [ class "text" ] 
                        [ if model.error == NoError then
                            text "Du har nå blitt logget inn. Vennligst vent mens du blir videresendt..."
                         else
                             text msgToUser
                        ]
                    ]
            MinSide ->
                div [ class "min-side" ]
                    [ div [ id "min-side-content" ]
                        [ h1 [ class "min-side-item", id "min-side-header"] [ text "Registrering" ]
                        , div [ class "min-side-item text" ]
                            [ div [] [ text "Her kan du registrere deg i forkant av påmeldingen." ]
                            , div [ class "inline-text" ] [ text "Påmeldingen vil dukke opp her " ]
                            , div [ class "inline-link" ] [ text " 29. april kl. 12:00" ]
                            , div [ class "inline-text" ] [ text ", gitt at du er logget inn og har registrert deg." ]
                            , br [] []
                            , div [ class "inline-text" ] [ text "Det er " ]
                            , div [ class "inline-link" ] [ text "IKKE" ]
                            , div [ class "inline-text" ] [ text " nødvendig å refreshe siden for å få påmeldingen til å vises." ]
                            ]
                        , div [ id "err-msg" ] [ text msgToUser ]
                        , if (Ticket.toBool model.user.hasTicket) then
                            div [ id "has-ticket-yes" ]
                                [ text "Du har fått plass!" ]
                          else
                            div [ id "has-ticket-no" ]
                                [ text "Du har ikke fått plass enda." ]
                        , input [ class "min-side-item"
                                , type_ "text"
                                , disabled True
                                , placeholder "Email"
                                , value (Email.toString model.user.email) 
                                , autocomplete False
                                ] []
                        , input [ class "min-side-item"
                                , type_ "text"
                                , placeholder "Fornavn"
                                , value (model.inputContent.firstName) 
                                , Html.Events.onInput TypedFirstName
                                , autocomplete False
                                ] []
                        , br [] []
                        , input [ class "min-side-item"
                                , type_ "text"
                                , placeholder "Etternavn"
                                , value (model.inputContent.lastName)
                                , Html.Events.onInput TypedLastName
                                , autocomplete False
                                ] []
                        , br [] []
                        , select [ class "min-side-item", value (Degree.toString True model.inputContent.degree), Html.Events.onInput TypedDegree ]
                            [ option [ value "" ] [ text (Degree.toString False None) ]
                            , option [ value "DTEK" ] [ text (Degree.toString False (Valid DTEK)) ]
                            , option [ value "DVIT" ] [ text (Degree.toString False (Valid DVIT)) ]
                            , option [ value "DSIK" ] [ text (Degree.toString False (Valid DSIK)) ]
                            , option [ value "BINF" ] [ text (Degree.toString False (Valid BINF)) ]
                            , option [ value "IMØ" ] [ text (Degree.toString False (Valid IMØ)) ]
                            , option [ value "IKT" ] [ text (Degree.toString False (Valid IKT)) ]
                            , option [ value "KOGNI" ] [ text (Degree.toString False (Valid KOGNI)) ]
                            , option [ value "INF" ] [ text (Degree.toString False (Valid INF)) ]
                            , option [ value "PROG" ] [ text (Degree.toString False (Valid PROG)) ]
                            ]
                        , div [ class "min-side-item checkbox-grid" ]
                            [ div [ class "checkbox-container" ]
                                [ Svg.svg [ Svg.Attributes.class (getCheckboxClass FirstBox model), Svg.Attributes.width "40", Svg.Attributes.height "40", Svg.Events.onClick CheckedBoxOne ]
                                    [ Svg.rect [ x "0", y "0", Svg.Attributes.width "40", Svg.Attributes.height "40" ] [] ]
                                ]
                            , div [ class "text" ]
                                [ text "Jeg bekrefter at jeg er representert av echo - Fagutvalget for Informatikk, ifølge echo sine statutter per 22. april 2020." ]
                            ]
                        , div [ class "min-side-item checkbox-grid" ]
                            [ div [ class "checkbox-container" ]
                                [ Svg.svg [ Svg.Attributes.class (getCheckboxClass SecondBox model), Svg.Attributes.width "40", Svg.Attributes.height "40", Svg.Events.onClick CheckedBoxTwo ]
                                    [ Svg.rect [ x "0", y "0", Svg.Attributes.width "40", Svg.Attributes.height "40" ] [] ]
                                ]
                            , div [ class "text" ]
                                [ text "Jeg bekrefter at jeg enten er påmeldt et bachelorprogram og starter mitt femte semester høsten 2020, eller er påmeldt et masterprogram og starter mitt første eller andre semester høsten 2020." ]
                            ]
                        , div [ class "min-side-item checkbox-grid" ]
                            [ div [ class "checkbox-container" ]
                                [ Svg.svg [ Svg.Attributes.class (getCheckboxClass ThirdBox model), Svg.Attributes.width "40", Svg.Attributes.height "40", Svg.Events.onClick CheckedBoxThree ]
                                    [ Svg.rect [ x "0", y "0", Svg.Attributes.width "40", Svg.Attributes.height "40" ] [] ]
                                ]
                            , div [ class "text" ]
                                [ div [ class "inline-text" ] [ text "Jeg godtar " ]
                                , a [ class "inline-link", href "/info" ] [ text "reglene" ]
                                , div [ class "inline-text" ] [ text " for bedriftsturen." ]
                                ]
                            ]
                        , div [ id "update-info-text" ]
                            [ text (if model.updateStage == Succeeded then "Dine endringer har blitt lagret!" else "") ]
                        , div [ class "min-side-item", id "min-side-buttons" ]
                            [ input
                                [ id "save-btn"
                                , type_ "button"
                                , value (if model.updateStage /= Updating then "Lagre endringer" else " . . . ")
                                , Html.Events.onClick
                                    (UpdateUserInfo model.session
                                        { firstName = model.inputContent.firstName, lastName = model.inputContent.lastName, degree = model.inputContent.degree, terms = (Terms (hasCheckedAllRules model)) }
                                    )
                                , disabled (not (hasChangedInfo model && hasCheckedAllRules model && infoIsNotEmpty model && Session.isSignedIn model.session))
                                ] []
                            , input [ id "signout-btn", type_ "button", value "Logg ut", Html.Events.onClick AttemptSignOut ] []
                            ]
                        ]
                    ]
