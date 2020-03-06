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
import User exposing (..)
import Uid exposing (..)
import Email exposing (..)
import Degree exposing (..)
import Content exposing (..)
import Session exposing (..)
import Ticket exposing (..)

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
    | UpdateUserInfo Session Content
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

type SubPage
    = Verified
    | MinSide

type alias Model =
    { url : Url.Url
    , key : Browser.Navigation.Key
    , user : User
    , currentTime : Time.Posix
    , inputContent : Content
    , submittedContent : Content
    , checkedRules : List (Bool)
    , currentSubPage : SubPage
    , session : Session
    }

init : Url.Url -> Browser.Navigation.Key -> Model
init url key =
    { url = url
    , key = key
    , user = User.empty
    , currentTime = Time.millisToPosix 0
    , inputContent = Content.empty
    , submittedContent = Content.empty
    , checkedRules = [ False, False, False ]
    , currentSubPage = Verified
    , session = Session.empty
    }

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 100 Tick
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
            let session = Session.decode json
            in
                if Session.isSignedIn session then
                    ({ model | session = session }, getUserInfo (Session.encode session))
                else
                    (model, Cmd.none)
        SignInSucceeded userJson ->
            ({ model | currentSubPage = MinSide }, Cmd.none)
        SignInError json ->
            (model, Cmd.none)
        GetUserInfoSucceeded json ->
            let content = User.decode json
                user = { email = content.email
                       , firstName = content.firstName
                       , lastName = content.lastName
                       , degree = content.degree
                       }
                submittedContent = { firstName = content.firstName
                                   , lastName = content.lastName
                                   , degree = content.degree
                                   }
            in
                ({ model | user = user, submittedContent = submittedContent, inputContent = submittedContent }, Cmd.none)
        GetUserInfoError json ->
            (model, Cmd.none)
        UpdateUserInfo session content ->
            let newContent = { firstName = model.inputContent.firstName
                             , lastName = model.inputContent.lastName
                             , degree = model.inputContent.degree
                             } 
                message = Content.encode session newContent
            in (model, Cmd.batch [ updateUserInfo message, Browser.Navigation.pushUrl model.key redirectToHome ])
        UpdateUserInfoSucceeded _ ->
            let subCont = model.submittedContent
                newSubCont = Content.updateAll model.user.firstName model.user.lastName model.user.degree subCont
            in ({ model | submittedContent = newSubCont }, attemptSignOut (Encode.object [ ("requestedSignOut", Encode.bool True) ]))
        UpdateUserInfoError json ->
            (model, Cmd.none)
        CreateTicket ->
            (model, createTicket (Ticket.encode model.session))
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
    List.all (\x -> x == True) model.checkedRules

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
                            , br [] []
                            , div [ style "font-weight" "bold"] [ text "Det er IKKE nødvendig å refreshe siden for å få påmeldingen til å vises." ]
                            ]
                        , input [ class "min-side-item"
                                , id "email", type_ "text"
                                , disabled True
                                , value (Email.toString model.user.email) 
                                ] []
                        , input [ class "min-side-item"
                                , id "firstName"
                                , type_ "text"
                                , placeholder "Fornavn"
                                , value (model.inputContent.firstName) 
                                , Html.Events.onInput TypedFirstName
                                ] []
                        , br [] []
                        , input [ class "min-side-item"
                                , id "lastName"
                                , type_ "text"
                                , placeholder "Etternavn"
                                , value (model.inputContent.lastName)
                                , Html.Events.onInput TypedLastName
                                ] []
                        , br [] []
                        , select [ class "min-side-item", id "degree", value (Degree.toString True model.inputContent.degree), Html.Events.onInput TypedDegree ]
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
                                    (UpdateUserInfo model.session
                                        { firstName = model.inputContent.firstName, lastName = model.inputContent.lastName, degree = model.inputContent.degree }
                                    )
                                , disabled (not (hasChangedInfo model && hasCheckedAllRules model && infoIsNotEmpty model && Session.isSignedIn model.session))
                                ] []
                            , input [ id "signout-btn", type_ "button", value "Logg ut", Html.Events.onClick AttemptSignOut ] []
                            ]
                        , if True then
                            input [ class "min-side-item", type_ "button", value "Meld meg på!", Html.Events.onClick CreateTicket ] []
                         else
                             div [ class "min-side-item", id "countdown" ]
                                []
                        ]
                    ]
