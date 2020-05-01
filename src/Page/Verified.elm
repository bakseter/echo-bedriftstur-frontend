port module Page.Verified exposing (init, subscriptions, view, update, Model, Msg, route)

import Html exposing (Html, div, span, br, text, input, select, option, h1, a, label)
import Html.Attributes exposing (class, id, type_, value, placeholder, disabled, style, autocomplete, href, target, rel, for, download)
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

import User exposing (User)
import Uid exposing (Uid(..))
import Email exposing (Email(..))
import Degree exposing (Degree(..), Degrees(..))
import Terms exposing (..)
import Content exposing (Content)
import Session exposing (Session)
import Ticket exposing (Ticket(..), toBool)
import Error exposing (Error(..))

paameldingUte : Int
paameldingUte =
    1588672800000

route : String
route =
    "verified"

redirectToHome : String
redirectToHome =
    "https://echobedriftstur.no"

port userStatusChanged : (Encode.Value -> msg) -> Sub msg
port signInSucceeded : (Encode.Value -> msg) -> Sub msg
port signInError : (Encode.Value -> msg) -> Sub msg

port getUserInfo : Encode.Value -> Cmd msg
port getUserInfoSucceeded : (Encode.Value -> msg) -> Sub msg
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
    = UpdateIdle
    | Updating
    | Succeeded

type TicketStage
    = TicketIdle
    | Creating
    | Created

type alias Model =
    { url : Url.Url
    , key : Browser.Navigation.Key
    , user : User
    , currentTime : Time.Posix
    , inputContent : Content
    , submittedContent : Content
    , checkedTerms : (Bool, Bool, Bool)
    , updateStage : UpdateUserInfoStage
    , ticketStage : TicketStage
    , currentSubPage : SubPage
    , session : Session
    , error : Error
    }

init : Url.Url -> Browser.Navigation.Key -> Model
init url key =
    { url = url
    , key = key
    , user = User.empty
    , currentTime = Time.millisToPosix 0
    , inputContent = Content.empty
    , submittedContent = Content.empty
    , checkedTerms = (False, False, False)
    , updateStage = UpdateIdle
    , ticketStage = TicketIdle
    , currentSubPage = Verified
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
                        newCheckedRules = if Terms.toBool content.terms then (True, True, True) else model.checkedTerms
                        newTicketStage = if content.submittedTicket then
                                            Created
                                         else
                                             TicketIdle
                    in ({ model | user = User content.email content.firstName content.lastName content.degree content.terms content.hasTicket content.submittedTicket
                        , submittedContent = submittedContent
                        , inputContent = submittedContent 
                        , checkedTerms = newCheckedRules
                        , ticketStage = newTicketStage }
                        , Cmd.none)
                Nothing ->
                    update (GotError (Error.encode "json-parse-error")) model
        UpdateUserInfo session content ->
            let newContent = Content model.inputContent.firstName model.inputContent.lastName model.inputContent.degree model.inputContent.terms
                message = Content.encode session newContent
            in ({ model | updateStage = Updating }, updateUserInfo message)
        UpdateUserInfoSucceeded _ ->
            let subCont = model.submittedContent
                newSubCont = Content.updateAll model.inputContent.firstName model.inputContent.lastName model.inputContent.degree model.inputContent.terms subCont
            in ({ model | submittedContent = newSubCont, updateStage = Succeeded }, Cmd.none)
        CreateTicket ->
            ({ model | ticketStage = Creating }, createTicket (Ticket.encode model.session))
        CreateTicketSucceeded json ->
            ({ model | ticketStage = Created }, Cmd.none)
        AttemptSignOut ->
            (model, attemptSignOut (Encode.object [ ("requestedSignOut", Encode.bool True) ]))
        SignOutSucceeded _ ->
            ((init model.url model.key), Browser.Navigation.load redirectToHome)
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
            let (one, two, three) = model.checkedTerms
                input = Content.updateTerms (Terms ((not one) && two && three)) model.inputContent
                newCheckedTerms = ((not one), two, three)
            in ({ model | inputContent = input, checkedTerms = newCheckedTerms }, Cmd.none)
        CheckedBoxTwo ->
            let (one, two, three) = model.checkedTerms
                input = Content.updateTerms (Terms (one && (not two) && three)) model.inputContent
                newCheckedTerms = (one, (not two), three)
            in ({ model | inputContent = input, checkedTerms = newCheckedTerms }, Cmd.none)
        CheckedBoxThree ->
            let (one, two, three) = model.checkedTerms
                input = Content.updateTerms (Terms (one && two && (not three))) model.inputContent
                newCheckedTerms = (one, two, (not three))
            in ({ model | inputContent = input, checkedTerms = newCheckedTerms }, Cmd.none)
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
        inp.degree /= sub.degree ||
        inp.terms /= sub.terms

infoIsNotEmpty : Model -> Bool
infoIsNotEmpty model =
    model.inputContent.firstName /= "" &&
    model.inputContent.lastName /= "" &&
    model.inputContent.degree /= None

getCheckboxClass : CheckboxId -> Model -> String
getCheckboxClass id model =
    case model.checkedTerms of
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

ticketElem : Ticket -> Html Msg
ticketElem (Ticket maybeBool) =
    case maybeBool of
        Just bool ->
            if bool then
                div [ class "ticket-item", id "has-ticket-yes" ]
                    [ text "Du har fått plass!" ]
              else
                div [ class "ticket-item", id "has-ticket-no" ]
                    [ text "Du fikk dessverre ikke plass." ]
        Nothing ->
            div [ class "ticket-item", id "has-ticket-maybe" ]
                [ text "Du har ikke fått plass enda." ]

getTicketBtn : Model -> Bool -> Html Msg
getTicketBtn model isRelease =
    case model.ticketStage of
        TicketIdle ->
            if isRelease then
                input
                    [ id "ticket-button"
                    , type_ "button"
                    , value "Meld meg på!"
                    , disabled False
                    , Html.Events.onClick CreateTicket
                    ] []
            else
                input
                    [ id "ticket-button"
                    , type_ "button"
                    , value "Påmeldingen har ikke åpnet enda."
                    , disabled True
                    ] []
        Creating ->
            input
                [ id "ticket-button"
                , type_ "button"
                , value " . . . "
                , disabled True
                ] []
        Created ->
            input
                [ id "ticket-button"
                , type_ "button"
                , value "Du har blitt meldt på."
                , disabled True
                ] []

view : Model -> Html Msg
view model =
    showSubPage model

-- Shows a subpage
showSubPage : Model -> Html Msg
showSubPage model =
    let msgToUser = Error.toString model.error
        miscDegree = model.user.degree == Valid MISC
        isRelease = (Time.posixToMillis model.currentTime) >= paameldingUte
    in
        case model.currentSubPage of
            Verified ->
                div [ class "verified" ]
                    [ if model.error == NoError then
                        div [ class "text-bold text-center" ] [ text "Du har nå blitt logget inn. Vennligst vent mens du blir videresendt..." ]
                      else
                        div [ class "text-bold text-center", style "color" "red" ] [ text msgToUser ]
                    ]
            MinSide ->
                div [ class "min-side" ]
                    [ div [ id "min-side-content" ]
                        [ h1 [ class "min-side-item" ] [ text "Registrering og påmelding" ]
                        , div [ class "min-side-item text" ]
                            [ div [] [ text "Her kan du registrere deg i forkant av påmeldingen." ]
                            , span [] [ text "Du melder deg på ved å trykke på knappen under når påmeldingen åpner " ]
                            , span [ class "text-underline" ] [ text " 5. mai kl. 12:00" ]
                            , span [] [ text ", gitt at du er logget inn og har fylt inn din informasjon." ]
                            ]
                        , div [ class "min-side-item", id "err-msg" ] [ text msgToUser ]
                        , div [ class "min-side-item", id "tickets" ]
                            [ ticketElem model.user.hasTicket
                            , getTicketBtn model isRelease
                            ]
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
                        , label [ id "degree-label", for "degree" ] [ text "Studieretning" ]
                        , select [ class "min-side-item"
                                 , id "degree"
                                 , value (Degree.toString True model.inputContent.degree)
                                 , disabled miscDegree
                                 , Html.Events.onInput TypedDegree
                                 ]
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
                            , option [ value "POST" ] [ text (Degree.toString False (Valid POST)) ]
                            , if miscDegree then
                                option [ value "MISC" ] [ text (Degree.toString False (Valid MISC)) ]
                             else
                                span [] []
                            ]
                        , div [ class "min-side-item checkbox-grid" ]
                            [ div [ class "checkbox-container" ]
                                [ Svg.svg [ Svg.Attributes.class (getCheckboxClass FirstBox model), Svg.Attributes.width "40", Svg.Attributes.height "40", Svg.Events.onClick CheckedBoxOne ]
                                    [ Svg.rect [ x "0", y "0", Svg.Attributes.width "40", Svg.Attributes.height "40" ] [] ]
                                ]
                            , div [ class "text" ]
                                [ span [] [ text "Jeg bekrefter at jeg er representert av echo – Fagutvalget for informatikk, ifølge echo sine " ] 
                                , a [ class "text-underline", href "/docs/statutter_30_04_2020.pdf", download "statutter_30_04_2020.pdf" ]
                                    [ text "statutter" ]
                                , span [] [ text " per 30. april 2020." ]
                                ]
                            ]
                        , div [ class "min-side-item checkbox-grid" ]
                            [ div [ class "checkbox-container" ]
                                [ Svg.svg [ Svg.Attributes.class (getCheckboxClass SecondBox model), Svg.Attributes.width "40", Svg.Attributes.height "40", Svg.Events.onClick CheckedBoxTwo ]
                                    [ Svg.rect [ x "0", y "0", Svg.Attributes.width "40", Svg.Attributes.height "40" ] [] ]
                                ]
                            , div [ class "text" ]
                                [ text 
                                    """
                                    Jeg bekrefter at jeg er påmeldt et bachelorprogram og starter mitt femte semester høsten 2020,
                                    har søkt/er påmeldt et masterprogram og starter mitt første eller andre semester høsten 2020,
                                    eller er en postbachelorstudent og tar emner ved institutt for informatikk eller institutt for matematikk høsten 2020.
                                    """
                                ]
                            ]
                        , div [ class "min-side-item checkbox-grid" ]
                            [ div [ class "checkbox-container" ]
                                [ Svg.svg [ Svg.Attributes.class (getCheckboxClass ThirdBox model), Svg.Attributes.width "40", Svg.Attributes.height "40", Svg.Events.onClick CheckedBoxThree ]
                                    [ Svg.rect [ x "0", y "0", Svg.Attributes.width "40", Svg.Attributes.height "40" ] [] ]
                                ]
                            , div [ class "text" ]
                                [ span [] [ text "Jeg bekrefter at informasjonen over stemmer, jeg godtar " ]
                                , a [ class "text-underline", href "/info" ] [ text "reglene" ]
                                , span [] [ text " for bedriftsturen, og jeg godtar " ]
                                , a [ class "text-underline", href "/docs/personvernerklæring.pdf", download "personvernerklæring.pdf" ] [ text "personvernerklæringen" ]
                                , span [] [ text "." ]
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
                                        (Content model.inputContent.firstName model.inputContent.lastName model.inputContent.degree model.inputContent.terms)
                                    )
                                , disabled (not (hasChangedInfo model && (Terms.toBool model.inputContent.terms) && infoIsNotEmpty model && Session.isSignedIn model.session))
                                ] []
                            , input [ id "signout-btn", type_ "button", value "Logg ut", Html.Events.onClick AttemptSignOut ] []
                            ]
                        ]
                    ]
