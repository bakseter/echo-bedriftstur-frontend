module Page.LoggInn exposing (Model, Msg, init, route, subscriptions, title, toSession, update, view)

import Email exposing (Email(..))
import Error exposing (Error(..))
import Html exposing (Html, a, br, div, h1, input, span, text)
import Html.Attributes exposing (autocomplete, class, disabled, href, id, placeholder, rel, target, type_, value)
import Html.Events
import Session exposing (Session)
import Time


ready : Int
ready =
    1588067400000


type Msg
    = Tick Time.Posix
    | TypedEmail String


type SubPage
    = NotReady
    | SignIn
    | LinkSent


type alias Model =
    { session : Session
    , currentSubPage : SubPage
    , currentTime : Time.Posix
    , email : Email
    , error : Error
    }


init : Session -> Model
init session =
    { currentSubPage = NotReady
    , currentTime = Time.millisToPosix 0
    , email = Email ""
    , error = NoError
    , session = session
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every 100 Tick
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick time ->
            let
                isReady =
                    Time.posixToMillis time >= ready

                newSubPage =
                    if isReady && model.currentSubPage == NotReady then
                        SignIn

                    else
                        model.currentSubPage
            in
            ( { model | currentTime = time, currentSubPage = newSubPage }, Cmd.none )

        TypedEmail str ->
            ( { model | email = Email str }, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "logg-inn" ]
        [ showPage model ]


showPage : Model -> Html Msg
showPage model =
    case model.currentSubPage of
        NotReady ->
            div [ id "logg-inn-content" ]
                [ h1 [] [ text "Lag bruker/logg inn" ]
                , br [] []
                , br [] []
                , span [ class "text-bold" ]
                    [ text "Det er ikke mulig å lage en bruker eller logge inn enda. " ]
                , span [ class "text-bold" ]
                    [ text "Registrering åpner " ]
                , span [ class "text-underline" ]
                    [ text "28. april kl. 12:00" ]
                , span [ class "text-bold" ]
                    [ text "." ]
                ]

        SignIn ->
            div [ id "logg-inn-content" ]
                [ h1 [] [ text "Lag bruker/logg inn" ]
                , div [ class "text", id "login-info" ]
                    [ br [] []
                    , div []
                        [ text "For å lage en bruker eller logge inn, vennligst oppgi en gyldig studentmail på formen:" ]
                    , div [ class "text-bold" ]
                        [ text "fornavn.etternavn@student.uib.no" ]
                    , br [] []
                    , div []
                        [ text "Du vil få tilsendt en inloggingslink til mailen du oppgir. Denne bruker du for å logge inn." ]
                    , br [] []
                    , div [ class "text-bold" ]
                        [ text "Vi anbefaler sterkt å sette opp videresending fra studentmailen din til din vanlig mailadresse." ]
                    , div [ class "text-bold" ]
                        [ text "Da får du mye lettere med deg informasjon vi sender deg på mail senere." ]
                    , br [] []
                    , span [] [ text "Følg " ]
                    , a [ class "text-underline", target "_blank", rel "noopener noreferrer", href "https://tjinfo.uib.no/reise" ]
                        [ text "denne" ]
                    , span [] [ text " lenken for å sette opp videresending fra din studentmail." ]
                    , br [] []
                    , br [] []
                    , span [ class "text" ] [ text "Dersom du åpner mailen på webmail.uib.no, må du trykke på " ]
                    , span [ class "text-italic" ] [ text "Vis i HTML" ]
                    , span [ class "text" ] [ text " for å få frem linken." ]
                    , br [] []
                    , br [] []
                    ]
                , div [ id "login-form" ]
                    [ input
                        [ if isEmailValid model.email then
                            id "email-valid"

                          else if model.error /= NoError then
                            id "email-invalid"

                          else
                            id "email"
                        , type_ "text"
                        , Html.Events.onInput TypedEmail
                        , autocomplete False
                        , placeholder "Email"
                        ]
                        []
                    , br [] []
                    , br [] []
                    , input
                        [ id "submitBtn"
                        , type_ "button"
                        , value "Logg inn"
                        , if isEmailValid model.email then
                            disabled False

                          else
                            disabled True
                        ]
                        []
                    ]
                , div [ id "err-msg" ] [ text (Error.toString model.error) ]
                ]

        LinkSent ->
            div [ class "text-center", id "logg-inn-content" ]
                [ h1 [] [ text "Lag bruker/logg inn" ]
                , br [] []
                , br [] []
                , div []
                    [ text ("Vi har nå sendt deg en mail på " ++ (String.toLower << Email.toString) model.email ++ ".") ]
                , div []
                    [ text "Husk å sjekke søppelposten din!" ]
                ]


isEmailValid : Email -> Bool
isEmailValid email =
    String.right (String.length "@student.uib.no") (Email.toString email) == "@student.uib.no"


route : String
route =
    "logg-inn"


title : String
title =
    "Logg inn"


toSession : Model -> Session
toSession model =
    model.session
