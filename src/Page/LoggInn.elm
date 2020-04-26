port module Page.LoggInn exposing (init, subscriptions, update, view, Model, Msg, route)

import Html exposing (Html, div, text, h1, input, br, span, a)
import Html.Attributes exposing (class, id, src, alt, type_, value, disabled, autocomplete, href, target, rel, placeholder)
import Html.Events
import Json.Encode
import Json.Decode
import Time

import Email exposing (..)
import Error exposing (Error(..))

ready : Int
ready =
--  1588067700000

route : String
route =
    "logg-inn"

type Msg
    = Tick Time.Posix
    | TypedEmail String
    | SendSignInLink
    | SendSignInLinkSucceeded Json.Encode.Value
    | SendSignInLinkError Json.Encode.Value

type SubPage
    = NotReady
    | SignIn
    | LinkSent

type alias Model = 
    { currentSubPage : SubPage
    , currentTime : Time.Posix
    , email : Email
    , error : Error
    }
     
init : Model
init =
    { currentSubPage = NotReady
    , currentTime = Time.millisToPosix 0
    , email = Email ""
    , error = NoError
    }

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 1000 Tick
        , sendSignInLinkSucceeded SendSignInLinkSucceeded
        , sendSignInLinkError SendSignInLinkError
        ] 

port sendSignInLink : Json.Encode.Value -> Cmd msg
port sendSignInLinkError : (Json.Encode.Value -> msg) -> Sub msg
port sendSignInLinkSucceeded : (Json.Encode.Value -> msg) -> Sub msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Tick time ->
            let isReady = (Time.posixToMillis time) >= ready
                newSubPage = if isReady && model.currentSubPage == NotReady then
                                SignIn
                            else
                                model.currentSubPage
            in ({ model | currentTime = time, currentSubPage = newSubPage }, Cmd.none)
        TypedEmail str ->
            ({ model | email = Email str }, Cmd.none)
        SendSignInLink ->
            let lowercaseEmail = Email (String.toLower (Email.toString model.email))
            in (model, sendSignInLink (Email.encode lowercaseEmail))
        SendSignInLinkSucceeded _ ->
            ({ model | currentSubPage = LinkSent }, Cmd.none)
        SendSignInLinkError json ->
            ({ model | error = (Error.fromJson json) }, Cmd.none)

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
                , div [ class "text-center text-bold" ]
                    [ text "Det er ikke mulig å lage en bruker eller logge inn enda." ]
                , span [ class "text-center text-bold" ]
                    [ text "Registrering åpner " ]
                , span [ class "text-underline" ]
                    [ text "28. april kl. 12:00" ]
                , span [ class "text-center text-bold" ]
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
                        [ text "Du vil få tilsendt en link til mailen du oppgir. Denne bruker du for å logge inn." ]
                    , br [] []
                    , div [ class "text-bold" ]
                        [ text "Vi anbefaler sterkt å sette opp videresending fra studentmailen din til din vanlig mailadresse." ]
                    , div [ class "text-bold" ]
                        [ text "Da får du mye lettere med deg eventuell informasjon vi sender deg på mail senere." ]
                    , br [] []
                    , span [] [ text "Følg " ]
                    , a [ class "text-underline", target "_blank", rel "noopener noreferrer", href "https://tjinfo.uib.no/reise" ]
                        [ text "denne" ]
                    , span [] [ text " lenken for å sette opp videresending fra din studentmail." ]
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
                        , type_ "text", Html.Events.onInput TypedEmail
                        , autocomplete False
                        , placeholder "Email"
                        ] []
                    , br [] []
                    , br [] []
                    , input 
                        [ id "submitBtn"
                        , type_ "button"
                        , value "Logg inn"
                        , Html.Events.onClick SendSignInLink 
                        , if isEmailValid model.email then
                            disabled False
                          else
                            disabled True
                        ] []
                    ]
                , div [ id "err-msg" ] [ text (Error.toString model.error) ]
                ]
        LinkSent ->
            div [ class "text-center", id "logg-inn-content" ]
                [ h1 [] [ text "Lag bruker/logg inn" ]
                , div []
                    [ text ("Vi har nå sendt deg en mail på " ++ (String.toLower (Email.toString model.email)) ++ ".") ]
                , div []
                    [ text "Husk å sjekke søppelposten din!" ]
                ]

isEmailValid : Email -> Bool
isEmailValid email =
    (String.right (String.length "@student.uib.no")) (Email.toString email) == "@student.uib.no"
