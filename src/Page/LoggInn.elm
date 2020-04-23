port module Page.LoggInn exposing (init, subscriptions, update, view, Model, Msg, route)

import Html exposing (Html, div, text, h1, h3, img, form, input, br, p, span, a)
import Html.Attributes exposing (class, id, src, alt, type_, value, style, disabled, autocomplete, href, target, rel)
import Html.Events
import Json.Encode
import Json.Decode
import Time
import Countdown

import Error exposing (Error(..))

launch : Int
launch =
--  1587549600000
    0

type Msg
    = Tick Time.Posix
    | TypedEmail String
    | SendSignInLink
    | SendSignInLinkSucceeded Json.Encode.Value
    | SendSignInLinkError Json.Encode.Value

type SubPage
    = Countdown
    | SignIn
    | LinkSent

type alias Model = 
    { currentSubPage : SubPage
    , currentTime : Time.Posix
    , email : String
    , error : Error
    }
     
route : String
route =
    "logg-inn"
init : Model
init =
    { currentSubPage = Countdown
    , currentTime = Time.millisToPosix 0
    , email = ""
    , error = NoError
    }

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 100 Tick
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
            let newModel = { model | currentTime = time }
            in
                if (Time.posixToMillis time) < launch then
                    ({ newModel | currentSubPage = Countdown }, Cmd.none)
                else if model.currentSubPage /= LinkSent then
                    ({ newModel | currentSubPage = SignIn }, Cmd.none)
                else
                    (newModel, Cmd.none)
        TypedEmail str ->
            ({ model | email = str }, Cmd.none)
        SendSignInLink ->
            (model, sendSignInLink (encode model))
        SendSignInLinkSucceeded _ ->
            ({ model | currentSubPage = LinkSent }, Cmd.none)
        SendSignInLinkError json ->
            ({ model | error = (Error.fromJson json) }, Cmd.none)

view : Model -> Html Msg
view model =
    div [ class "logg-inn" ]
        [ showPage model ]

encode : Model -> Json.Encode.Value
encode model =
    Json.Encode.object
        [ ( "email", Json.Encode.string model.email ) ]

showPage : Model -> Html Msg
showPage model =
    case model.currentSubPage of
        Countdown ->
            div [ id "logg-inn-content" ]
                [ div [ id "countdown" ]
                    (Tuple.first (Countdown.countdownFromTo (Time.posixToMillis model.currentTime) launch))
                ]
        SignIn ->
            div [ id "logg-inn-content" ]
                [ h1 [] [ text "Registrer deg/logg inn" ] 
                , div [ class "text", id "login-info" ]
                    [ br [] []
                    , div [] 
                        [ text "For å registrere deg eller logge inn, vennligst oppgi en gyldig studentmail på formen:" ]
                    , div [ style "font-style" "italic" ]
                        [ text "Fornavn.Etternavn@student.uib.no" ]
                    , br [] []
                    , div []
                        [ text "Du vil få tilsendt en link til mailen du oppgir. Denne bruker du for å logge inn." ]
                    , br [] []
                    , div [ style "font-weight" "bold" ]
                        [ text "Vi anbefaler sterkt å sette opp videresending fra studentmailen din til din vanlig mailadresse." ]
                    , div [ style "font-weight" "bold" ]
                        [ text "Da får du mye lettere med deg eventuell informasjon vi sender deg på mail senere." ]
                    , br [] []
                    , div [ class "inline-text" ]
                        [ text "Følg " ]
                    , a [ class "inline-link", target "_blank", rel "noopener noreferrer", href "https://tjinfo.uib.no/reise" ]
                        [ text "denne" ]
                    , div [ class "inline-text" ]
                        [ text " lenken for å sette opp videresending fra din studentmail." ]
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
                , h3 [] [ text (Error.toString model.error) ]
                ]
        LinkSent ->
            div [ id "logg-inn-content" ]
                [ h1 [] [ text "Registrer deg/logg inn" ]
                , p []
                    [ text ("Vi har nå sendt deg en mail på " ++ model.email ++ ".") ]
                , p []
                    [ text "Husk å sjekke søppelposten din!" ]
                ]

isEmailValid : String -> Bool
isEmailValid str =
    (String.right (String.length "@student.uib.no")) str == "@student.uib.no"

countdown : Model -> SubPage
countdown model =
    if (Time.posixToMillis model.currentTime) >= launch then
        SignIn
    else
        Countdown
