port module Page.LoggInn exposing (init, subscriptions, update, view, Model, Msg)

import Html exposing (Html, div, text, h1, h3, img, form, input, br, p, i)
import Html.Attributes exposing (class, id, src, alt, type_, value, style, disabled)
import Html.Events
import Json.Encode
import Json.Decode
import String

type Msg
    = TypedEmail String
    | SendSignInLink
    | SendSignInLinkSucceded Json.Encode.Value
    | SendSignInLinkError Json.Encode.Value

type Error
    = None
    | InvalidEmail
    | UnathorizedContinueUri
    | InvalidContinueUri
    | ArgumentError

type alias Model = 
    { email : String
    , submittedEmail : Bool
    , error : Error
    , info : String
    }
     
init : Model
init =
    { email = ""
    , submittedEmail = False
    , error = None
    , info = ""
    }

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ sendSignInLinkSucceeded SendSignInLinkSucceded
        , sendSignInLinkError SendSignInLinkError
        ] 

port sendSignInLink : Json.Encode.Value -> Cmd msg
port sendSignInLinkError : (Json.Encode.Value -> msg) -> Sub msg
port sendSignInLinkSucceeded : (Json.Encode.Value -> msg) -> Sub msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        TypedEmail str ->
            ({ model | email = str }, Cmd.none)
        SendSignInLink ->
            (model, sendSignInLink (encode model))
        SendSignInLinkSucceded _ ->
            ({ model | submittedEmail = True }, Cmd.none)
        SendSignInLinkError json ->
            let error = errorFromString (getErrorCode (Json.Encode.encode 0 json))
            in ({ model | info = (Json.Encode.encode 0 json), error = error }, Cmd.none)

view : Model -> Html Msg
view model =
    showPage model

encode : Model -> Json.Encode.Value
encode model =
    Json.Encode.object
        [ ( "email", Json.Encode.string model.email ) ]

showPage : Model -> Html Msg
showPage model =
    if model.submittedEmail && model.error == None
    then
        div [ class "logg-inn" ]
            [ div [] []
            , div []
                [ h1 [] [ text "Registrer deg/logg inn" ]
                , p []
                    [ text ("Vi har nå sendt deg en mail på " ++ model.email ++ ".") ]
                , p []
                    [ text "Husk å sjekke søppelposten din!" ]
                ]
            ]
    else
        div [ class "logg-inn" ]
            [ div [] []
            , div []
                [ h1 [] [ text "Registrer deg/logg inn" ] 
                , div [ id "login-info" ]
                    [ p [] 
                        [ text "For å registrere deg eller logge inn, vennligst oppgi en gyldig studentmail på formen:" ]
                    , p [ style "font-style" "italic" ]
                        [ text "Fornavn.Etternavn@student.uib.no" ]
                    , p []
                        [ text "Du vil få tilsendt en link til mailen du oppgir. Denne bruker du for å logge inn." ]
                    ]
                , div [ id "login-form" ]
                    [ input 
                        [ if model.error == InvalidEmail then
                            id "email-invalid"
                          else if isEmailValid model.email then
                            id "email-valid"
                          else
                            id "email"
                        , type_ "text", Html.Events.onInput TypedEmail ] []
                    , br [] []
                    , br [] []
                    , input 
                        [ id "submitBtn"
                        , type_ "submit"
                        , value "Logg inn"
                        , Html.Events.onClick SendSignInLink 
                        , if isEmailValid model.email then
                            disabled False
                          else
                            disabled True
                        ] []
                    ]
                , h3 [] [text (errorMessageToUser model.error) ]
                ]
            ]

isEmailValid : String -> Bool
isEmailValid str =
    (String.right (String.length "@student.uib.no")) str == "@student.uib.no"

getErrorCode : String -> String
getErrorCode json =
    case Json.Decode.decodeString Json.Decode.string json of
        Ok code ->
            code
        Err _ ->
            ""

errorFromString : String -> Error
errorFromString str =
    case str of
        "auth/invalid-email" ->
            InvalidEmail
        "auth/unauthorized-continue-uri" ->
            UnathorizedContinueUri
        "auth/invalid-continue-uri" ->
            InvalidContinueUri
        "auth/argumenterror" ->
            ArgumentError
        _ ->
            None

errorMessageToUser : Error -> String
errorMessageToUser error =
    case error of
        InvalidEmail ->
            "Mailen du har skrevet inn er ikke gyldig. Prøv igjen"
        None ->
            ""
        _ ->
            "Det har skjedd en feil. Prøv igjen senere"
