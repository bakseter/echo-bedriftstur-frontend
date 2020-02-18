port module Page.LoggInn exposing (init, subscriptions, update, view, Model, Msg)

import Html exposing (Html, div, text, h1, h3, img, form, input, br, p, i)
import Html.Attributes exposing (class, id, src, alt, type_, name, value, method)
import Html.Events
import Json.Encode
import String

type Msg
    = TypedEmail String
    | LogIn

type alias Model = 
    { email : String
    , submittedEmail : Bool
    }
     
init : Model
init =
    { email = ""
    , submittedEmail = False
    }

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

port logIn : Json.Encode.Value -> Cmd msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        TypedEmail str ->
            ({ model | email = str }, Cmd.none)
        LogIn ->
            ({ model | submittedEmail = True }, logIn (encode model))

view : Model -> Html Msg
view model =
    showPage model

encode : Model -> Json.Encode.Value
encode model =
    Json.Encode.object
        [ ( "email", Json.Encode.string model.email ) ]


showPage : Model -> Html Msg
showPage model =
    if model.submittedEmail
    then
        div [ class "logg-inn" ]
            [ div [] []
            , div []
                [ h1 [] [ text "Registrer deg/logg inn" ] 
                , div [ id "login-info" ]
                    [ p [] 
                        [ text "For å registrere deg eller logge inn, vennligst oppgi en gyldig studentmail på formen:" ]
                    , p []
                        [ text "Fornavn.Etternavn@student.uib.no" ]
                    , p []
                        [ text "Du vil få tilsendt en link til mailen du oppgir. Denne bruker du for å logge inn." ]
                    ]
                , div [ id "login-form" ]
                    [ h3 [] [ text "Email" ]
                    , input [ id "email", type_ "text", name "Email", Html.Events.onInput TypedEmail ] []
                    , isEmailCorrect model
                    , br [] []
                    , br [] []
                    , input [ id "submitBtn", type_ "submit", value "Logg inn", Html.Events.onClick LogIn ] []
                    ]
                ]
            ]
    else
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

-- TODO: make this less spaghetti
isEmailCorrect : Model -> Html msg
isEmailCorrect model =
    let split = String.split "@" model.email
        domain = List.head (List.drop ((List.length split) - 1) split)
    in
        if List.length split == 1 || List.head split == Just "" || List.head split == Nothing
        then
            i [ class "fa fa-times", id "email-invalid" ] []
        else
            case domain of
                Just str ->
                    if str == "student.uib.no"
                    then
                        i [ class "fa fa-check", id "email-valid" ] []
                    else
                        i [class "fa fa-times", id "email-invalid" ] []
                Nothing ->
                        i [class "fa fa-times", id "email-invalid" ] []
