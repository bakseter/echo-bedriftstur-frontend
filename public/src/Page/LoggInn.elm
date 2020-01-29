port module Page.LoggInn exposing (init, subscriptions, update, view, Model, Msg)

import Html exposing (Html, div, text, h1, h3, img, form, input, br, p)
import Html.Attributes exposing (class, id, src, alt, type_, name, value, method)
import Html.Events
import Json.Encode

type Msg
    = TypedEmail String
    | TypedPassword String
    | TypedUsername String
    | LogIn
    | LogInSuccessful Json.Encode.Value

type alias Model = 
    { email : String
    , username : String
    , password : String
    , info : String
    }
     
init : Model
init =
    { email = ""
    , username = ""
    , password = ""
    , info = ""
    }

subscriptions : Model -> Sub Msg
subscriptions model =
    logInInfo LogInSuccessful

port logIn : Json.Encode.Value -> Cmd msg
port logInInfo : (Json.Encode.Value -> msg) -> Sub msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        TypedEmail str ->
            ({ model | email = str }, Cmd.none)
        TypedUsername str ->
            ({ model | username = str}, Cmd.none)
        TypedPassword str ->
            ({ model | password = str }, Cmd.none)
        LogIn ->
            (model, logIn (encode model))
        LogInSuccessful json ->
            ({ model | info = Json.Encode.encode 2 json }, Cmd.none)


view : Model -> Html Msg
view model =
    div [ class "logg-inn" ]
        [ div [] []
        , div []
            [ h1 [] [ text "Logg inn" ] 
            , div[ id "loginForm" ]
                [ h3 [] [ text "Email" ]
                , input [ id "email", type_ "text", name "Email", Html.Events.onInput TypedEmail ] []
                , br [] []
                , h3 [] [ text "Brukernavn" ]
                , input [ id "brukernavn", type_ "text", Html.Events.onInput TypedUsername ] []
                , br [] []
                , h3 [] [ text "Passord" ]
                , input [ id "passord", type_ "password", Html.Events.onInput TypedPassword ] []
                , br [] []
                , br [] []
                , input [ id "submitBtn", type_ "submit", value "Logg inn", Html.Events.onClick LogIn ] []
                ]
            , p [] [ text model.info ]
            ]
        ]

encode : Model -> Json.Encode.Value
encode model =
    Json.Encode.object
        [ ( "email", Json.Encode.string model.email )
        , ( "password", Json.Encode.string model.password )
        , ( "username", Json.Encode.string model.username )
        ]
