port module Page.LoggInn exposing (init, subscriptions, update, view, Model, Msg, countdown)

import Html exposing (Html, div, text, h1, h3, img, form, input, br, p, span)
import Html.Attributes exposing (class, id, src, alt, type_, value, style, disabled)
import Html.Events
import Json.Encode
import Json.Decode
import Time

type Msg
    = Tick Time.Posix
    | TypedEmail String
    | SendSignInLink
    | SendSignInLinkSucceeded Json.Encode.Value
    | SendSignInLinkError Json.Encode.Value

type Error
    = NoError
    | InvalidEmail
    | UnathorizedContinueUri
    | InvalidContinueUri
    | ArgumentError

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
            let newModel = { model | currentTime = time }
            in
                if (Time.posixToMillis time) >= release then
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
            let error = errorFromString <| getErrorCode <| Json.Encode.encode 0 json
            in ({ model | error = error }, Cmd.none)

view : Model -> Html Msg
view model =
    div [ class "logg-inn" ]
        (showPage model)

encode : Model -> Json.Encode.Value
encode model =
    Json.Encode.object
        [ ( "email", Json.Encode.string model.email ) ]

showPage : Model -> List (Html Msg)
showPage model =
    case model.currentSubPage of
        Countdown ->
            [ getClock model ]
        SignIn ->
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
        LinkSent ->
            [ h1 [] [ text "Registrer deg/logg inn" ]
            , p []
                [ text ("Vi har nå sendt deg en mail på " ++ model.email ++ ".") ]
            , p []
                [ text "Husk å sjekke søppelposten din!" ]
            ]

getClock : Model -> Html msg
getClock model =
    div [ class "clock" ]
        ([ span [ id "days" ] [ text "D" ]
         , span [ id "hours" ] [ text "H" ]
         , span [ id "minutes" ] [ text "M" ]
         , span [ id "seconds" ] [ text "S" ]
         ] ++ getCountDown model.currentTime)

getCountDown : Time.Posix -> List (Html msg)
getCountDown dateNow =
    let date = release - (Time.posixToMillis dateNow)
    in
        if date == release then 
            (List.map (\x -> div 
                [ class "clock-item", id ("clock" ++ Tuple.second x) ]
                [ text <| fixNum <| String.fromInt <| Tuple.first x ]) 
                [(0,"D"),(0,"H"),(0,"M"),(0,"S")]) 
        else 
            (List.map (\x -> div 
                [ class "clock-item", id ("clock" ++ Tuple.second x) ]
                [ text <| fixNum <| String.fromInt <| Tuple.first x ])
                (calcDate date))

fixNum : String -> String
fixNum str =
    if String.length str == 1
    then
        "0" ++ str
    else
        str

calcDate : Int -> List (Int, String)
calcDate diff =
    let day = diff // (86400 * 1000)
        dayMod = modBy (86400 * 1000) diff
        hour = dayMod // (3600 * 1000)
        hourMod = modBy (3600 * 1000) dayMod
        min = hourMod // (60 * 1000)
        minMod = modBy (60 * 1000) hourMod
        sec = minMod // 1000
    in
        [(day,"D"), (hour,"H"), (min,"M"), (sec,"S")]

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
            NoError

errorMessageToUser : Error -> String
errorMessageToUser error =
    case error of
        InvalidEmail ->
            "Mailen du har skrevet inn er ikke gyldig. Prøv igjen"
        NoError ->
            ""
        _ ->
            "Det har skjedd en feil. Prøv igjen senere"

countdown : SubPage
countdown =
    Countdown

release : Int
release =
    1584961200000
