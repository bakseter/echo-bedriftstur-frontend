module Page.Hjem exposing (init, subscriptions, update, view, Model, Msg)

import Html exposing (Html, div, span, h1, text, br)
import Html.Attributes exposing (class, id)
import Time
import Animation
import Animation.Messenger
import String

type Msg
    = TransitionHeader
    | AnimateHeader Animation.Msg

type Name
    = Initial
    | Mnemonic
    | Computas
    | Knowit
    | Dnb
    | Bekk

type alias Names
    = (Name, String)

type alias Model =
    { headerName : Names
    , headerStyle : Animation.Messenger.State Msg
    , currentTime : Time.Posix
    }

init : Model
init =
    { headerName = (Initial, "bedriftstur")
    , headerStyle = Animation.interrupt
                        [ Animation.loop 
                            [ Animation.wait (Time.millisToPosix 4000)
                            , Animation.to [ Animation.opacity 0 ]
                            , Animation.Messenger.send TransitionHeader
                            , Animation.wait (Time.millisToPosix 1500)
                            , Animation.to [ Animation.opacity 1 ]
                            ] 
                        ] (Animation.style [ Animation.opacity 1 ])
    , currentTime = (Time.millisToPosix 0)
    }

subscriptions : Model -> Sub Msg 
subscriptions model =
    Animation.subscription AnimateHeader [ model.headerStyle ]  

view : Model -> Html Msg
view model =
    let (_, name) = nextName model.headerName
    in
        div [ class "hjem" ]
            [ div [ class "hjem-content" ]
                [ div []
                    [ h1 [ class "anim-text" ] [ text "echo | " ]
                    , h1
                        (Animation.render model.headerStyle ++ [ class "anim-text" ]) [ text name ]
                    ]
                , br [] []
                , div [ class "text" ] [ text "echo har startet en komité for å arrangere bedriftstur til Oslo høsten 2020." ]
                , div [ class "text" ] [ text "Formålet med arrangementet er å gjøre våre informatikkstudenter kjent med karrieremulighetene i Oslo." ]
                , br [] []
                , div [ class "text" ] [ text "Informasjon kommer fortløpende!" ]
                ]
            ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        TransitionHeader ->
             ({ model | headerName = nextName model.headerName }, Cmd.none)
        AnimateHeader anim ->
            let (newHeaderStyle, headerCmds) = Animation.Messenger.update anim model.headerStyle
            in
               ({ model | headerStyle = newHeaderStyle }, headerCmds)

nextName : Names -> Names
nextName name =
    case name of
        (Initial, _) ->
            (Mnemonic, "mnemonic")
        (Mnemonic, _) ->
            (Computas, "Computas")
        (Computas, _) ->
            (Knowit, "Knowit")
        (Knowit, _) ->
            (Dnb, "DNB")
        (Dnb, _) ->
            (Bekk, "Bekk")
        (Bekk, _) ->
            (Initial, "bedriftstur")
