module Page.Hjem exposing (init, subscriptions, update, view, Model, Msg)

import Html exposing (Html, div, span, h1, text, br, ul, li, i)
import Html.Attributes exposing (class, id)
import Html.Events
import Animation
import Animation.Messenger
import Time

type Msg
    = TransitionHeader
    | AnimateHeader Animation.Msg

type Name
    = Initial
    | Mnemonic
    | Computas
    | Cisco
    | Knowit
    | Dnb
    | Bekk

type alias Model =
    { headerName : Name
    , headerStyle : Animation.Messenger.State Msg
    }

init : Model
init =
    { headerName = Bekk
    , headerStyle = Animation.interrupt
                        [ Animation.loop 
                            [ Animation.wait (Time.millisToPosix 4000)
                            , Animation.to [ Animation.opacity 0 ]
                            , Animation.Messenger.send TransitionHeader
                            , Animation.wait (Time.millisToPosix 1500)
                            , Animation.to [ Animation.opacity 1 ]
                            ] 
                        ] (Animation.style [ Animation.opacity 1 ])
    }

subscriptions : Model -> Sub Msg 
subscriptions model =
    Animation.subscription AnimateHeader [ model.headerStyle ]  

view : Model -> Html Msg
view model =
    let name = nameToString (nextName model.headerName)
    in
        div [ class "hjem" ]
            [ div [ class "hjem-content" ]
                [ div [ class "text" ]
                    [ h1 [ class "anim-text" ] [ text "echo | " ]
                    , h1
                        (Animation.render model.headerStyle ++ [ class "anim-text" ]) [ text name ]
                    , br [] []
                    , br [] []
                    , div [] [ text "echo har startet en komité for å arrangere bedriftstur til Oslo høsten 2020." ]
                    , div [] [ text "Formålet med arrangementet er å gjøre våre informatikkstudenter kjent med karrieremulighetene i Oslo." ]
                    , br [] []
                    , div [] [ text "Mer informasjon kommer snart!" ]
                    ]
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

namesList : List (Name, String)
namesList = 
    [ (Initial, "bedriftstur")
    , (Mnemonic, "mnemonic")
    , (Computas, "Computas")
    , (Cisco, "Cisco")
    , (Knowit, "Knowit")
    , (Dnb, "DNB")
    , (Bekk, "Bekk")
    ]

stringToName : String -> Name
stringToName str =
    let result = List.filter (\(x,y) -> y == str) namesList
    in
        case result of
            [ (name, _) ] ->
                name
            _ ->
                Initial

nameToString : Name -> String
nameToString name =
    let result = List.filter (\(x,y) -> x == name) namesList
    in
        case result of
            [ (_, string) ] ->
                string
            _ ->
                ""

nextName : Name -> Name
nextName name =
    case name of
        Initial -> Mnemonic
        Mnemonic -> Computas
        Computas -> Cisco
        Cisco -> Knowit
        Knowit -> Dnb
        Dnb -> Bekk
        Bekk -> Initial

mail : String
mail =
    "andreas.bakseter@echo.uib.no"
