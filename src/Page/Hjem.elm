module Page.Hjem exposing (init, subscriptions, update, view, Model, Msg)

import Html exposing (Html, div, span, h1, text, br)
import Html.Attributes exposing (class, id)
import Time
import Animation
import Animation.Messenger
import String

type Msg
    = Tick Time.Posix
    | TransitionHeader
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
    Sub.batch [ Time.every 1000 Tick
              , Animation.subscription AnimateHeader [ model.headerStyle ]  
              ]

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
            , getClock model
            ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Tick time ->
            ({ model | currentTime = time}, Cmd.none)
        TransitionHeader ->
             ({ model | headerName = nextName model.headerName }, Cmd.none)
        AnimateHeader anim ->
            let (newHeaderStyle, headerCmds) = Animation.Messenger.update anim model.headerStyle
            in
               ({ model | headerStyle = newHeaderStyle }, headerCmds)

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
    let dateThen = 1598436000 * 1000
        date = dateThen - (Time.posixToMillis dateNow)
    in
        if date == dateThen then 
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
