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
                    {-
                    , div [] [ text "Arrangementet går over 3 dager; 26., 27. og 28. august 2020." ]
                    , br [] []
                    , div [] [ text "Registrering åpner kl. 12:00 17. mars. Da kan du registrere deg med din studentmail, og fylle inn din kontaktinformasjon. Dette gjør det lettere og raskere for deg å melde deg på når påmeldingen kommer ut, fordi all informasjonen din allerede er fylt inn." ]
                    , br [] []
                    , div [] [ text "Påmelding åpner kl. 12:00 3. april. Her gjelder “førstemann-til-mølla”-prinsippet." ]
                    , div [] [ text "Det vil være 20 plasser til 3. klassinger og 27 plasser til 4. klassinger." ]
                    , div [] [ text "Både registrering og påmelding vil foregå på denne nettsiden." ]
                    , br [] []
                    , div [] [ text "For å melde deg på bedriftsturen må du:" ]
                    , ul []
                        [ li [] [ text "være påmeldt et bachelorprogram og begynne ditt 5. semester august 2020 eller være påmeldt et masterprogram og begynne ditt 1. eller 2. semester august 2020" ]
                        , li [] [ text "være representert av echo, ifølge echo sine statutter per 17. mars 2020" ]
                        , li [] [ text "følge normert studieløp" ]
                        ]
                    , div [] [ text ("Har du et spesielt studieløp som ikke faller under disse kriteriene, kan du maile oss på " ++ mail ++ ", så vil vi evaluere om du kan melde deg på turen.") ]
                    , br [] []
                    , h1 [] [ text "Transport & hotell" ]
                    , br [] []
                    , br [] []
                    , div [] [ text "Vi flyr med Norwegian fra Bergen lufthavn 26. August kl. 07:30." ]
                    , div [] [ text "Det anmodes om å møte opp i god tid, senest kl. 06:30." ]
                    , div [] [ text "Du er selv ansvarlig for å rekke flyet." ]
                    , br [] []
                    , div [] [ text "Inkludert i billetten er opptil to kolli innsjekket bagasje per person (maks 20 kg per kolli), i tillegg til én håndbagasje (10 kg)." ]
                    , br [] []
                    , div [] [ text "Flyet lander 26. August kl 08:25." ]
                    , br [] []
                    , div [] [ text "Vi har leid felles buss fra Gardermoen til hotellet som går kl 09:00 fra flyplassen" ]
                    , div [] [ text "Du er selv ansvarlig for å møte opp på riktig sted og tid." ]
                    , br [] []
                    , div [] [ text "Det er IKKE felles hjemreise, og kostnadene for dette må du dekke selv." ]
                    , br [] []
                    , div [] [ text "Under hele oppholdet bor alle på Hotell Bondeheimen, Rosenkrantz' gate 8, 0159 Oslo." ]
                    , div [] [ text "Det vil bo 2-5 personer på rom sammen. Romfordeling vil bli bestemt på et senere tidspunkt." ]
                    , div [] [ text "Utsjekking er 29. august kl. 12:00." ]
                    , br [] []
                    , h1 [] [ text "Regler" ]
                    , br [] []
                    , ul []
                        [ li [] [ text "Du er personlig ansvarlig for å rekke flyet, og konsekvensen hvis du ikke møter opp er en bot på 5 000 NOK." ]
                        , br [] []
                        , li [] [ text "Hvis du ikke møter opp eller møter opp i beruset tilstand til et bedriftsbesøk vil du bli fakturert bot på 1 500 NOK." ]
                        , br [] []
                        , li [] [ text "Du er personlig ansvarlig for hærverk eller andre ekstra kostnader du påfører." ]
                        , br [] []
                        , li [] [ text "Du er personlig ansvarlig for dine eiendeler." ]
                        ]
                    ]
                    -}
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
