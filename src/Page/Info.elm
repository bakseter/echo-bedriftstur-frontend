module Page.Info exposing (init, subscriptions, update, view, Model, Msg, route)

import Html exposing (Html, div, span, h1, br, li, ul, text, a)
import Html.Attributes exposing (class, id, src, href, rel, target)

type Msg =
    None

type alias Model =
    Html Msg

route : String
route =
    "info"

init : Model
init =
    div [] []

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    (model, Cmd.none)

view : Model -> Html Msg
view model =
        div [ class "info" ]
            [ div [ class "info-content" ]
                [ div [ class "text" ]
                    [ h1 [] [ text "Påmelding" ]
                    , br [] []
                    , br [] []
                    , div [] [ text "Registrering åpner 22. april kl. 12:00. Da kan du registrere deg med din studentmail, og fylle inn din kontaktinformasjon. Dette gjør det lettere og raskere for deg å melde deg på når påmeldingen kommer ut, fordi all informasjonen din allerede er fylt inn." ]
                    , br [] []
                    , div [] [ text "Påmelding åpner 29. april kl. 12:00. Her gjelder \"førstemann-til-mølla\"-prinsippet." ]
                    , div [] [ text "Det vil være 20 plasser til 3. klassinger og 27 plasser til 4. klassinger (altså de som er 3. og 4. klassinger høsten 2020)." ]
                    , div [] [ text "Både registrering og påmelding vil foregå på denne nettsiden." ]
                    , br [] []
                    , div [] [ text "For å melde deg på bedriftsturen må du:" ]
                    , ul []
                        [ li [] [ text "være påmeldt et bachelorprogram og begynne ditt 5. semester august 2020 eller være påmeldt et masterprogram og begynne ditt 1. eller 2. semester august 2020" ]
                        , li [] [ text "være representert av echo, ifølge echo sine ", 
                                    a [ class "inline-link", target "_blank", rel "noopener noreferrer", href "https://echo.uib.no/om/statutter" ] [ text "statutter" ]
                                    , text " per 22. april 2020" 
                                ]
                        , li [] [ text "følge normert studieløp" ]
                        ]
                    , div [ class "inline-text" ] [ text "Har du et spesielt studieløp som ikke faller under disse kriteriene, kan du kontakte oss på " ]
                    , a [ class "inline-link", href "mailto:kontakt@echobedriftstur.no" ] [ text "kontakt@echobedriftstur.no" ]
                    , div [ class "inline-text" ] [ text ", så vil vi evaluere om du kan melde deg på turen." ]
                    , br [] []
                    , br [] []
                    , h1 [] [ text "Transport & Hotell" ]
                    , br [] []
                    , br [] []
                    , div [] [ text "Vi flyr med Norwegian fra Bergen lufthavn 26. August kl. 07:30." ]
                    , div [] [ text "Det anmodes å møte opp i god tid, senest kl. 06:30." ]
                    , div [] [ text "Du er selv ansvarlig for å rekke flyet." ]
                    , br [] []
                    , div [] [ text "Inkludert i billetten er opptil to kolli innsjekket bagasje per person (maks 20 kg per kolli), i tillegg til én håndbagasje (10 kg)." ]
                    , br [] []
                    , div [] [ text "Flyet lander 26. August kl 08:25." ]
                    , br [] []
                    , div [] [ text "Vi har leid felles buss fra Gardermoen til hotellet som går kl 09:00 fra flyplassen." ]
                    , div [] [ text "Du er selv ansvarlig for å møte opp på riktig sted og tid." ]
                    , br [] []
                    , div [] [ text "Det er IKKE felles hjemreise, og kostnadene for dette må du dekke selv." ]
                    , br [] []
                    , div [] [ text "Under hele oppholdet bor alle på Hotell Bondeheimen, Rosenkrantz' gate 8, 0159 Oslo." ]
                    , div [] [ text "Det vil bo 2-5 personer på rom sammen. Romfordeling vil bli bestemt på et senere tidspunkt." ]
                    , div [] [ text "Utsjekking er 29. august kl. 12:00." ]
                    , br [] []
                    , h1 [] [ text "COVID-19" ]
                    , br [] []
                    , br [] []
                    , div [] 
                    [ text 
                        """
                        Vi i bedriftsturkomitéen følger nøye med på situasjonen rundt Koronaviruset, og vi vil følge alle retningslinjer fra regjeringen angående store arrangementer.
                        Slik det ser ut nå, velger vi å gå fremover med påmeldingsprosessen, og den eventuelle turen til høsten.
                        """
                     ]
                    , br [] []
                    , div [] 
                    [ text
                        """
                        Hvis situasjonen endrer seg fremover, og det ikke lenger er forsvarlig å arrangere en slik tur, vil vi enten finne en alternativ løsning, eller i verste fall avlyse turen.
                        Vi har vært i dialog med en rekke andre linjeforeninger i Norge som arrangerer samme type turer, og de ser heller ingen grunn til å avlyse per nå.
                        """
                    ]
                    , br [] []
                    , h1 [] [ text "Regler" ]
                    , br [] []
                    , ul []
                        [ li [] [ text "Hvis du ikke møter opp, eller møter opp i beruset tilstand til et bedriftsbesøk, vil du bli fakturert 1 500 NOK per besøk du bryter denne regelen på." ]
                        , br [] []
                        , li [] [ text "Du er personlig ansvarlig for hærverk eller andre ekstra kostnader du påfører." ]
                        , br [] []
                        , li [] [ text "Du er personlig ansvarlig for dine eiendeler." ]
                        ]
                    ]
                ]
            ]
