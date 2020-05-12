module Page.Info exposing (Model, Msg, init, route, subscriptions, update, view)

import Html exposing (Html, a, br, div, h1, li, span, text, ul)
import Html.Attributes exposing (class, download, href, id, rel, src, style, target)


type Msg
    = None


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "info" ]
        [ div [ class "info-content" ]
            [ div [ class "text" ]
                [ h1 [] [ text "Hvordan bli med" ]
                , br [] []
                , br [] []
                , span [] [ text "Registrering åpner " ]
                , span [ class "text-underline" ] [ text "28. april kl. 12:00" ]
                , span []
                    [ text
                        """
                        . Da kan du registrere deg med din studentmail, og fylle inn din kontaktinformasjon.
                        Dette gjør det lettere og raskere for deg å melde deg på når påmeldingen kommer ut,
                        fordi all informasjonen din allerede er fylt inn.
                        """
                    ]
                , br [] []
                , br [] []
                , span [] [ text "Påmelding åpner " ]
                , span [ class "text-underline" ] [ text "5. mai kl. 12:00" ]
                , span [] [ text ". Her gjelder \"førstemann-til-mølla\"-prinsippet." ]
                , br [] []
                , span [] [ text "Det vil være 20 plasser til 3. klassinger og 27 plasser til 4. klassinger " ]
                , span [ class "text-bold" ] [ text "(altså de som er 3. og 4. klassinger høsten 2020)." ]
                , div [] [ text "Både registrering og påmelding vil foregå på denne nettsiden." ]
                , br [] []
                , div [] [ text "Du vil ikke få svar på om du har fått plass med en gang. Vi vil sende deg en mail når alle har fått plass." ]
                , span [] [ text "Dersom du ønsker å melde deg av turen, må du sende oss en mail på " ]
                , a [ class "text-underline", href "mailto:kontakt@echobedriftstur.no" ] [ text "kontakt@echobedriftstur.no" ]
                , span [] [ text " innen " ]
                , span [ class "text-underline" ] [ text "1. august 2020" ]
                , span [] [ text "." ]
                , br [] []
                , div []
                    [ text
                        """
                        Det er ikke mulig å melde seg av etter denne fristen. Dersom du likevel ikke møter opp etter
                        å ha prøvd å melde deg av etter fristen, gjelder regelen for å ikke møte opp på et bedriftsbesøk
                        (beskrevet under).
                        """
                    ]
                , br [] []
                , div [] [ text "For å melde deg på bedriftsturen må du:" ]
                , ul []
                    [ li []
                        [ text
                            """
                            være påmeldt et bachelorprogram og begynne ditt femte semester august 2020, 
                            ha søkt/være påmeldt et masterprogram og begynne ditt første eller andre semester august 2020,
                            eller være en postbachelorstudent og ta emner ved institutt for informatikk eller institutt for matematikk høsten 2020.
                            """
                        ]
                    , li []
                        [ text "være representert av echo – Fagutvalget for informatikk, ifølge echo sine "
                        , a [ class "text-underline", href "/docs/statutter_30_04_2020.pdf", download "statutter_30_04_2020.pdf" ]
                            [ text "statutter" ]
                        , text " per 30. april 2020"
                        ]
                    , li []
                        [ span [ class "text" ] [ text "følge " ]
                        , span [ class "text-underline" ] [ text "normert studieløp" ]
                        , span [ class "text-bold" ] [ text " *" ]
                        ]
                    ]
                , br [] []
                , span [ class "text-bold" ] [ text "*" ]
                , span [ class "text-bold" ]
                    [ text
                        """
                        Det vi mener med å følge normert studieløp, er at du tar fagene som ditt studieprogram krever det gjeldende semestret.
                        Du kan ta opp/konte så mange fag du vil ved siden av, så lenge du fortsatt følger studieprogrammet ditt det semesteret.
                        """
                    ]
                , br [] []
                , br [] []
                , div []
                    [ text
                        """
                        Dersom du har søkt master til høsten, trenger vi en bekreftelse på at du har fått plass.
                        Når du registrerer deg, fyller du bare inn det masterprogrammet du har søkt på.
                        Nærmere informasjon om dette vil komme på mail til de det gjelder.
                        """
                    ]
                , br [] []
                , span [] [ text "Har du et spesielt studieløp som ikke faller under disse kriteriene, kan du kontakte oss på " ]
                , a [ class "text-underline", href "mailto:kontakt@echobedriftstur.no" ] [ text "kontakt@echobedriftstur.no" ]
                , span [] [ text ", så vil vi evaluere om du kan melde deg på turen." ]
                , br [] []
                , br [] []
                , h1 [] [ text "Transport og hotell" ]
                , br [] []
                , br [] []
                , div [] [ text "Vi flyr med Norwegian fra Bergen lufthavn 26. august kl. 07:30." ]
                , div [] [ text "Det anmodes å møte opp i god tid, senest kl. 06:30." ]
                , div [] [ text "Du er selv ansvarlig for å rekke flyet." ]
                , br [] []
                , div [] [ text "Inkludert i billetten er opptil to kolli innsjekket bagasje per person (maks 20 kg per kolli), i tillegg til én håndbagasje (10 kg)." ]
                , br [] []
                , div [] [ text "Flyet lander 26. august kl 08:25." ]
                , br [] []
                , div [] [ text "Vi har leid felles buss fra Gardermoen til hotellet som går kl 09:00 fra flyplassen." ]
                , div [] [ text "Du er selv ansvarlig for å møte opp på riktig sted og tid." ]
                , br [] []
                , span [] [ text "Det er " ]
                , span [ class "text-underline" ] [ text "IKKE" ]
                , span [] [ text " felles hjemreise, og kostnadene for dette må du dekke selv." ]
                , br [] []
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
                        Vi er i kontinuerlig dialog med andre linjeforeninger i Norge som også arrangerer samme type turer, og vi er sammen enige i at vi ikke trenger å avlyse enda.
                        """
                    ]
                , br [] []
                , h1 [] [ text "Regler" ]
                , br [] []
                , ul []
                    [ li [] [ text "Dersom du ikke møter opp på et bedriftsbesøk, vil du bli fakturert 1 500 NOK per besøk du bryter denne regelen på." ]
                    , br [] []
                    , li [] [ text "Dersom du ikke melder deg av turen i tide, vil du bli utestengt fra neste års tur." ]
                    , br [] []
                    , li [] [ text "Du er personlig ansvarlig for hærverk eller andre ekstra kostnader du påfører." ]
                    , br [] []
                    , li [] [ text "Du er personlig ansvarlig for dine egne eiendeler." ]
                    ]
                ]
            ]
        ]
