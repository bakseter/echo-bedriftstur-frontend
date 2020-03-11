module Page.Bedrifter exposing (init, subscriptions, update, view, Model, Msg, route)

import Html exposing (Html, div, span, text, a, img, h1, h2, p)
import Html.Attributes exposing (class, id, target, rel, href, src, alt)
import Time
import Browser.Dom
import Task
import Animation exposing (percent)

type Msg
    = GotViewport Browser.Dom.Viewport
    | Tick Time.Posix

type alias Model = 
    { viewport : Browser.Dom.Viewport
    , mnemonicSlidIn : Bool
    , computasSlidIn : Bool
    , ciscoSlidIn : Bool
    , knowitSlidIn : Bool
    , dnbSlidIn : Bool
    , bekkSlidIn : Bool
    }

route : String
route =
    "bedrifter"

init : Model
init =
    { viewport = maybeViewport
    , mnemonicSlidIn = False
    , computasSlidIn = False
    , ciscoSlidIn = False
    , knowitSlidIn = False
    , dnbSlidIn = False
    , bekkSlidIn = False
    }

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ if allAreSlidIn model then 
            Sub.none
          else
            Time.every 10 Tick
        ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GotViewport newViewport ->
            let scroll = percentageOfScreenScrolled model
                newModel = { model | viewport = newViewport}
                deviceWidth = newViewport.scene.width
            in
                if deviceWidth < 900 then
                   ({ model | mnemonicSlidIn = True
                            , computasSlidIn = True
                            , ciscoSlidIn = True
                            , knowitSlidIn = True
                            , dnbSlidIn = True
                            , bekkSlidIn = True
                   }, Cmd.none)
                else
                    if scroll > 0.1 && not model.mnemonicSlidIn then
                        ({ model | mnemonicSlidIn = True }, Cmd.none)
                    else if scroll > 0.3 && not model.computasSlidIn then
                        ({ model | computasSlidIn = True }, Cmd.none)
                    else if scroll > 0.43 && not model.ciscoSlidIn then
                        ({ model | ciscoSlidIn = True }, Cmd.none)
                    else if scroll > 0.56 && not model.knowitSlidIn then
                        ({ model | knowitSlidIn = True }, Cmd.none)
                    else if scroll > 0.69 && not model.dnbSlidIn then
                        ({ model | dnbSlidIn = True }, Cmd.none)
                    else if scroll > 0.79 && not model.bekkSlidIn then
                        ({ model | bekkSlidIn = True }, Cmd.none)
                    else
                        (newModel, Cmd.none)
        Tick time ->
            (model, Task.perform GotViewport Browser.Dom.getViewport)

view : Model -> Html Msg
view model =
    div [ class "bedrifter" ]
        [ div [ id "bedrifter-content" ] 
            [ span [ (getClass model.mnemonicSlidIn True), id "mnemonic" ]
                [ a [ target "_blank", rel "noopener noreferrer", href "https://www.mnemonic.no" ]
                    [ img  [ class "bed-logo", src "/img/mnemonic.png", alt "mnemonic" ] [] ]
                , div [ class "bed-text" ]
                    [ p []
                        [ text "mnemonic hjelper virksomheter med å administrere og håndtere sine sikkerhetsrisikoer, beskytte sine data og forsvare seg mot trusler fra Internett." ]
                    , p []
                        [ text "Vårt ekspertteam av sikkerhetskonsulenter, produktspesialister, trusseletterforskere, team av hendelseshåndterere og etiske hackere, kombinert med vår Argus sikkerhetsplattform sikrer at vi ligger i forkant av avanserte angrep fra Internett og beskytter våre kunder fra nye trusler." ]
                    ]
                ]
            , span [ (getClass model.computasSlidIn False), id "computas" ]
                [ a [ target "_blank", rel "noopener noreferrer", href "https://computas.com" ]
                    [ img  [ class "bed-logo", src "/img/computas.png", alt "Computas" ] [] ]
                , div [ class "bed-text" ]
                    [ p []
                        [ text "Computas er en norsk leverandør av IT-løsninger og rådgivningstjenester innen teknologisk innovasjon. Vi leverer verdiskapende og samfunnsnyttige løsninger til både offentlig og privat sektor, og har særlig spisskompetanse innenfor offentlig forvaltning, justis, tilsyn, helse, logistikk, olje og gass. Vi jobber med alt fra apper som redder liv og prisvinnende saksbehandlingsløsninger, til dataanalyse, kunstig intelligens og omfattende, skybaserte innovasjonsprosjekter." ]
                    ]
                ]
            , span [ (getClass model.ciscoSlidIn True), id "cisco" ]
                [ a [ target "_blank", rel "noopener noreferrer", href "https://www.cisco.no" ]
                    [ img [ class "bed-logo", src "/img/cisco.png", alt "Cisco" ] [] ]
                , div [ class "bed-text" ]
                    [ p []
                        [ text "Cisco er et verdensomspennende teknologiselskap som er markedsleder innen nettverk, datasenter, sikkerhet og samhandlingsløsninger. På Lysaker er vi rundt 500 ansatte som jobber med alt fra R&D, markedsføring og salg, supply chain og ledelse. Alt som kalles Webex Devices utvikles hos oss, fra idé til design, maskinvare og mekanikk, programvare, skyløsninger, maskinlæring og intelligens. Vi har et av de største in-house design teamene i Norge og har vunnet prestisjetunge awards som iF Gold og Red Dot Best of the Best. Det er mange som vil jobbe med teknologi - hos oss får du lage den!" ]
                    ]
                ]
            , span [ (getClass model.knowitSlidIn False), id "knowit" ]
                [ a [ target "_blank", rel "noopener noreferrer", href "https://www.knowit.no" ]
                    [ img  [ class "bed-logo", src "/img/knowit.png", alt "Knowit" ] [] ]
                , div [ class "bed-text" ]
                    [ p []
                        [ text "Knowit er et konsulentselskap som, i den stadig raskere digitaliseringen, skaper unike kundeverdier gjennom å tilby grenseoverskridende leveranser fra de tre forretningsområdene Experience, Insight og Solutions. Det er evnen til å kombinere kompetanse innen design og kommunikasjon, management consulting og IT som skiller oss fra andre konsulentfirmaer. Vår kultur preges av åpenhet, forståelse for kundens forretninger, høy spesialistkompetanse og en vilje til å utvikles kontinuerlig." ]
                    ]
                ]
            , span [ (getClass model.dnbSlidIn True), id "dnb" ]
                [ a [ target "_blank", rel "noopener noreferrer", href "https://www.dnb.no" ]
                    [ img  [ id "dnb-logo", class "bed-logo", src "/img/dnb.png", alt "DNB" ] [] ]
                , div [ class "bed-text" ]
                    [ p []
                        [ text "DNB er mer enn bare en bank. Vår ambisjon er å være et av Europas ledende teknologiselskaper. Bank- og finansbransjen gjennomgår en enorm forandring, og kundeadferden endrer seg raskt. En kritisk del av denne transformasjonen er å etablere raskere og mer effektive måter å jobbe på." ]
                    , p []
                        [ text "Ved hjelp av metoder som \"design thinking\" og \"lean startup\" skaper vi tjenester som gir de best kundeopplevelsene. Vi jobber hele tiden med å bygge en kultur av gjensidig respekt, læring og åpenhet mot våre kunder og samfunn. Vi investerer i våre folk, fordi det er de som driver forandringen." ]
                    ]
                ]
            , span [ (getClass model.bekkSlidIn False), id "bekk" ]
                [ a [ target "_blank", rel "noopener noreferrer", href "https://www.bekk.no" ]
                    [ img  [ class "bed-logo", src "/img/bekk.png", alt "Bekk" ] [] ]
                , div [ class "bed-text" ]
                    [ p [] 
                        [ text " I Bekk er vi flinke til å bygge hverandre opp, utfordre hverandre og, ikke minst, de vi jobber med. Vi motiveres av å drive utviklingen fremover og heve standarden for hva som anses for å være godt levert. Vi inspireres av å spre kunnskap om hva som virker, og hvorfor." ]
                    , p []
                        [ text "Vi har ingen tro på strømlinjeformede arbeidsplasser. Kreativ frihet og muligheten til å påvirke egen arbeidsdag, sette dagsorden og styre utviklingen av selskapet, det er mener vi viktigere enn å følge opptråkkede stier. Initiativ og engasjement er hardkodet i ryggraden vår og har skapt en sterk og inkluderende kultur." ]
                    ]
                ]
            ]
        ]

maybeViewport : Browser.Dom.Viewport
maybeViewport =
        { scene =
            { width = 0
            , height = 0
            }
        , viewport =
            { x = 0
            , y = 0
            , width = 0
            , height = 0
            }
        }

percentageOfScreenScrolled : Model -> Float
percentageOfScreenScrolled model =
    let yPos = model.viewport.viewport.y
        screenHeight = model.viewport.scene.height
        viewportHeight = model.viewport.viewport.height
    in (yPos + (viewportHeight / 2)) / screenHeight

getClass : Bool -> Bool -> Html.Attribute Msg
getClass slide fromLeft =
    if slide then
        class "logo-item"
    else if fromLeft then
        class "logo-item-hidden-left"
    else
        class "logo-item-hidden-right"

allAreSlidIn : Model -> Bool
allAreSlidIn model =
    model.mnemonicSlidIn &&
    model.computasSlidIn &&
    model.ciscoSlidIn &&
    model.knowitSlidIn &&
    model.dnbSlidIn &&
    model.bekkSlidIn
