module Page.Bedrifter exposing (init, subscriptions, update, view, Model, Msg)

import Html exposing (Html, div, span, text, a, img, h1, h2, p)
import Html.Attributes exposing (class, id, target, rel, href, src, alt)
import Time
import Browser.Dom
import String
import Task
import Animation exposing (percent)

type Msg
    = GotViewport Browser.Dom.Viewport
    | Tick Time.Posix
    | SlideInBekk
    | AnimBekk Animation.Msg
    | SlideInMnemonic
    | AnimMnemonic Animation.Msg
    | SlideInDnb
    | AnimDnb Animation.Msg
    | SlideInComputas
    | AnimComputas Animation.Msg
    | SlideInKnowit
    | AnimKnowit Animation.Msg
    | SlideInTba
    | AnimTba Animation.Msg

type alias Model = 
    { viewport : Browser.Dom.Viewport
    , bekkAnim : Animation.State
    , mnemonicAnim : Animation.State
    , dnbAnim : Animation.State
    , computasAnim : Animation.State 
    , knowitAnim : Animation.State 
    , tbaAnim : Animation.State
    }

init : Model
init =
    { viewport = maybeViewport
    , bekkAnim = Animation.style [ Animation.opacity 0.0
                                 , Animation.left (percent -50)
                                 ]
    , mnemonicAnim = Animation.style [ Animation.opacity 0.0
                                     , Animation.left (percent -50)
                                     ] 
    , dnbAnim = Animation.style [ Animation.opacity 0.0
                                , Animation.left (percent -50)
                                ]
    , computasAnim = Animation.style [ Animation.opacity 0.0
                                     , Animation.left (percent -50)
                                     ]
    , knowitAnim = Animation.style [ Animation.opacity 0.0
                                   , Animation.left (percent -50)
                                   ]
    , tbaAnim = Animation.style [ Animation.opacity 0.0
                                , Animation.left (percent -50)
                                ]
    }

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 100 Tick
        , Animation.subscription AnimBekk [ model.bekkAnim ]
        , Animation.subscription AnimMnemonic [ model.mnemonicAnim ]
        , Animation.subscription AnimDnb [ model.dnbAnim ]
        , Animation.subscription AnimComputas [ model.computasAnim ]
        , Animation.subscription AnimKnowit [ model.knowitAnim ]
        ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GotViewport newViewport ->
            let scroll = percentageOfScreenScrolled model
                newModel = { model | viewport = newViewport}
            in
                if scroll > 0.1 && scroll < 0.3 then
                    update SlideInBekk newModel
                else if scroll > 0.3 && scroll < 0.5 then
                    update SlideInMnemonic newModel
                else if scroll > 0.5 && scroll < 0.7 then
                    update SlideInDnb newModel
                else if scroll > 0.7 && scroll < 0.8 then
                    update SlideInComputas newModel
                else if scroll > 0.8 && scroll < 0.9 then
                    update SlideInKnowit newModel
                else if scroll > 0.9 then
                    update SlideInTba newModel
                else
                    (newModel, Cmd.none)
        Tick time ->
            (model, Task.perform GotViewport Browser.Dom.getViewport)
        SlideInBekk ->
            let newStyle = Animation.interrupt 
                            [ Animation.toWith ease
                                [ Animation.opacity 1.0
                                , Animation.left (percent 40) 
                                ] 
                            ] model.bekkAnim
            in ({ model | bekkAnim = newStyle }, Cmd.none)
        SlideInMnemonic ->
            let newStyle = Animation.interrupt
                            [ Animation.toWith ease
                                [ Animation.opacity 1.0
                                , Animation.left (percent 40) 
                                ]
                            ] model.mnemonicAnim
            in ({ model | mnemonicAnim = newStyle }, Cmd.none)
        SlideInDnb ->
            let newStyle = Animation.interrupt
                            [ Animation.toWith ease
                                [ Animation.opacity 1.0
                                , Animation.left (percent 40)
                                ]
                            ] model.dnbAnim
            in ({ model | dnbAnim = newStyle }, Cmd.none)
        SlideInComputas ->
            let newStyle = Animation.interrupt
                            [ Animation.toWith ease
                                [ Animation.opacity 1.0
                                , Animation.left (percent 40)
                                ]
                            ] model.computasAnim
            in ({ model | computasAnim = newStyle }, Cmd.none)
        SlideInKnowit ->
            let newStyle = Animation.interrupt
                            [ Animation.toWith ease
                                [ Animation.opacity 1.0
                                , Animation.left (percent 40)
                                ]
                            ] model.knowitAnim
            in ({ model | knowitAnim = newStyle }, Cmd.none)
        SlideInTba ->
            let newStyle = Animation.interrupt
                            [ Animation.toWith ease
                                [ Animation.opacity 1.0
                                , Animation.left (percent 40)
                                ]
                            ] model.tbaAnim
            in ({ model | tbaAnim = newStyle }, Cmd.none)
        AnimBekk anim ->
            ({ model | bekkAnim = Animation.update anim model.bekkAnim }, Cmd.none)
        AnimMnemonic anim ->
            ({ model | mnemonicAnim = Animation.update anim model.mnemonicAnim }, Cmd.none)
        AnimDnb anim ->
            ({ model | dnbAnim = Animation.update anim model.dnbAnim }, Cmd.none)
        AnimComputas anim ->
            ({ model | computasAnim = Animation.update anim model.computasAnim }, Cmd.none)
        AnimKnowit anim ->
            ({ model | knowitAnim = Animation.update anim model.knowitAnim }, Cmd.none)
        AnimTba anim ->
            ({ model | tbaAnim = Animation.update anim model.tbaAnim }, Cmd.none)

view : Model -> Html Msg
view model =
    div [ class "bedrifter" ]
    --  [ h2 [id "viewport" ] [ text (String.fromFloat (percentageOfScreenScrolled model)) ]
        [ div [ id "bedrifter-content" ] 
            [ span (Animation.render model.bekkAnim ++ [ class "logo-item", id "bekk" ])
                [ a [ target "_blank", rel "noopener noreferrer", href "https://www.bekk.no" ]
                    [ img  [ class "bed-logo", src "/img/bekk.png", alt "Bekk" ] [] ]
                , p [ class "bed-text" ] [ text " I Bekk er vi flinke til å bygge hverandre opp, utfordre hverandre og, ikke minst, de vi jobber med. Vi motiveres av å drive utviklingen fremover og heve standarden for hva som anses for å være godt levert. Vi inspireres av å spre kunnskap om hva som virker, og hvorfor." ]
                , p [ class "bed-text" ] [ text "Vi har ingen tro på strømlinjeformede arbeidsplasser. Kreativ frihet og muligheten til å påvirke egen arbeidsdag, sette dagsorden og styre utviklingen av selskapet, det er mener vi viktigere enn å følge opptråkkede stier. Initiativ og engasjement er hardkodet i ryggraden vår og har skapt en sterk og inkluderende kultur." ]
                ]
            , span (Animation.render model.mnemonicAnim ++ [ class "logo-item", id "mnemonic" ])
                [ a [ target "_blank", rel "noopener noreferrer", href "https://www.mnemonic.no" ]
                    [ img  [ class "bed-logo", src "/img/mnemonic.png", alt "mnemonic" ] [] ]
                , p [ class "bed-text" ] [ text "mnemonic hjelper virksomheter med å administrere og håndtere sine sikkerhetsrisikoer, beskytte sine data og forsvare seg mot trusler fra Internett. " ]
                , p [ class "bed-text" ] [ text "Vårt ekspertteam av sikkerhetskonsulenter, produktspesialister, trusseletterforskere, team av hendelseshåndterere og etiske hackere, kombinert med vår Argus sikkerhetsplattform sikrer at vi ligger i forkant av avanserte angrep fra Internett og beskytter våre kunder fra nye trusler." ]
                ]
            , span (Animation.render model.dnbAnim ++ [ class "logo-item", id "DNB" ])
                [ a [ target "_blank", rel "noopener noreferrer", href "https://www.dnb.no" ]
                    [ img  [ id "dnb-logo", class "bed-logo", src "/img/dnb.png", alt "DNB" ] [] ]
                , p [ class "bed-text" ] [ text "DNB er mer enn bare en bank. Vår ambisjon er å være et av Europas ledende teknologiselskaper. Bank- og finansbransjen gjennomgår en enorm forandring, og kundeadferden endrer seg raskt. En kritisk del av denne transformasjonen er å etablere raskere og mer effektive måter å jobbe på." ]
                , p [ class "bed-text" ] [ text "Ved hjelp av metoder som \"design thinking\" og \"lean startup\" skaper vi tjenester som gir de best kundeopplevelsene. Vi jobber hele tiden med å bygge en kultur av gjensidig respekt, læring og åpenhet mot våre kunder og samfunn. Vi investerer i våre folk, fordi det er de som driver forandringen." ]
                ]
            , span (Animation.render model.computasAnim ++ [ class "logo-item", id "computas" ])
                [ a [ target "_blank", rel "noopener noreferrer", href "https://computas.com" ]
                    [ img  [ class "bed-logo", src "/img/computas.png", alt "Computas" ] [] ]
                , p [ class "bed-text" ] [ text "Computas er en norsk leverandør av IT-løsninger og rådgivningstjenester innen teknologisk innovasjon. Vi leverer verdiskapende og samfunnsnyttige løsninger til både offentlig og privat sektor, og har særlig spisskompetanse innenfor offentlig forvaltning, justis, tilsyn, helse, logistikk, olje og gass. Vi jobber med alt fra apper som redder liv og prisvinnende saksbehandlingsløsninger, til dataanalyse, kunstig intelligens og omfattende, skybaserte innovasjonsprosjekter." ]
                ]
            , span (Animation.render model.knowitAnim ++ [ class "logo-item", id "knowit" ])
                [ a [ target "_blank", rel "noopener noreferrer", href "https://www.knowit.no" ]
                    [ img  [ class "bed-logo", src "/img/knowit.png", alt "Knowit" ] [] ]
                , p [ class "bed-text" ] [ text "Knowit er et konsulentselskap som, i den stadig raskere digitaliseringen, skaper unike kundeverdier gjennom å tilby grenseoverskridende leveranser fra de tre forretningsområdene Experience, Insight og Solutions. Det er evnen til å kombinere kompetanse innen design og kommunikasjon, management consulting og IT som skiller oss fra andre konsulentfirmaer. Vår kultur preges av åpenhet, forståelse for kundens forretninger, høy spesialistkompetanse og en vilje til å utvikles kontinuerlig." ]
                ]
            , span (Animation.render model.tbaAnim ++ [ class "logo-item" ])
                [ a [ target "_blank", rel "noopener noreferrer", href "" ]
                    [ h1 [] [ text "To be announced" ] ]
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

ease : Animation.Interpolation
ease =
    Animation.spring
        { stiffness = 80
        , damping = 20 }
