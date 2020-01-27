module Page.Bedrifter exposing (..)

import Html exposing (Html, div, span, text, a, img)
import Html.Attributes exposing (class, id, target, rel, href, src, alt)

type Msg
    = None

type alias Model = 
    Html Msg

init : (Model, Cmd Msg)
init =
    ( div [] []
    , Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    (model, Cmd.none)

view : Model -> Html Msg
view model =
    div [ class "bedrifter" ]
        [ span [ class "logo-item", id "bekk" ]
            [ a [ target "_blank", rel "noopener noreferrer", href "https://www.bekk.no" ]
                [ img  [ class "bed-logo", src "/img/bekk.png", alt "Bekk" ] [] ]
            ]
        , span [ class "logo-item", id "mnemonic" ]
            [ a [ target "_blank", rel "noopener noreferrer", href "https://www.mnemonic.no" ]
                [ img  [ class "bed-logo", src "/img/mnemonic.png", alt "mnemonic" ] [] ]
            ]
        , span [ class "logo-item", id "DNB" ]
            [ a [ target "_blank", rel "noopener noreferrer", href "https://www.dnb.no" ]
                [ img  [ class "bed-logo", src "/img/dnb.png", alt "DNB" ] [] ]
            ]
        , span [ class "logo-item", id "computas" ]
            [ a [ target "_blank", rel "noopener noreferrer", href "https://computas.com" ]
                [ img  [ class "bed-logo", src "/img/computas.png", alt "Computas" ] [] ]
            ]
        , span [ class "logo-item", id "knowit" ]
            [ a [ target "_blank", rel "noopener noreferrer", href "https://www.knowit.no" ]
                [ img  [ class "bed-logo", src "/img/knowit.png", alt "Knowit" ] [] ]
            ]
        , span [ class "logo-item", id "tba" ]
            [ a [ target "_blank", rel "noopener noreferrer", href "" ]
                [ text "To be announced" ]
            ]
        ]

