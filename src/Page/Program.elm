module Page.Program exposing (init, subscriptions, update, view, Model, Msg, route)

import Html exposing (Html, div, h1, h3, text, a, br)
import Html.Attributes exposing (class, id)
import Html.Events
import Browser.Navigation as Nav

type Msg
    = MnemonicMap
    | ComputasMap
    | CiscoMap
    | KnowitMap
    | DnbMap
    | BekkMap

type alias Model = 
    Html Msg

route : String
route =
    "program"

init : Model
init =
    div [] []

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        MnemonicMap ->
            (model, Nav.load "https://goo.gl/maps/qx9MA3UUGHFNRjzz7")
        ComputasMap ->
            (model, Nav.load "https://goo.gl/maps/v8yhucbxmmBGT79h8")
        CiscoMap ->
            (model, Nav.load "https://goo.gl/maps/jMfn2oLGCDRA25rLA")
        KnowitMap ->
            (model, Nav.load "https://goo.gl/maps/WtChazwnMMmPCFBv7")
        DnbMap ->
            (model, Nav.load "https://goo.gl/maps/4v6ZngBfBdVCiYDKA")
        BekkMap ->
            (model, Nav.load "https://goo.gl/maps/LYnPouRhiv56hFWf7")

view : Model -> Html Msg
view model =
    div [ class "program" ]
        [ div [ class "day-item", id "onsdag" ]
            [ h1 [] [ text "Onsdag 26. august" ]
            , div [ class "program-item", Html.Events.onClick MnemonicMap ]
                [ div [ class "program-tab", id "mnemonic-tab" ] [ br [] [] ]
                , div [ class "program-content", id "mnemonic-content" ]
                    [ div [ class "program-text" ]
                        [ h1 [] [ text "mnemonic" ]
                        , h3 [] [ text "11:00 - 15:00" ]
                        , h3 [] [ text "Henrik Ibsens gate 100" ]
                        ]
                    ]
                ]
            , div [ class "program-item", Html.Events.onClick ComputasMap ]
                [ div [ class "program-tab", id "computas-tab" ] [ br [] [] ]
                , div [ class "program-content", id "computas-content" ]
                    [ div [ class "program-text" ]
                        [ h1 [] [ text "Computas" ]
                        , h3 [] [ text "17:00 - 21:00" ]
                        , h3 [] [ text "Akersgata 35" ]
                        ]
                    ]
                ]
            ]
        , div [ class "day-item", id "torsdag" ]
            [ h1 [] [ text "Torsdag 27. august" ]
            , div [ class "program-item", Html.Events.onClick CiscoMap ]
                [ div [ class "program-tab", id "cisco-tab" ] [ br [] [] ]
                , div [ class "program-content", id "cisco-content" ]
                    [ div [ class "program-text" ]
                        [ h1 [] [ text "Cisco" ]
                        , h3 [] [ text "11:00 - 15:00" ]
                        , h3 [] [ text "Philip Pedersens vei 1" ] 
                        ]
                    ]
                ]
            , div [ class "program-item", Html.Events.onClick KnowitMap ]
                [ div [ class "program-tab", id "knowit-tab" ] [ br [] [] ]
                , div [ class "program-content", id "knowit-content" ]
                    [ div [ class "program-text" ]
                        [ h1 [] [ text "Knowit" ]
                        , h3 [] [ text "17:00 - 21:00" ]
                        , h3 [] [ text "Lakkegata 53" ] 
                        ]
                    ]
                ]
            ]
        , div [ class "day-item", id "fredag" ] 
            [ h1 [] [ text "Fredag 28. august" ]
            , div [ class "program-item", Html.Events.onClick DnbMap ]
                [ div [ class "program-tab", id "dnb-tab" ] [ br [] [] ]
                , div [ class "program-content", id "dnb-content" ]
                    [ div [ class "program-text" ]
                        [ h1 [] [ text "DNB" ] 
                        , h3 [] [ text "11:00 - 15:00" ]
                        , h3 [] [ text "Dronning Eufemias gate 30" ] 
                        ]
                    ]
                ]
            , div [ class "program-item", Html.Events.onClick BekkMap ]
                [ div [ class "program-tab", id "bekk-tab" ] [ br [] [] ]
                , div [ class "program-content", id "bekk-content" ]
                    [ div [ class "program-text" ]
                        [ h1 [] [ text "Bekk" ]
                        , h3 [] [ text "17:00 - 21:00" ]
                        , h3 [] [ text "Akershusstranda 21, Skur 39" ]
                        ]
                    ]
                ]
            ]
        ]
