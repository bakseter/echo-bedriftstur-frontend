module Page.Program exposing (init, subscriptions, update, view, Model, Msg)

import Html exposing (Html, div, h1, h3, text, a, br)
import Html.Attributes exposing (class, id, target, rel, href)
import Html.Events
import Browser.Navigation as Nav

type Msg
    = MnemonicMap
    | ComputasMap
    | TbaMap
    | KnowitMap
    | DnbMap
    | BekkMap

type alias Model = 
    Html Msg

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
        TbaMap ->
            (model, Cmd.none)
        KnowitMap ->
            (model, Nav.load "https://goo.gl/maps/WtChazwnMMmPCFBv7")
        DnbMap ->
            (model, Nav.load "https://goo.gl/maps/4v6ZngBfBdVCiYDKA")
        BekkMap ->
            (model, Nav.load "https://goo.gl/maps/LYnPouRhiv56hFWf7")

view : Model -> Html Msg
view model =
    div [ class "program" ]
        [ div [ id "onsdag" ]
            [ h1 [] [ text "Onsdag" ]
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
        , div [ id "torsdag" ]
            [ h1 [] [ text "Torsdag" ]
            , div [ class "program-item", Html.Events.onClick TbaMap ]
                [ div [ class "program-tab", id "tba-tab" ] [ br [] [] ]
                , div [ class "program-content", id "tba-content" ]
                    [ div [ class "program-text" ]
                        [ h1 [] [ text "To be announced" ]
                        , h3 [] [ text "11:00 - 15:00" ]
                        , h3 [] [ text "" ] 
                        , br [] []
                        , br [] []
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
        , div [ id "fredag" ] 
            [ h1 [] [ text "Fredag" ]
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
