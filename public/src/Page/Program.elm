module Page.Program exposing (init, subscriptions, update, view, Model, Msg)

import Html exposing (Html, div, h1, h3, text, a, br)
import Html.Attributes exposing (class, id, target, rel, href)

type Msg
    = None

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
    (model, Cmd.none)

view : Model -> Html Msg
view model =
    div [ class "program" ]
        [ div [ id "onsdag" ]
            [ h1 [] [ text "Onsdag" ]
            , div [ class "program-item" ]
                [ div [ class "program-tab", id "mnemonic-tab" ] [ br [] [] ]
                , div [ class "program-content", id "mnemonic-content" ]
                    [ div [ class "program-text" ]
                        [ h1 [] [ text "mnemonic" ]
                        , h3 [] [ text "11:00 - 15:00" ]
                        , h3 [] [ a [ target "_blank", rel "noopener noreferrer", href "" ] [ text "Henrik Ibsens gate 100" ] ]
                        ]
                    ]
                ]
            , div [ class "program-item" ]
                [ div [ class "program-tab", id "computas-tab" ] [ br [] [] ]
                , div [ class "program-content", id "computas-content" ]
                    [ div [ class "program-text" ]
                        [ h1 [] [ text "Computas" ]
                        , h3 [] [ text "17:00 - 21:00" ]
                        , h3 [] [ a [ target "_blank", rel "noopener noreferrer", href "" ] [ text "Akersgata 35" ] ]
                        ]
                    ]
                ]
            ]
        , div [ id "torsdag" ]
            [ h1 [] [ text "Torsdag" ]
            , div [ class "program-item" ]
                [ div [ class "program-tab", id "tba-tab" ] [ br [] [] ]
                , div [ class "program-content", id "tba-content" ]
                    [ div [ class "program-text" ]
                        [ h1 [] [ text "To be announced" ]
                        , h3 [] [ text "11:00 - 15:00" ]
                        , h3 [] [ a [ target "_blank", rel "noopener noreferrer", href "" ] [ text "" ] ] 
                        , br [] []
                        , br [] []
                        ]
                    ]
                ]
            , div [ class "program-item" ]
                [ div [ class "program-tab", id "knowit-tab" ] [ br [] [] ]
                , div [ class "program-content", id "knowit-content" ]
                    [ div [ class "program-text" ]
                        [ h1 [] [ text "Knowit" ]
                        , h3 [] [ text "17:00 - 21:00" ]
                        , h3 [] [  a [ target "_blank", rel "noopener noreferrer", href "" ] [ text "Lakkegata 53" ] ] 
                        ]
                    ]
                ]
            ]
        , div [ id "fredag" ] 
            [ h1 [] [ text "Fredag" ]
            , div [ class "program-item" ]
                [ div [ class "program-tab", id "dnb-tab" ] [ br [] [] ]
                , div [ class "program-content", id "dnb-content" ]
                    [ div [ class "program-text" ]
                        [ h1 [] [ text "DNB" ] 
                        , h3 [] [ text "11:00 - 15:00" ]
                        , h3 [] [ a [ target "_blank", rel "noopener noreferrer", href "" ] [ text "Dronning Eufemias gate 30" ] ] 
                        ]
                    ]
                ]
            , div [ class "program-item" ]
                [ div [ class "program-tab", id "bekk-tab" ] [ br [] [] ]
                , div [ class "program-content", id "bekk-content" ]
                    [ div [ class "program-text" ]
                        [ h1 [] [ text "Bekk" ]
                        , h3 [] [ text "17:00 - 21:00" ]
                        , h3 [] [ a [ target "_blank", rel "noopener noreferrer", href "" ] [ text "Skur 39 Akershusstranda 21" ] ] 
                        ]
                    ]
                ]
            ]
        ]
