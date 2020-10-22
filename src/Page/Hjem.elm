module Page.Hjem exposing (Model, init, title, toSession, view)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class, id)
import Session exposing (Session)
import Svg
import Svg.Attributes


type Model
    = Model Session


init : Session -> Model
init =
    Model


view : Html msg
view =
    div [ class "hjem" ]
        [ div [ id "hjem-header" ]
            [ div [] [ text "Bedriftstur til Oslo" ]
            , div [] [ text "for informatikkstudenter ved UiB" ]
            ]
        , div [ id "hjem-text" ]
            [ div [] [ text "Er du nysgjerrig på jobbmulighetene i Oslo etter endt utdanning?" ]
            , div [] [ text "Bedriftsturkomitéen arrangerer besøk hos seks forskjellige bedrifter til høsten!" ]
            ]
        , div [ id "barcode-anim" ]
            [ Svg.svg [ Svg.Attributes.width "100%", Svg.Attributes.viewBox "0 0 800 350" ]
                [ Svg.rect [ Svg.Attributes.class "barcode-item barcode-item-1", Svg.Attributes.x "0", Svg.Attributes.y "153", Svg.Attributes.width "66", Svg.Attributes.height "207" ] []
                , Svg.rect [ Svg.Attributes.class "barcode-item barcode-item-2", Svg.Attributes.x "91", Svg.Attributes.y "84", Svg.Attributes.width "59", Svg.Attributes.height "276" ] []
                , Svg.rect [ Svg.Attributes.class "barcode-item barcode-item-3", Svg.Attributes.x "150", Svg.Attributes.y "46", Svg.Attributes.width "56", Svg.Attributes.height "314" ] []
                , Svg.rect [ Svg.Attributes.class "barcode-item barcode-item-4", Svg.Attributes.x "238", Svg.Attributes.y "93", Svg.Attributes.width "49", Svg.Attributes.height "367" ] []
                , Svg.rect [ Svg.Attributes.class "barcode-item barcode-item-1", Svg.Attributes.x "346", Svg.Attributes.y "44", Svg.Attributes.width "66", Svg.Attributes.height "316" ] []
                , Svg.rect [ Svg.Attributes.class "barcode-item barcode-item-2", Svg.Attributes.x "425", Svg.Attributes.y "143", Svg.Attributes.width "44", Svg.Attributes.height "217" ] []
                , Svg.rect [ Svg.Attributes.class "barcode-item barcode-item-3", Svg.Attributes.x "469", Svg.Attributes.y "130", Svg.Attributes.width "46", Svg.Attributes.height "230" ] []
                , Svg.rect [ Svg.Attributes.class "barcode-item barcode-item-4", Svg.Attributes.x "542", Svg.Attributes.y "58", Svg.Attributes.width "108", Svg.Attributes.height "302" ] []
                , Svg.rect [ Svg.Attributes.class "barcode-item barcode-item-1", Svg.Attributes.x "660", Svg.Attributes.y "116", Svg.Attributes.width "65", Svg.Attributes.height "244" ] []
                , Svg.rect [ Svg.Attributes.class "barcode-item barcode-item-2", Svg.Attributes.x "725", Svg.Attributes.y "75", Svg.Attributes.width "60", Svg.Attributes.height "285" ] []
                ]
            ]
        ]


title : String
title =
    "echo bedriftstur"


toSession : Model -> Session
toSession (Model session) =
    session
