module Page.Hjem exposing (init, subscriptions, update, view, Model, Msg)

import Html exposing (Html, div, span, h1, text, br, ul, li, i, a)
import Html.Attributes exposing (class, id, href, target, rel, style)
import Svg
import Svg.Attributes

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

view : Model -> Html Msg
view model =
    div [ class "hjem" ]
        [ div [ id "hjem-content" ]
            [ div [ id "hjem-header" ]
                [ div [] [ text "Bedriftstur til Oslo" ]
                , div [] [ text "for informatikkstudenter ved UiB" ]
                ]
            , div [ id "hjem-text" ]
                [ div [] [ text "Er du nysgjerrig på jobbmulighetene i Oslo etter endt utdanning?" ]
                , div [] [ text "Bedriftsturkomitéen arrangerer besøk hos seks forskjellige bedrifter til høsten!" ]
                , div [] [ text "" ]
                ]
            ]
        , div [ id "barcode-anim" ]
            [ Svg.svg [ Svg.Attributes.width "800", Svg.Attributes.height "350" ]
                [ Svg.rect [ Svg.Attributes.class "barcode-item-1", Svg.Attributes.x "0", Svg.Attributes.y "153", Svg.Attributes.width "66", Svg.Attributes.height "207" ]
                    [ Svg.animate [ Svg.Attributes.attributeName "y", Svg.Attributes.values "350; 153", Svg.Attributes.dur "1.5s" ] [] ]
                , Svg.rect [ Svg.Attributes.class "barcode-item-2", Svg.Attributes.x "91", Svg.Attributes.y "84", Svg.Attributes.width "59", Svg.Attributes.height "276" ]
                    [ Svg.animate [ Svg.Attributes.attributeName "y", Svg.Attributes.values "350; 84", Svg.Attributes.dur "1.5s" ] [] ]
                , Svg.rect [ Svg.Attributes.class "barcode-item-3", Svg.Attributes.x "150", Svg.Attributes.y "46", Svg.Attributes.width "56", Svg.Attributes.height "314" ]
                    [ Svg.animate [ Svg.Attributes.attributeName "y", Svg.Attributes.values "350; 46", Svg.Attributes.dur "1.5s" ] [] ]
                , Svg.rect [ Svg.Attributes.class "barcode-item-4", Svg.Attributes.x "238", Svg.Attributes.y "93", Svg.Attributes.width "49", Svg.Attributes.height "367" ] 
                    [ Svg.animate [ Svg.Attributes.attributeName "y", Svg.Attributes.values "350; 93", Svg.Attributes.dur "1.5s" ] [] ]
                , Svg.rect [ Svg.Attributes.class "barcode-item-1", Svg.Attributes.x "346", Svg.Attributes.y "44", Svg.Attributes.width "66", Svg.Attributes.height "316" ]
                    [ Svg.animate [ Svg.Attributes.attributeName "y", Svg.Attributes.values "350; 44", Svg.Attributes.dur "1.5s" ] [] ]
                , Svg.rect [ Svg.Attributes.class "barcode-item-2", Svg.Attributes.x "425", Svg.Attributes.y "143", Svg.Attributes.width "44", Svg.Attributes.height "217" ]
                    [ Svg.animate [ Svg.Attributes.attributeName "y", Svg.Attributes.values "350; 143", Svg.Attributes.dur "1.5s" ] [] ]
                , Svg.rect [ Svg.Attributes.class "barcode-item-3", Svg.Attributes.x "469", Svg.Attributes.y "130", Svg.Attributes.width "46", Svg.Attributes.height "230" ]
                    [ Svg.animate [ Svg.Attributes.attributeName "y", Svg.Attributes.values "350; 130", Svg.Attributes.dur "1.5s" ] [] ]
                , Svg.rect [ Svg.Attributes.class "barcode-item-4", Svg.Attributes.x "542", Svg.Attributes.y "58", Svg.Attributes.width "108", Svg.Attributes.height "302" ]
                    [ Svg.animate [ Svg.Attributes.attributeName "y", Svg.Attributes.values "350; 58", Svg.Attributes.dur "1.5s" ] [] ]
                , Svg.rect [ Svg.Attributes.class "barcode-item-1", Svg.Attributes.x "660", Svg.Attributes.y "116", Svg.Attributes.width "65", Svg.Attributes.height "244" ]
                    [ Svg.animate [ Svg.Attributes.attributeName "y", Svg.Attributes.values "350; 116", Svg.Attributes.dur "1.5s" ] [] ]
                , Svg.rect [ Svg.Attributes.class "barcode-item-2", Svg.Attributes.x "725", Svg.Attributes.y "75", Svg.Attributes.width "60", Svg.Attributes.height "285" ]
                    [ Svg.animate [ Svg.Attributes.attributeName "y", Svg.Attributes.values "350; 75", Svg.Attributes.dur "1.5s" ] [] ]
                ]
            ]
        ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    (model, Cmd.none)
