module Page.Om exposing (init, subscriptions, update, view, Model, Msg)

import Html exposing (Html, div, text, img, i, a)
import Html.Attributes exposing (class, id, src, alt, href, target, rel)

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
    div [ class "om" ]
        [ div [ id "om-content" ]
            [ div [ id "om-tekst" ] 
                [ div [ class "text" ] [ text "Bedriftsturkomitéen består av tre frivillige studenter." ] ]
                , div [ id "elias" ] [ img [ class "portrett", src "/img/elias.png", alt "Elias" ] [] ]
                , div [ class "om-info", id "elias-info" ]
                    [ div [ class "navn" ] [ text "Elias Djupesland" ]
                    , div [ class "tittel" ] [ text "Bachelor, datateknologi" ]
                    ]
                , div [ id "andreas" ] [ img [ class "portrett", src "/img/andreas.png", alt "Andreas" ] [] ]
                , div [ class "om-info", id "andreas-info" ] 
                    [ div [ class "navn" ] [ text "Andreas Salhus Bakseter" ]
                    , div [ class "tittel" ] [ text "Bachelor, Datateknologi" ]
                    ]
                , div [ id "tuva" ] [ img [ class "portrett", src "/img/tuva.png", alt "Tuva" ] [] ]
                , div [ class "om-info", id "tuva-info" ] 
                    [ div [ class "navn" ] [ text "Tuva Kvalsøren" ]
                    , div [ class "tittel" ] [ text "Bachelor, IMØ" ]
                    ]
            ]
        , div [ id "om-links" ]
            [ a [ target "_blank"
                , rel "noopener noreferrer"
                , href "https://www.linkedin.com/showcase/echobedriftstur"
                , id "linkedinLink" 
                ] [ i [ class "fa fa-linkedin" ] [] ]
            , a [ target "_blank", rel "noopener noreferrer"
                 , href "https://github.com/bakseter/echo-bedriftstur"
                 , id "githubLink" 
                 ] [ i [ class "fa fa-github" ] [] ]
            ]
        ]
