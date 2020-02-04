module Page.Om exposing (init, subscriptions, update, view, Model, Msg)

import Html exposing (Html, div, text, img)
import Html.Attributes exposing (class, id, src, alt)

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
      [ div [ id "om-tekst" ] 
            [ div [ class "text" ] [ text "Bedriftsturkomitéen består av tre frivillige studenter." ] ]
      , div [ id "elias" ] [ img [ class "portrett", src "/img/elias.png", alt "Elias" ] [] ]
      , div [ class "om-info", id "elias-info" ]
            [ div [ class "navn" ] [ text "Elias Djupesland" ]
            , div [ class "tittel" ] [ text "Executive Manager and Head of Coporate Relations" ]
            ]
      , div [ id "andreas" ] [ img [ class "portrett", src "/img/andreas.png", alt "Andreas" ] [] ]
      , div [ class "om-info", id "andreas-info" ] 
            [ div [ class "navn" ] [ text "Andreas Salhus Bakseter" ]
            , div [ class "tittel" ] [ text "Lead Web Application Developer and Executive Transport Manager" ]
            ]
      , div [ id "tuva" ] [ img [ class "portrett", src "/img/tuva.png", alt "Tuva" ] [] ]
      , div [ class "om-info", id "tuva-info" ] 
            [ div [ class "navn" ] [ text "Tuva Kvalsøren" ]
            , div [ class "tittel" ] [ text "Head of Public Relations and Executive Acommodation Manager" ]
            ]
        ]

