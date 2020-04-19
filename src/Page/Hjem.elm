module Page.Hjem exposing (init, subscriptions, update, view, Model, Msg)

import Html exposing (Html, div, span, h1, text, br, ul, li, i, a)
import Html.Attributes exposing (class, id, href, target, rel, style)

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
        [ div [ class "hjem-content" ]
            [ div [ class "text" ]
                [ div [ class "text" ]
                    [ div [] [ text "test" ]
                    ]
                ]
            ]
        ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    (model, Cmd.none)
