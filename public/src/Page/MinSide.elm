module Page.MinSide exposing (init, subscriptions, update, view, Model, Msg)

import Html exposing (Html, div, span, h1, p, text)
import Html.Attributes exposing (class, id)

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
    div [ class "min-side" ]
        [ text "min side" ]
