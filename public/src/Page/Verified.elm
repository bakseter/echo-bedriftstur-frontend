module Page.Verified exposing (init, subscriptions, update, view, Model, Msg)

import Html exposing (Html, div, span, br, text) 
import Html.Attributes exposing (class, id)

type Msg
    = LoggedIn

type alias Model
    = String

init : Model
init =
    "lol"

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg  -> Model -> (Model, Cmd Msg)
update msg model =
    (model, Cmd.none)

view : Model -> Html Msg
view model =
    div [ class "verified" ]
        [ text "halla bro" ]
