port module Page.MinSide exposing (init, subscriptions, update, view, Model, Msg)

import Html exposing (Html, div, span, h1, h3, p, text, button, option, select, input, br)
import Html.Attributes exposing (class, id, type_, value, name, placeholder, disabled)
import Html.Events
import Json.Encode

type Msg

type alias Model =
    { email : String
    , firstName : String
    , lastName : String
    , degree : Degree
    }


init : Model
init =
    { email = ""
    , firstName = ""
    , lastName = ""
    , degree = None
    }

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ requestedUserInfo RequestedUserInfo
        , gotUserInfo GotUserInfo
        ]


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of

view : Model -> Html Msg
view model =
