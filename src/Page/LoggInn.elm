module Page.LoggInn exposing (Model, Msg, init, route, subscriptions, title, toSession, update, view)

import Element exposing (Element, el, text)
import Email exposing (Email(..))
import Error exposing (Error(..))
import Session exposing (Session)
import Time


type Msg
    = NoOp


type alias Model =
    { session : Session
    , currentTime : Time.Posix
    , email : Email
    , error : Error
    }


init : Session -> Model
init session =
    { session = session
    , currentTime = Time.millisToPosix 0
    , email = Email ""
    , error = NoError
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Element Msg
view _ =
    el [] <| text "Logg inn"


route : String
route =
    "logg-inn"


title : String
title =
    "Logg inn"


toSession : Model -> Session
toSession model =
    model.session
