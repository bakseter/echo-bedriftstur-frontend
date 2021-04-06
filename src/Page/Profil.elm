module Page.Profil exposing (..)

import Element exposing (Element, centerX, column, padding)
import Session exposing (Session)
import Theme


type alias Model =
    Session


type Msg
    = NoOp


init : Session -> ( Model, Cmd Msg )
init session =
    ( session, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )


view : Model -> Element Msg
view _ =
    column [ centerX, padding 100 ]
        [ Theme.h1 [] "Kommer snart!" ]


route : String
route =
    "profil"


title : String
title =
    "Min profil"


updateSession : Model -> Session -> Model
updateSession _ =
    identity
