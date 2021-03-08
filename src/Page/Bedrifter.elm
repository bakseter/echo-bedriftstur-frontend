module Page.Bedrifter exposing (Model, Msg, init, route, subscriptions, title, update, updateSession, view)

import Browser.Dom
import Element exposing (Element, column, el, row, spacing, text)
import Session exposing (Session)
import Task
import Time


type Msg
    = NoOp


type alias Model =
    { session : Session }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Element Msg
view model =
    let
        weeksInMonth =
            4

        weekDays =
            [ "Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun" ]
    in
    column [] <|
        List.repeat 4
            (row [ spacing 10 ] <|
                List.map (\day -> column [] [ text day ]) weekDays
            )


route : String
route =
    "bedrifter"


title : String
title =
    "Bedrifter"


updateSession : Model -> Session -> Model
updateSession model session =
    { model | session = session }
