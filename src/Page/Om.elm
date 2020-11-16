module Page.Om exposing (Model, Msg, init, route, subscriptions, title, toSession, update, updateSession, view)

import Element exposing (..)
import Element.Events as Events
import Element.Font as Font
import Session exposing (Session)


type Msg
    = ShowMailElias
    | ShowMailAndreas
    | ShowMailTuva


type alias Model =
    { session : Session
    , mailElias : String
    , mailAndreas : String
    , mailTuva : String
    }


init : Session -> Model
init session =
    { session = session
    , mailElias = "Trykk for å se mail"
    , mailAndreas = "Trykk for å se mail"
    , mailTuva = "Trykk for å se mail"
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShowMailElias ->
            ( { model | mailElias = "elias.djupesland@echo.uib.no" }, Cmd.none )

        ShowMailAndreas ->
            ( { model | mailAndreas = "andreas.bakseter@echo.uib.no" }, Cmd.none )

        ShowMailTuva ->
            ( { model | mailTuva = "tuva.kvalsoren@echo.uib.no" }, Cmd.none )


view : Model -> Element Msg
view model =
    row [ centerX ]
        [ textColumn [ paddingEach { edges | right = 100 }, spacing 50 ]
            [ el [ Font.bold, Font.size 42 ] <| text "Hvem er vi?"
            , paragraph [ Font.justify, spacing 15 ]
                [ text
                    """
                    Leo duis ut diam quam nulla porttitor. Egestas sed tempus urna et pharetra. Arcu odio ut sem nulla pharetra. Bibendum est ultricies integer quis auctor elit. Eu volutpat odio facilisis mauris sit. Integer feugiat scelerisque varius morbi. Egestas erat imperdiet sed euismod nisi porta. Adipiscing elit pellentesque habitant morbi tristique senectus et. Senectus et netus et malesuada. Id cursus metus aliquam eleifend mi in nulla posuere sollicitudin. Magna etiam tempor orci eu lobortis elementum nibh tellus. Lacus vestibulum sed arcu non. Dictum fusce ut placerat orci nulla. Amet tellus cras adipiscing enim eu turpis egestas pretium aenean. Tristique sollicitudin nibh sit amet commodo nulla. Sit amet est placerat in egestas erat imperdiet. Interdum varius sit amet mattis vulputate enim. Feugiat nisl pretium fusce id velit ut tortor pretium. Urna nunc id cursus metus aliquam eleifend mi.
                    """
                ]
            ]
        , column [ spacing 40 ]
            [ row [ spacing 20 ]
                [ image [ width (px 180), height (px 180) ] { src = "elias.png", description = "Elias" }
                , column [ spacing 20 ]
                    [ el [ Font.bold, Font.size 26 ] <| text "Elias Djupesland"
                    , el [ Font.bold, Font.size 20 ] <| text "Leder og bedriftskontakt"
                    , el [ Font.italic, Events.onClick ShowMailElias ] <| text model.mailElias
                    ]
                ]
            , row [ spacing 20 ]
                [ image [ width (px 180), height (px 180) ] { src = "andreas.png", description = "Andreas" }
                , column [ spacing 20 ]
                    [ el [ Font.bold, Font.size 26 ] <| text "Andreas Salhus Bakseter"
                    , el [ Font.bold, Font.size 20 ] <| text "Webansvarlig"
                    , el [ Font.italic, Events.onClick ShowMailAndreas ] <| text model.mailAndreas
                    ]
                ]
            , row [ spacing 20 ]
                [ image [ width (px 180), height (px 180) ] { src = "tuva.png", description = "Tuva" }
                , column [ spacing 20 ]
                    [ el [ Font.bold, Font.size 26 ] <| text "Tuva Kvalsøren"
                    , el [ Font.bold, Font.size 20 ] <| text "PR-ansvarlig"
                    , el [ Font.italic, Events.onClick ShowMailTuva ] <| text model.mailTuva
                    ]
                ]
            ]
        ]


edges : { top : Int, right : Int, bottom : Int, left : Int }
edges =
    { top = 0
    , right = 0
    , bottom = 0
    , left = 0
    }


route : String
route =
    "om"


title : String
title =
    "Om oss"


toSession : Model -> Session
toSession model =
    model.session


updateSession : Model -> Session -> Model
updateSession model session =
    { model | session = session }
