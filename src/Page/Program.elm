module Page.Program exposing (Model, Msg, init, route, subscriptions, title, update, updateSession, view)

import Element exposing (..)
import Element.Font as Font
import Html.Attributes
import Session exposing (Session)
import Util


type alias Model =
    { session : Session
    }


type Msg
    = NoOp


type Company
    = Mnemonic
    | Computas
    | Knowit
    | Cisco
    | Dnb
    | Bekk


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )


view : Model -> Element Msg
view model =
    let
        grid =
            List.map2
                (\company time ->
                    column [ width fill ]
                        [ image
                            [ centerX
                            , width (px 300)
                            , mouseOver [ scale 4.0 ]
                            , htmlAttribute (Html.Attributes.style "transition" "0.4s ease")
                            ]
                            { src = Util.getPng <| companyToString company
                            , description = companyToString company
                            }
                        , el [ centerX, Font.bold, padding 50 ] <| text time
                        ]
                )
    in
    column [ centerX ]
        [ row [ spacing 50 ] <| grid [ Mnemonic, Knowit, Dnb ] <| List.repeat 3 "11:00 - 15:00"
        , row [ spacing 50 ] <| grid [ Computas, Cisco, Bekk ] <| List.repeat 3 "17:00 - 21:00"
        ]


companyToString : Company -> String
companyToString company =
    case company of
        Mnemonic ->
            "mnemonic"

        Computas ->
            "computas"

        Knowit ->
            "knowit"

        Cisco ->
            "cisco"

        Dnb ->
            "dnb"

        Bekk ->
            "bekk"


route : String
route =
    "program"


title : String
title =
    "Program"


updateSession : Model -> Session -> Model
updateSession model session =
    { model | session = session }
