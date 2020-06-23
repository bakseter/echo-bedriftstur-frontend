module Main exposing (main)

import Animation
import Animation.Messenger
import Browser
import Browser.Navigation
import Html exposing (Html, a, div, i, img, span, text)
import Html.Attributes exposing (alt, class, href, id, src)
import Html.Events
import Page exposing (Page(..))
import Page.Bedrifter as Bedrifter
import Page.Hjem as Hjem
import Page.Info as Info
import Page.LoggInn as LoggInn
import Page.Om as Om
import Page.Program as Program
import Page.Verified as Verified
import Session
import Svg
import Svg.Attributes exposing (x1, x2, y1, y2)
import Svg.Events
import Time
import Url


type Msg
    = UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
    | GotLoggInnMsg LoggInn.Msg
    | GotBedrifterMsg Bedrifter.Msg
    | GotProgramMsg Program.Msg
    | GotOmMsg Om.Msg
    | GotVerifiedMsg Verified.Msg
    | ShowNavbar Bool
    | AnimateLogoText Animation.Msg
    | LogoTextCursor Bool
    | RemoveCharLogoText
    | AddCharLogoText String



-- The names displayed in the typewriter animation


type Name
    = Bedriftstur
    | Mnemonic
    | Computas
    | Cisco
    | Knowit
    | Dnb
    | Bekk


type alias Model =
    { key : Browser.Navigation.Key
    , url : Url.Url
    , route : Page
    , showNavbar : Bool
    , logoTextNameAnim : String
    , logoTextStyle : Animation.Messenger.State Msg
    , modelLoggInn : LoggInn.Model
    , modelVerified : Verified.Model
    , modelBedrifter : Bedrifter.Model
    , modelProgram : Program.Model
    , modelOm : Om.Model
    }


init : () -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { key = key
      , url = url
      , route = Page.urlToPage url
      , showNavbar = False
      , logoTextNameAnim = ""
      , logoTextStyle =
            Animation.interrupt
                [ Animation.loop
                    (List.concatMap typeWriterAnim [ Bedriftstur, Mnemonic, Computas, Cisco, Knowit, Dnb, Bekk ])
                ]
                (Animation.style [])
      , modelLoggInn = LoggInn.init
      , modelVerified = Verified.init url key
      , modelBedrifter = Bedrifter.init
      , modelProgram = Program.init
      , modelOm = Om.init
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        subBedrifter =
            if model.route == Bedrifter then
                (Sub.map GotBedrifterMsg << Bedrifter.subscriptions) model.modelBedrifter

            else
                Sub.none
    in
    Sub.batch
        [ (Sub.map GotLoggInnMsg << LoggInn.subscriptions) model.modelLoggInn
        , (Sub.map GotVerifiedMsg << Verified.subscriptions) model.modelVerified
        , subBedrifter
        , (Sub.map GotProgramMsg << Program.subscriptions) model.modelProgram
        , (Sub.map GotOmMsg << Om.subscriptions) model.modelOm
        , Animation.subscription AnimateLogoText [ model.logoTextStyle ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( { model | modelLoggInn = LoggInn.init }
                    , (Browser.Navigation.pushUrl model.key << Url.toString) url
                    )

                Browser.External href ->
                    ( model, Browser.Navigation.load href )

        UrlChanged url ->
            let
                route =
                    Page.urlToPage url
            in
            ( { model | url = url, route = route }, Cmd.none )

        GotLoggInnMsg pageMsg ->
            let
                ( newModel, cmd ) =
                    LoggInn.update pageMsg model.modelLoggInn
            in
            ( { model | modelLoggInn = newModel }, Cmd.map GotLoggInnMsg cmd )

        GotVerifiedMsg pageMsg ->
            let
                ( newModel, cmd ) =
                    Verified.update pageMsg model.modelVerified
            in
            ( { model | modelVerified = newModel }, Cmd.map GotVerifiedMsg cmd )

        GotProgramMsg pageMsg ->
            let
                ( newModel, cmd ) =
                    Program.update pageMsg model.modelProgram
            in
            ( { model | modelProgram = newModel }, Cmd.map GotProgramMsg cmd )

        GotBedrifterMsg pageMsg ->
            let
                ( newModel, cmd ) =
                    Bedrifter.update pageMsg model.modelBedrifter
            in
            ( { model | modelBedrifter = newModel }, Cmd.map GotBedrifterMsg cmd )

        GotOmMsg pageMsg ->
            let
                ( newModel, cmd ) =
                    Om.update pageMsg model.modelOm
            in
            ( { model | modelOm = newModel }, Cmd.map GotOmMsg cmd )

        ShowNavbar linksToHome ->
            if model.showNavbar == False then
                if linksToHome then
                    ( model, Cmd.none )

                else
                    ( { model | showNavbar = True }, Cmd.none )

            else
                ( { model | showNavbar = False }, Cmd.none )

        AnimateLogoText anim ->
            let
                ( newLogoTextStyle, logoTextCmds ) =
                    Animation.Messenger.update anim model.logoTextStyle
            in
            ( { model | logoTextStyle = newLogoTextStyle }, logoTextCmds )

        LogoTextCursor show ->
            if show then
                ( { model | logoTextNameAnim = model.logoTextNameAnim ++ "_" }, Cmd.none )

            else
                ( { model | logoTextNameAnim = String.dropRight 1 model.logoTextNameAnim }, Cmd.none )

        RemoveCharLogoText ->
            let
                newText =
                    if String.endsWith "_" model.logoTextNameAnim then
                        String.dropRight 2 model.logoTextNameAnim ++ "_"

                    else
                        String.dropRight 1 model.logoTextNameAnim ++ "_"
            in
            ( { model | logoTextNameAnim = newText }, Cmd.none )

        AddCharLogoText char ->
            let
                newText =
                    if String.endsWith "_" model.logoTextNameAnim then
                        String.dropRight 1 model.logoTextNameAnim ++ char ++ "_"

                    else
                        model.logoTextNameAnim ++ char ++ "_"
            in
            ( { model | logoTextNameAnim = newText }, Cmd.none )


header : Model -> Html Msg
header model =
    let
        ( navbtnId, navbarClass ) =
            if model.showNavbar then
                ( "-anim", "navbar" )

            else
                ( "", "navbar-hidden" )
    in
    div [ class "menu" ]
        [ div [ id "logo-wrapper" ]
            [ a [ href "/", Html.Events.onClick (ShowNavbar True) ]
                [ img [ id "logo", alt "logo", src "/img/echoicon.png" ] [] ]
            ]
        , div [ id "logo-text" ]
            [ span [] [ text "echo " ]
            , span
                (Animation.render model.logoTextStyle)
                [ text model.logoTextNameAnim ]
            ]
        , Svg.svg [ Svg.Attributes.id "navbtn", Svg.Events.onClick (ShowNavbar False), Svg.Attributes.width "50", Svg.Attributes.height "40" ]
            [ Svg.line
                [ Svg.Attributes.class "navbtn-line", Svg.Attributes.id ("first-line" ++ navbtnId), x1 "0", x2 "50", y1 "5", y2 "5" ]
                []
            , Svg.line
                [ Svg.Attributes.class "navbtn-line", Svg.Attributes.id ("middle-line" ++ navbtnId), x1 "0", x2 "50", y1 "20", y2 "20" ]
                []
            , Svg.line
                [ Svg.Attributes.class "navbtn-line", Svg.Attributes.id ("second-line" ++ navbtnId), x1 "0", x2 "50", y1 "35", y2 "35" ]
                []
            ]
        , div [ class navbarClass ]
            (navbar model)
        ]


userInfo : Model -> List (Html Msg)
userInfo model =
    [ if Session.isSignedIn model.modelVerified.session then
        a [ class "user-info", href ("/" ++ Verified.route), Html.Events.onClick (ShowNavbar False) ]
            [ i [ id "profile-icon", class "fa fa-user-circle" ] [], text "Min profil" ]

      else
        a [ class "user-info", href ("/" ++ LoggInn.route), Html.Events.onClick (ShowNavbar False) ]
            [ i [ id "sign-in-icon", class "fa fa-sign-in" ] [], text "Logg inn" ]
    ]


navbar : Model -> List (Html Msg)
navbar model =
    [ span [] []
    , div [ class "navbar-item" ]
        (userInfo model)
    , span [] []
    , a [ class "navbar-item", href ("/" ++ Info.route), Html.Events.onClick (ShowNavbar False) ] [ text "Informasjon" ]
    , a [ class "navbar-item", href ("/" ++ Program.route), Html.Events.onClick (ShowNavbar False) ] [ text "Program" ]
    , a [ class "navbar-item", href ("/" ++ Bedrifter.route), Html.Events.onClick (ShowNavbar False) ] [ text "Bedrifter" ]
    , a [ class "navbar-item", href ("/" ++ Om.route), Html.Events.onClick (ShowNavbar False) ] [ text "Om oss" ]
    ]


view : Model -> Browser.Document Msg
view model =
    let
        ( title, body ) =
            case model.route of
                Hjem ->
                    ( "echo bedriftstur"
                    , Hjem.view
                    )

                Info ->
                    ( "Informasjon"
                    , Info.view
                    )

                LoggInn ->
                    ( "Logg inn"
                    , (Html.map GotLoggInnMsg << LoggInn.view) model.modelLoggInn
                    )

                Verified ->
                    ( "Min side"
                    , (Html.map GotVerifiedMsg << Verified.view) model.modelVerified
                    )

                Program ->
                    ( "Program"
                    , (Html.map GotProgramMsg << Program.view) model.modelProgram
                    )

                Bedrifter ->
                    ( "Bedrifter"
                    , (Html.map GotBedrifterMsg << Bedrifter.view) model.modelBedrifter
                    )

                Om ->
                    ( "Om oss"
                    , (Html.map GotOmMsg << Om.view) model.modelOm
                    )

                NotFound ->
                    ( "Fant ikke siden"
                    , div [ class "not-found" ]
                        [ div [ id "not-found-header" ] [ text "404" ]
                        , div [ id "not-found-text" ] [ text "Siden du leter etter eksisterer ikke." ]
                        ]
                    )
    in
    { title = title
    , body = [ header model, body ]
    }



-- Return the corresponding string of a Name type


nameToString : Name -> String
nameToString name =
    let
        result =
            List.filter (\( x, _ ) -> x == name) namesList
    in
    case result of
        [ ( _, string ) ] ->
            string

        _ ->
            ""



-- List of all the strings for every Name type


namesList : List ( Name, String )
namesList =
    [ ( Bedriftstur, "bedriftstur" )
    , ( Mnemonic, "mnemonic" )
    , ( Computas, "Computas" )
    , ( Cisco, "Cisco" )
    , ( Knowit, "Knowit" )
    , ( Dnb, "DNB" )
    , ( Bekk, "Bekk" )
    ]



-- Animates the top left part of the logo text


typeWriterAnim : Name -> List (Animation.Messenger.Step Msg)
typeWriterAnim transitionToName =
    let
        stylizedName =
            if transitionToName /= Bedriftstur then
                "+ " ++ nameToString transitionToName

            else
                nameToString transitionToName
    in
    [ Animation.repeat 4
        [ (Animation.wait << Time.millisToPosix) 500
        , Animation.Messenger.send (LogoTextCursor True)
        , (Animation.wait << Time.millisToPosix) 500
        , Animation.Messenger.send (LogoTextCursor False)
        ]
    , Animation.repeat 1
        [ (Animation.wait << Time.millisToPosix) 500
        , Animation.Messenger.send (LogoTextCursor True)
        , (Animation.wait << Time.millisToPosix) 500
        ]
    , Animation.repeat 1
        (Animation.wait (Time.millisToPosix 120)
            :: (List.intersperse << Animation.wait << Time.millisToPosix) 120
                (List.map (Animation.Messenger.send << AddCharLogoText << Tuple.second)
                    (List.indexedMap
                        (\x y -> ( x, String.dropLeft x (String.left (x + 1) y) ))
                        (List.repeat (String.length stylizedName) stylizedName)
                    )
                )
        )
    , Animation.repeat 1
        [ Animation.Messenger.send (LogoTextCursor False)
        , (Animation.wait << Time.millisToPosix) 500
        , Animation.Messenger.send (LogoTextCursor False)
        , (Animation.wait << Time.millisToPosix) 500
        ]
    , Animation.repeat 4
        [ (Animation.wait << Time.millisToPosix) 500
        , Animation.Messenger.send (LogoTextCursor True)
        , (Animation.wait << Time.millisToPosix) 500
        , Animation.Messenger.send (LogoTextCursor False)
        ]
    , Animation.repeat 1
        [ (Animation.wait << Time.millisToPosix) 500
        , Animation.Messenger.send (LogoTextCursor True)
        , (Animation.wait << Time.millisToPosix) 500
        ]
    , Animation.repeat (String.length stylizedName + 1)
        [ Animation.Messenger.send RemoveCharLogoText
        , (Animation.wait << Time.millisToPosix) 120
        ]
    , Animation.repeat 1
        [ (Animation.wait << Time.millisToPosix) 500
        , Animation.Messenger.send (LogoTextCursor False)
        ]
    ]


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }
