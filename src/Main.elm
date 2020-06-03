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
                    (typeWriterAnim Bedriftstur
                        ++ typeWriterAnim Mnemonic
                        ++ typeWriterAnim Computas
                        ++ typeWriterAnim Cisco
                        ++ typeWriterAnim Knowit
                        ++ typeWriterAnim Dnb
                        ++ typeWriterAnim Bekk
                    )
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
    Sub.batch
        [ manageSubscriptions GotProgramMsg Program.subscriptions model.modelProgram
        , manageSubscriptions GotLoggInnMsg LoggInn.subscriptions model.modelLoggInn
        , manageSubscriptions GotVerifiedMsg Verified.subscriptions model.modelVerified
        , if model.route == Bedrifter then
            manageSubscriptions GotBedrifterMsg Bedrifter.subscriptions model.modelBedrifter

          else
            Sub.none
        , manageSubscriptions GotOmMsg Om.subscriptions model.modelOm
        , Animation.subscription AnimateLogoText [ model.logoTextStyle ]
        ]


manageSubscriptions : (a -> msg) -> (b -> Sub a) -> b -> Sub msg
manageSubscriptions msg subFunc model =
    (Sub.map msg << subFunc) model


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
                    updateWithAndSendMsg LoggInn.update pageMsg model.modelLoggInn GotLoggInnMsg
            in
            ( { model | modelLoggInn = newModel }, cmd )

        GotVerifiedMsg pageMsg ->
            let
                ( newModel, cmd ) =
                    updateWithAndSendMsg Verified.update pageMsg model.modelVerified GotVerifiedMsg
            in
            ( { model | modelVerified = newModel }, cmd )

        GotProgramMsg pageMsg ->
            let
                ( newModel, cmd ) =
                    updateWithAndSendMsg Program.update pageMsg model.modelProgram GotProgramMsg
            in
            ( { model | modelProgram = newModel }, cmd )

        GotBedrifterMsg pageMsg ->
            let
                ( newModel, cmd ) =
                    updateWithAndSendMsg Bedrifter.update pageMsg model.modelBedrifter GotBedrifterMsg
            in
            ( { model | modelBedrifter = newModel }, cmd )

        GotOmMsg pageMsg ->
            let
                ( newModel, cmd ) =
                    updateWithAndSendMsg Om.update pageMsg model.modelOm GotOmMsg
            in
            ( { model | modelOm = newModel }, cmd )

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
        navbtnId =
            if model.showNavbar then
                "-anim"

            else
                ""

        name =
            model.logoTextNameAnim
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
                [ text name ]
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
        , div
            [ if model.showNavbar then
                class "navbar"

              else
                class "navbar-hidden"
            ]
            (navbar model)
        ]


userInfo : Model -> List (Html Msg)
userInfo model =
    let
        isSignedIn =
            Session.isSignedIn model.modelVerified.session
    in
    [ if isSignedIn then
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
    case model.route of
        Hjem ->
            { title = "echo bedriftstur"
            , body = [ header model, Hjem.view ]
            }

        Info ->
            { title = "Informasjon"
            , body = [ header model, Info.view ]
            }

        LoggInn ->
            { title = "Logg inn"
            , body = [ header model, translateHtmlMsg GotLoggInnMsg LoggInn.view model.modelLoggInn ]
            }

        Verified ->
            { title = "Min side"
            , body = [ header model, translateHtmlMsg GotVerifiedMsg Verified.view model.modelVerified ]
            }

        Program ->
            { title = "Program"
            , body = [ header model, translateHtmlMsg GotProgramMsg Program.view model.modelProgram ]
            }

        Bedrifter ->
            { title = "Bedrifter"
            , body = [ header model, translateHtmlMsg GotBedrifterMsg Bedrifter.view model.modelBedrifter ]
            }

        Om ->
            { title = "Om oss"
            , body = [ header model, translateHtmlMsg GotOmMsg Om.view model.modelOm ]
            }

        NotFound ->
            { title = "Fant ikke siden"
            , body =
                [ header model
                , div [ class "not-found" ]
                    [ div [ id "not-found-header" ] [ text "404" ]
                    , div [ id "not-found-text" ] [ text "Siden du leter etter eksisterer ikke." ]
                    ]
                ]
            }



-- Voodo and black magic to transform the Html Msg types from all the different Page modules
-- into the same type


translateHtmlMsg : (a -> msg) -> (b -> Html.Html a) -> b -> Html.Html msg
translateHtmlMsg msg viewFunc model =
    (Html.map msg << viewFunc) model



-- More voodoo and black magic to call the update functions of the Page modules, and send
-- messages with the Page modules respective Msg type


updateWithAndSendMsg : (b -> c -> ( d, Cmd a )) -> b -> c -> (a -> msg) -> ( d, Cmd msg )
updateWithAndSendMsg updateFunc msg model msg2 =
    let
        ( newModel, cmd ) =
            updateFunc msg model
    in
    ( newModel, Cmd.map msg2 cmd )



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
                ((List.map Animation.Messenger.send
                    << List.map AddCharLogoText
                    << List.map Tuple.second
                 )
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
