port module Page.Profil exposing (..)

import Content exposing (Content)
import Cred exposing (Cred)
import Degree exposing (Degree(..))
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input
import Email exposing (Email(..))
import Error exposing (Error)
import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Json.Decode as Decode
import Json.Encode as Encode
import Monocle.Compose as Compose
import Monocle.Lens exposing (Lens)
import Monocle.Optional exposing (Optional)
import Session exposing (Session)
import Terms exposing (Terms(..))
import Theme
import User exposing (User)


port userStatusChanged : (Encode.Value -> msg) -> Sub msg


port sendSignInLink : Encode.Value -> Cmd msg


port sendSignInLinkSucceeded : (Encode.Value -> msg) -> Sub msg


port sendSignInLinkError : (Encode.Value -> msg) -> Sub msg


port signInSucceeded : (Encode.Value -> msg) -> Sub msg


port signInError : (Encode.Value -> msg) -> Sub msg


port getUserInfo : Encode.Value -> Cmd msg


port getUserInfoSucceeded : (Encode.Value -> msg) -> Sub msg


port getUserInfoError : (Encode.Value -> msg) -> Sub msg


port updateUserInfo : Encode.Value -> Cmd msg


port updateUserInfoSucceeded : (Encode.Value -> msg) -> Sub msg


port updateUserInfoError : (Encode.Value -> msg) -> Sub msg


port createTicket : Encode.Value -> Cmd msg


port createTicketSucceeded : (Encode.Value -> msg) -> Sub msg


port createTicketError : (Encode.Value -> msg) -> Sub msg


port signOut : Encode.Value -> Cmd msg


port signOutSucceeded : (Encode.Value -> msg) -> Sub msg


port signOutError : (Encode.Value -> msg) -> Sub msg


type Msg
    = GotSignInMsg SignInMsg
    | GotProfileMsg ProfileMsg
    | UserStatusChanged Encode.Value
    | GotError Encode.Value


type SignInMsg
    = SendSignInLink
    | SendSignInLinkSucceeded Encode.Value
    | TypedEmail String


type ProfileMsg
    = SignInSucceeded Encode.Value
    | GetUserInfo
    | GetUserInfoSucceeded Encode.Value
    | UpdateUserInfo Session Content
    | UpdateUserInfoSucceeded Encode.Value
    | CreateTicket
    | CreateTicketSucceeded Encode.Value
    | SignOut
    | SignOutSucceeded Encode.Value
    | TypedFirstName String
    | TypedLastName String
    | ChangedDegree String
    | CheckedBoxOne Bool
    | CheckedBoxTwo Bool
    | CheckedBoxThree Bool


type alias Model =
    { session : Session
    , signInModel : SignInModel
    , profileModel : ProfileModel
    , subpage : Subpage
    , error : Maybe Error
    }


type Subpage
    = SignIn
    | Profile


type alias SignInModel =
    { emailInput : Maybe Email
    , linkSent : Bool
    }


type alias ProfileModel =
    { firstName : String
    , lastName : String
    , degree : Maybe Degree
    , terms : Terms
    , user : Maybe User
    , formStage : FormStage
    }


type FormStage
    = FormIdle
    | FormSent
    | FormReceived


init : Session -> ( Model, Cmd Msg )
init session =
    let
        ( cmd, subpage ) =
            case session.cred of
                Just cred ->
                    ( getUserInfo <| Cred.encode cred, Profile )

                Nothing ->
                    ( Cmd.none, SignIn )
    in
    ( { session = session
      , signInModel =
            { emailInput = Nothing
            , linkSent = False
            }
      , profileModel =
            { firstName = ""
            , lastName = ""
            , degree = Nothing
            , terms = Terms.fromAbsolute False
            , user = Nothing
            , formStage = FormIdle
            }
      , subpage = subpage
      , error = Nothing
      }
    , cmd
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch <|
        List.map (Sub.map GotProfileMsg)
            [ signInSucceeded SignInSucceeded
            , createTicketSucceeded CreateTicketSucceeded
            , getUserInfoSucceeded GetUserInfoSucceeded
            , updateUserInfoSucceeded UpdateUserInfoSucceeded
            ]
            ++ List.map (Sub.map GotSignInMsg)
                [ sendSignInLinkSucceeded SendSignInLinkSucceeded ]
            ++ [ userStatusChanged UserStatusChanged
               , getUserInfoError GotError
               , sendSignInLinkError GotError
               , updateUserInfoError GotError
               , createTicketError GotError
               , signInError GotError
               ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSignInMsg signInMsg ->
            updateSignInModel signInMsg model

        GotProfileMsg profileMsg ->
            updateProfileModel profileMsg model

        UserStatusChanged json ->
            case Cred.decode json of
                Just cred ->
                    let
                        newModel =
                            updateCred.set (Just cred) model
                    in
                    ( { newModel | subpage = Profile }, Cmd.none )

                Nothing ->
                    ( { model | error = Just Error.JsonParseError }, Cmd.none )

        GotError json ->
            ( { model | error = Error.decode json }, Cmd.none )


updateSignInModel : SignInMsg -> Model -> ( Model, Cmd Msg )
updateSignInModel msg model =
    case msg of
        SendSignInLink ->
            case model.signInModel.emailInput of
                Just email ->
                    ( model, sendSignInLink (Email.encodeForSignInLink email) )

                Nothing ->
                    ( model, Cmd.none )

        SendSignInLinkSucceeded _ ->
            ( updateLinkSent.set True model, Cmd.none )

        TypedEmail str ->
            ( updateEmailInput.set (Just <| Email str) model, Cmd.none )


sessionLens : Lens Model Session
sessionLens =
    Lens .session (\b a -> { a | session = b })


updateCred : Lens Model (Maybe Cred)
updateCred =
    Compose.lensWithLens
        (Lens .cred (\b a -> { a | cred = b }))
        sessionLens


signInModelLens : Lens Model SignInModel
signInModelLens =
    Lens .signInModel (\b a -> { a | signInModel = b })


updateLinkSent : Lens Model Bool
updateLinkSent =
    Compose.lensWithLens
        (Lens .linkSent (\b a -> { a | linkSent = b }))
        signInModelLens


updateEmailInput : Lens Model (Maybe Email)
updateEmailInput =
    Compose.lensWithLens
        (Lens .emailInput (\b a -> { a | emailInput = b }))
        signInModelLens


profileModelLens : Lens Model ProfileModel
profileModelLens =
    Lens .profileModel (\b a -> { a | profileModel = b })


updateFirstName : Lens Model String
updateFirstName =
    Compose.lensWithLens
        (Lens .firstName (\b a -> { a | firstName = b }))
        profileModelLens


updateLastName : Lens Model String
updateLastName =
    Compose.lensWithLens
        (Lens .lastName (\b a -> { a | lastName = b }))
        profileModelLens


updateDegree : Lens Model (Maybe Degree)
updateDegree =
    Compose.lensWithLens
        (Lens .degree (\b a -> { a | degree = b }))
        profileModelLens


updateTerms : Lens Model Terms
updateTerms =
    Compose.lensWithLens
        (Lens .terms (\b a -> { a | terms = b }))
        profileModelLens


updateUser : Lens Model (Maybe User)
updateUser =
    Compose.lensWithLens
        (Lens .user (\b a -> { a | user = b }))
        profileModelLens


updateFormStage : Lens Model FormStage
updateFormStage =
    Compose.lensWithLens
        (Lens .formStage (\b a -> { a | formStage = b }))
        profileModelLens


updateProfileModel : ProfileMsg -> Model -> ( Model, Cmd Msg )
updateProfileModel msg model =
    case msg of
        SignInSucceeded _ ->
            ( { model | subpage = Profile }, Cmd.none )

        GetUserInfo ->
            case model.session.cred of
                Just cred ->
                    ( model, getUserInfo <| Cred.encode cred )

                Nothing ->
                    ( model, Cmd.none )

        GetUserInfoSucceeded json ->
            case User.decode json of
                Just user ->
                    ( updateUser.set (Just user) model
                        |> updateFirstName.set user.content.firstName
                        |> updateLastName.set user.content.lastName
                        |> updateDegree.set (Just user.content.degree)
                        |> updateTerms.set user.content.terms
                    , Cmd.none
                    )

                Nothing ->
                    ( { model | error = Just Error.JsonParseError }, Cmd.none )

        UpdateUserInfo _ _ ->
            ( updateFormStage.set FormSent model, updateUserInfo Encode.null )

        UpdateUserInfoSucceeded _ ->
            ( updateFormStage.set FormReceived model, Cmd.none )

        CreateTicket ->
            ( model, createTicket Encode.null )

        CreateTicketSucceeded _ ->
            ( model, Cmd.none )

        SignOut ->
            ( model, signOut Encode.null )

        SignOutSucceeded _ ->
            ( { model | subpage = SignIn }, Cmd.none )

        TypedFirstName str ->
            ( updateFirstName.set str model, Cmd.none )

        TypedLastName str ->
            ( updateLastName.set str model, Cmd.none )

        ChangedDegree str ->
            case Degree.fromString str of
                Just degree ->
                    ( updateDegree.set (Just degree) model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        CheckedBoxOne bool ->
            let
                (Terms _ b2 b3) =
                    model.profileModel.terms
            in
            ( updateTerms.set (Terms bool b2 b3) model, Cmd.none )

        CheckedBoxTwo bool ->
            let
                (Terms b1 _ b3) =
                    model.profileModel.terms
            in
            ( updateTerms.set (Terms b1 bool b3) model, Cmd.none )

        CheckedBoxThree bool ->
            let
                (Terms b1 b2 _) =
                    model.profileModel.terms
            in
            ( updateTerms.set (Terms b1 b2 bool) model, Cmd.none )


view : Model -> Element Msg
view model =
    case model.subpage of
        SignIn ->
            viewSignIn model

        Profile ->
            viewProfile model


viewSignIn : Model -> Element Msg
viewSignIn model =
    column [ centerX, spacing 50, padding 100 ]
        [ Input.email [ Input.focusedOnLoad ]
            { onChange = GotSignInMsg << TypedEmail
            , text = Email.toString <| Maybe.withDefault (Email "") model.signInModel.emailInput
            , placeholder = Just <| Input.placeholder [] (text "Email")
            , label = Input.labelHidden "Email"
            }
        , Input.button
            [ Background.color Theme.button
            , padding 10
            , mouseOver [ Background.color (rgb255 255 0 255) ]
            ]
            { onPress = Just <| GotSignInMsg SendSignInLink
            , label = text "Logg inn"
            }
        , el [] <|
            if model.signInModel.linkSent then
                text <| "Vi har sendt deg en link."

            else
                none
        ]


viewProfile : Model -> Element Msg
viewProfile model =
    column [ centerX, spacing 50, padding 100 ]
        [ Input.text []
            { onChange = GotProfileMsg << TypedFirstName
            , text = model.profileModel.firstName
            , placeholder = Just <| Input.placeholder [] (text "Fornavn")
            , label = Input.labelHidden "Fornavn"
            }
        , Input.text []
            { onChange = GotProfileMsg << TypedLastName
            , text = model.profileModel.lastName
            , placeholder = Just <| Input.placeholder [] (text "Etternavn")
            , label = Input.labelHidden "Etternavn"
            }
        , html <|
            Html.select [] <|
                List.map
                    (\d ->
                        Html.option
                            [ HtmlA.value <| Degree.toString False d ]
                            [ Html.text <| Degree.toString True d ]
                    )
                    [ DTEK, DSIK, DVIT, BINF, IMØ, IKT, KOGNI, INF, PROG, POST, MISC ]
        , Input.checkbox []
            { onChange = GotProfileMsg << CheckedBoxOne
            , icon = Input.defaultCheckbox
            , checked = Terms.fst model.profileModel.terms
            , label =
                Input.labelRight []
                    (text "Jeg godkjenner at ...")
            }
        , Input.checkbox []
            { onChange = GotProfileMsg << CheckedBoxTwo
            , icon = Input.defaultCheckbox
            , checked = Terms.snd model.profileModel.terms
            , label =
                Input.labelRight []
                    (text "Jeg godkjenner også at ...")
            }
        , Input.checkbox []
            { onChange = GotProfileMsg << CheckedBoxThree
            , icon = Input.defaultCheckbox
            , checked = Terms.thd model.profileModel.terms
            , label =
                Input.labelRight []
                    (text "Jeg godkjenner OGSÅ at ...")
            }
        ]


route : String
route =
    "profil"


title : String
title =
    "Min profil"


toSession : Model -> Session
toSession model =
    model.session


updateSession : Model -> Session -> Model
updateSession model session =
    { model | session = session }
