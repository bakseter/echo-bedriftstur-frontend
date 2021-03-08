module Page.Profil exposing (..)

import Api
import Content exposing (Content)
import Cred exposing (Cred, IdToken(..))
import Degree exposing (Degree(..))
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input
import Email exposing (Email(..))
import Error exposing (Error)
import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Monocle.Compose as Compose
import Monocle.Lens exposing (Lens)
import Monocle.Optional exposing (Optional)
import Password exposing (Password(..))
import Session exposing (Session)
import Terms exposing (Terms(..))
import Theme
import Time
import User exposing (User)


type Msg
    = GotRegisterMsg RegisterMsg
    | GotSignInMsg SignInMsg
    | GotProfileMsg ProfileMsg
    | GotError Encode.Value
    | CheckCred
    | RefreshIdTokenResponse (Result Http.Error Cred)
    | Toggle


type RegisterMsg
    = RegisterUser
    | RegisterUserResponse (Result Http.Error Cred)
    | TypedNewEmail String
    | TypedNewPassword String
    | TypedConfirmNewPassword String


type SignInMsg
    = SignInUser
    | SignInUserResponse (Result Http.Error Cred)
    | GetUserDataResponse (Result Http.Error User)
    | TypedEmail String
    | TypedPassword String


type ProfileMsg
    = TypedFirstName String
    | TypedLastName String
    | ChangedDegree String
    | CheckedBoxOne Bool
    | CheckedBoxTwo Bool
    | CheckedBoxThree Bool


type alias Model =
    { session : Session
    , registerModel : RegisterModel
    , signInModel : SignInModel
    , profileModel : ProfileModel
    , subpage : Subpage
    , error : Maybe Error
    }


type Subpage
    = Register
    | SignIn
    | Profile


type alias RegisterModel =
    { newEmailInput : Maybe Email
    , newPasswordInput : Maybe Password
    , confirmNewPasswordInput : Maybe Password
    }


type alias SignInModel =
    { emailInput : Maybe Email
    , passwordInput : Maybe Password
    }


type alias ProfileModel =
    { firstName : String
    , lastName : String
    , degree : Maybe Degree
    , terms : Terms
    , user : Maybe User
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , registerModel =
            { newEmailInput = Nothing
            , newPasswordInput = Nothing
            , confirmNewPasswordInput = Nothing
            }
      , signInModel =
            { emailInput = Nothing
            , passwordInput = Nothing
            }
      , profileModel =
            { firstName = ""
            , lastName = ""
            , degree = Nothing
            , terms = Terms.fromAbsolute False
            , user = Nothing
            }
      , subpage = SignIn
      , error = Nothing
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 60000 <| \_ -> CheckCred


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotRegisterMsg registerMsg ->
            updateRegisterModel registerMsg model

        GotSignInMsg signInMsg ->
            updateSignInModel signInMsg model

        GotProfileMsg profileMsg ->
            updateProfileModel profileMsg model

        GotError json ->
            ( { model | error = Error.decode json }, Cmd.none )

        Toggle ->
            let
                subpage =
                    case model.subpage of
                        Register ->
                            SignIn

                        SignIn ->
                            Register

                        _ ->
                            model.subpage
            in
            ( { model | subpage = subpage }, Cmd.none )

        CheckCred ->
            case model.session.cred of
                Just cred ->
                    let
                        (IdToken token time) =
                            cred.idToken
                    in
                    if time <= 120 then
                        ( model, Api.refreshIdToken RefreshIdTokenResponse model.session )

                    else
                        ( updateCred.set
                            (Just
                                { idToken = IdToken token (time - 60)
                                , refreshToken = cred.refreshToken
                                , uid = cred.uid
                                }
                            )
                            model
                        , Cmd.none
                        )

                Nothing ->
                    ( model, Cmd.none )

        RefreshIdTokenResponse response ->
            case response of
                Ok cred ->
                    ( updateCred.set
                        (Just
                            { idToken = cred.idToken
                            , refreshToken = cred.refreshToken
                            , uid = cred.uid
                            }
                        )
                        model
                    , Cmd.none
                    )

                Err err ->
                    ( Debug.log (Debug.toString err) model, Cmd.none )


updateRegisterModel : RegisterMsg -> Model -> ( Model, Cmd Msg )
updateRegisterModel msg model =
    case msg of
        RegisterUser ->
            case ( model.registerModel.newEmailInput, model.registerModel.newPasswordInput, model.registerModel.confirmNewPasswordInput ) of
                ( Just email, Just password, Just confirmPassword ) ->
                    {-
                       if Email.isValid email && Password.isValid password && password == confirmPassword then
                    -}
                    if True then
                        ( model, Api.register (GotRegisterMsg << RegisterUserResponse) email password model.session )

                    else
                        ( Debug.log "Password and email is not valid" model, Cmd.none )

                _ ->
                    ( Debug.log "Password and/or email is Nothing" model, Cmd.none )

        RegisterUserResponse response ->
            case response of
                Ok data ->
                    ( Debug.log (Debug.toString data) model, Cmd.none )

                Err err ->
                    ( Debug.log (Debug.toString err) model, Cmd.none )

        TypedNewEmail str ->
            ( updateNewEmailInput.set (Just <| Email str) model, Cmd.none )

        TypedNewPassword str ->
            ( updateNewPasswordInput.set (Just <| Password str) model, Cmd.none )

        TypedConfirmNewPassword str ->
            ( updateConfirmNewPassordInput.set (Just <| Password str) model, Cmd.none )


updateSignInModel : SignInMsg -> Model -> ( Model, Cmd Msg )
updateSignInModel msg model =
    case msg of
        SignInUser ->
            case ( model.signInModel.emailInput, model.signInModel.passwordInput ) of
                ( Just email, Just password ) ->
                    ( model, Api.signIn (GotSignInMsg << SignInUserResponse) email password model.session )

                _ ->
                    ( model, Cmd.none )

        SignInUserResponse response ->
            case response of
                Ok cred ->
                    let
                        newModel =
                            updateCred.set (Just cred) model
                    in
                    ( newModel, Api.getUserData (GotSignInMsg << GetUserDataResponse) newModel.session )

                Err err ->
                    ( Debug.log (Debug.toString err ++ "err: SignInUserResponse @ updateSignInModel") model, Cmd.none )

        GetUserDataResponse response ->
            case response of
                Ok user ->
                    let
                        newModel =
                            updateUser.set (Just user) model
                                |> updateFirstName.set user.content.firstName
                                |> updateLastName.set user.content.lastName
                                |> updateDegree.set (Just user.content.degree)
                                |> updateTerms.set user.content.terms
                    in
                    ( { newModel | subpage = Profile }, Cmd.none )

                Err err ->
                    ( Debug.log (Debug.toString err ++ "err: GetUserDataResponse @ updateSignInModel") model, Cmd.none )

        TypedEmail str ->
            ( updateEmailInput.set (Just <| Email str) model, Cmd.none )

        TypedPassword str ->
            ( updatePasswordInput.set (Just <| Password str) model, Cmd.none )


sessionLens : Lens Model Session
sessionLens =
    Lens .session (\b a -> { a | session = b })


updateCred : Lens Model (Maybe Cred)
updateCred =
    Compose.lensWithLens
        (Lens .cred (\b a -> { a | cred = b }))
        sessionLens


registerModelLens : Lens Model RegisterModel
registerModelLens =
    Lens .registerModel (\b a -> { a | registerModel = b })


updateNewEmailInput : Lens Model (Maybe Email)
updateNewEmailInput =
    Compose.lensWithLens
        (Lens .newEmailInput (\b a -> { a | newEmailInput = b }))
        registerModelLens


updateNewPasswordInput : Lens Model (Maybe Password)
updateNewPasswordInput =
    Compose.lensWithLens
        (Lens .newPasswordInput (\b a -> { a | newPasswordInput = b }))
        registerModelLens


updateConfirmNewPassordInput : Lens Model (Maybe Password)
updateConfirmNewPassordInput =
    Compose.lensWithLens
        (Lens .confirmNewPasswordInput (\b a -> { a | confirmNewPasswordInput = b }))
        registerModelLens


signInModelLens : Lens Model SignInModel
signInModelLens =
    Lens .signInModel (\b a -> { a | signInModel = b })


updateEmailInput : Lens Model (Maybe Email)
updateEmailInput =
    Compose.lensWithLens
        (Lens .emailInput (\b a -> { a | emailInput = b }))
        signInModelLens


updatePasswordInput : Lens Model (Maybe Password)
updatePasswordInput =
    Compose.lensWithLens
        (Lens .passwordInput (\b a -> { a | passwordInput = b }))
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


updateProfileModel : ProfileMsg -> Model -> ( Model, Cmd Msg )
updateProfileModel msg model =
    case msg of
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
        Register ->
            viewRegister model

        SignIn ->
            viewSignIn model

        Profile ->
            viewProfile model


toggleHeader : Element Msg
toggleHeader =
    Input.button
        [ Background.color Theme.button
        , padding 10
        ]
        { onPress = Just Toggle
        , label = text "Toggle"
        }


viewRegister : Model -> Element Msg
viewRegister model =
    column [ centerX, spacing 50, padding 100 ]
        [ toggleHeader
        , Input.email [ Input.focusedOnLoad ]
            { onChange = GotRegisterMsg << TypedNewEmail
            , text = Email.toString <| Maybe.withDefault (Email "") model.registerModel.newEmailInput
            , placeholder = Just <| Input.placeholder [] (text "Email")
            , label = Input.labelHidden "Email"
            }
        , Input.newPassword []
            { onChange = GotRegisterMsg << TypedNewPassword
            , text = Password.toString <| Maybe.withDefault (Password "") model.registerModel.newPasswordInput
            , placeholder = Just <| Input.placeholder [] (text "Passord")
            , label = Input.labelHidden "Passord"
            , show = False
            }
        , Input.newPassword []
            { onChange = GotRegisterMsg << TypedConfirmNewPassword
            , text = Password.toString <| Maybe.withDefault (Password "") model.registerModel.confirmNewPasswordInput
            , placeholder = Just <| Input.placeholder [] (text "Gjenta passord")
            , label = Input.labelHidden "Passord"
            , show = False
            }
        , Input.button
            [ Background.color Theme.button
            , padding 10
            ]
            { onPress = Just <| GotRegisterMsg RegisterUser
            , label = text "Lag ny bruker"
            }
        ]


viewSignIn : Model -> Element Msg
viewSignIn model =
    column [ centerX, spacing 50, padding 100 ]
        [ toggleHeader
        , Input.email [ Input.focusedOnLoad ]
            { onChange = GotSignInMsg << TypedEmail
            , text = Email.toString <| Maybe.withDefault (Email "") model.signInModel.emailInput
            , placeholder = Just <| Input.placeholder [] (text "Email")
            , label = Input.labelHidden "Email"
            }
        , Input.currentPassword []
            { onChange = GotSignInMsg << TypedPassword
            , text = Password.toString <| Maybe.withDefault (Password "") model.signInModel.passwordInput
            , placeholder = Just <| Input.placeholder [] (text "Passord")
            , label = Input.labelHidden "Passord"
            , show = False
            }
        , Input.button
            [ Background.color Theme.button
            , padding 10
            ]
            { onPress = Just <| GotSignInMsg SignInUser
            , label = text "Logg inn"
            }
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
            Html.select
                [ HtmlE.onInput (GotProfileMsg << ChangedDegree)
                , HtmlA.value <| Maybe.withDefault "" <| Maybe.map (Degree.toString True) model.profileModel.degree
                ]
            <|
                Html.option
                    [ HtmlA.value "" ]
                    [ Html.text "" ]
                    :: List.map
                        (\d ->
                            Html.option
                                [ HtmlA.value <| Degree.toString True d ]
                                [ Html.text <| Degree.toString False d ]
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


updateSession : Model -> Session -> Model
updateSession model session =
    { model | session = session }
