module Page.Profil exposing (..)

import Api
import Database.Account as Account exposing (Account)
import Database.Account.Password as Password exposing (Password(..))
import Database.Email as Email exposing (Email(..))
import Database.Registration exposing (Registration)
import Database.Registration.Terms as Terms
import Database.UserInfo as UserInfo exposing (UserInfo)
import Database.UserInfo.Degree as Degree exposing (Degree(..))
import Element exposing (..)
import Element.Font as Font
import Element.Input as Input
import Http
import Session exposing (Session)
import Theme


type Msg
    = GotSignUpMsg SignUpMsg
    | GotSignInMsg SignInMsg
    | GotUserDashboardMsg UserDashboardMsg
    | GotAdminDashboardMsg AdminDashboardMsg
    | Toggle
    | NoOp


type SignUpMsg
    = CreateAccount
    | CreateAccountResponse (Result Http.Error ())
    | SignUpTypedEmail String
    | SignUpTypedPassword String
    | SignUpTypedFirstName String
    | SignUpTypedLastName String
    | SignUpTypedDegree String


type SignInMsg
    = SignInUser
      --    | SignInUserResponse (Result Http.Error ())
    | SignInTypedEmail String
    | SignInTypedPassword String


type UserDashboardMsg
    = UpdateUserInfo
    | UpdateUserInfoResponse (Result Http.Error ())
    | UserDashTypedFirstName String
    | UserDashTypedLastName String
    | UserDashTypedDegree String


type AdminDashboardMsg
    = GetAllAccounts
    | GetAllAccountsResponse (Result Http.Error (List Account))
    | GetAllUserInfo
    | GetAllUserInfoResponse (Result Http.Error (List UserInfo))
    | GetAllRegistrations
    | GetAllRegistrationsResponse (Result Http.Error (List Registration))


type alias Model =
    { session : Session
    , signUpModel : SignUpModel
    , signInModel : SignInModel
    , userDashboard : UserDashboardModel
    , adminDashboard : AdminDashboardModel
    , subpage : Subpage
    }


type Subpage
    = SignUp
    | SignIn
    | UserDashboard
    | AdminDashboard


type SignUpModel
    = SignUpModel Account UserInfo


type alias SignInModel =
    Account


type alias UserDashboardModel =
    UserInfo


type AdminDashboardModel
    = AdminDashboardModel (List Account) (List UserInfo) (List Registration)


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , signUpModel = SignUpModel Account.empty UserInfo.empty
      , signInModel = Account.empty
      , userDashboard = UserInfo.empty
      , adminDashboard = AdminDashboardModel [] [] []
      , subpage = SignUp
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSignUpMsg m ->
            updateSignUp m model

        GotSignInMsg m ->
            updateSignIn m model

        GotUserDashboardMsg m ->
            updateUserDash m model

        GotAdminDashboardMsg m ->
            updateAdminDash m model

        Toggle ->
            let
                newSubpage =
                    case model.subpage of
                        SignUp ->
                            SignIn

                        SignIn ->
                            UserDashboard

                        UserDashboard ->
                            AdminDashboard

                        AdminDashboard ->
                            SignUp
            in
            ( { model | subpage = newSubpage }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


updateSignUp : SignUpMsg -> Model -> ( Model, Cmd Msg )
updateSignUp msg model =
    let
        (SignUpModel acc userInfo) =
            model.signUpModel
    in
    case msg of
        CreateAccount ->
            ( model, Api.createAccount acc userInfo (GotSignUpMsg << CreateAccountResponse) )

        CreateAccountResponse resp ->
            case resp of
                Ok _ ->
                    ( model, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        SignUpTypedEmail str ->
            ( { model
                | signUpModel =
                    SignUpModel
                        (Account.updateEmail (Email str) acc)
                        (UserInfo.updateEmail (Email str) userInfo)
              }
            , Cmd.none
            )

        SignUpTypedPassword str ->
            ( { model | signUpModel = SignUpModel (Account.updatePassword (Password str) acc) userInfo }, Cmd.none )

        SignUpTypedFirstName str ->
            ( { model | signUpModel = SignUpModel acc (UserInfo.updateFirstName str userInfo) }, Cmd.none )

        SignUpTypedLastName str ->
            ( { model | signUpModel = SignUpModel acc (UserInfo.updateLastName str userInfo) }, Cmd.none )

        SignUpTypedDegree str ->
            case Degree.fromString str of
                Just deg ->
                    ( { model | signUpModel = SignUpModel acc (UserInfo.updateDegree deg userInfo) }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )


updateSignIn : SignInMsg -> Model -> ( Model, Cmd Msg )
updateSignIn msg model =
    case msg of
        SignInUser ->
            ( model, Cmd.none )

        {-
           SignInUserResponse _ ->
               ( model, Cmd.none )
        -}
        SignInTypedEmail str ->
            ( { model | signInModel = Account.updateEmail (Email str) model.signInModel }, Cmd.none )

        SignInTypedPassword str ->
            ( { model | signInModel = Account.updatePassword (Password str) model.signInModel }, Cmd.none )


updateUserDash : UserDashboardMsg -> Model -> ( Model, Cmd Msg )
updateUserDash msg model =
    let
        userInfo =
            model.userDashboard
    in
    case msg of
        UpdateUserInfo ->
            ( model, Api.updateUserInfo userInfo (GotUserDashboardMsg << UpdateUserInfoResponse) )

        UpdateUserInfoResponse response ->
            case response of
                Ok _ ->
                    ( model, Cmd.none )

                Err _ ->
                    -- ( Debug.log ("Error @ UpdateUserInfoResponse: " ++ Debug.toString err) model, Cmd.none )
                    ( model, Cmd.none )

        UserDashTypedFirstName str ->
            ( { model | userDashboard = UserInfo.updateFirstName str userInfo }, Cmd.none )

        UserDashTypedLastName str ->
            ( { model | userDashboard = UserInfo.updateLastName str userInfo }, Cmd.none )

        UserDashTypedDegree str ->
            case Degree.fromString str of
                Just deg ->
                    ( { model | userDashboard = UserInfo.updateDegree deg userInfo }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )


updateAdminDash : AdminDashboardMsg -> Model -> ( Model, Cmd Msg )
updateAdminDash msg model =
    let
        (AdminDashboardModel accs userInfo regs) =
            model.adminDashboard
    in
    case msg of
        GetAllAccounts ->
            ( model, Api.getAllAccounts (GotAdminDashboardMsg << GetAllAccountsResponse) )

        GetAllAccountsResponse response ->
            case response of
                Ok newAccs ->
                    ( { model | adminDashboard = AdminDashboardModel newAccs userInfo regs }, Cmd.none )

                Err _ ->
                    -- ( Debug.log ("Error @ GetAllAccountsResponse: " ++ Debug.toString err) model, Cmd.none )
                    ( model, Cmd.none )

        GetAllUserInfo ->
            ( model, Api.getAllUserInfo (GotAdminDashboardMsg << GetAllUserInfoResponse) )

        GetAllUserInfoResponse response ->
            case response of
                Ok newUserInfo ->
                    ( { model | adminDashboard = AdminDashboardModel accs newUserInfo regs }, Cmd.none )

                Err _ ->
                    -- ( Debug.log ("Error @ GetAllUserInfoResponse: " ++ Debug.toString err) model, Cmd.none )
                    ( model, Cmd.none )

        GetAllRegistrations ->
            ( model, Api.getAllRegistrations (GotAdminDashboardMsg << GetAllRegistrationsResponse) )

        GetAllRegistrationsResponse response ->
            case response of
                Ok newRegs ->
                    ( { model | adminDashboard = AdminDashboardModel accs userInfo newRegs }, Cmd.none )

                Err _ ->
                    -- ( Debug.log ("Error @ GetAllRegistrationsResponse: " ++ Debug.toString err) model, Cmd.none )
                    ( model, Cmd.none )


view : Model -> Element Msg
view model =
    case model.subpage of
        SignUp ->
            viewSignUp model

        SignIn ->
            viewSignIn model

        UserDashboard ->
            viewUserDash model

        AdminDashboard ->
            viewAdminDash model


toggleHeader : Element Msg
toggleHeader =
    row [ centerX ]
        [ Theme.button
            { onPress = Just <| Toggle
            , label = text "Toggle"
            }
        ]


viewSignUp : Model -> Element Msg
viewSignUp model =
    let
        (SignUpModel acc userInfo) =
            model.signUpModel
    in
    column [ centerX, spacing 50 ]
        [ toggleHeader
        , el [ Font.bold, Font.size 32, centerX ] <| text "Sign up"
        , Input.email [ Input.focusedOnLoad ]
            { onChange = GotSignUpMsg << SignUpTypedEmail
            , text = Email.toString acc.email
            , placeholder = Just <| Input.placeholder [] (text "Email")
            , label = Input.labelHidden "Email"
            }
        , Input.newPassword []
            { onChange = GotSignUpMsg << SignUpTypedPassword
            , text = Password.toString acc.password
            , placeholder = Just <| Input.placeholder [] (text "Nytt passord")
            , label = Input.labelHidden "Nytt passord"
            , show = False
            }
        , Input.text []
            { onChange = GotSignUpMsg << SignUpTypedFirstName
            , text = userInfo.firstName
            , placeholder = Just <| Input.placeholder [] (text "Fornavn")
            , label = Input.labelHidden "Fornavn"
            }
        , Input.text []
            { onChange = GotSignUpMsg << SignUpTypedLastName
            , text = userInfo.lastName
            , placeholder = Just <| Input.placeholder [] (text "Etternavn")
            , label = Input.labelHidden "Etternavn"
            }
        , Theme.select
            { onInput = GotSignUpMsg << SignUpTypedDegree
            , value = Tuple.first <| Degree.toString userInfo.degree
            , options =
                List.map Degree.toString
                    [ DTEK, DSIK, DVIT, BINF, IMØ, IKT, KOGNI, INF, PROG, POST, MISC ]
            , addEmpty = True
            }
        , Theme.button
            { onPress = Just <| GotSignUpMsg CreateAccount
            , label = text "Lag ny bruker"
            }
        ]


viewSignIn : Model -> Element Msg
viewSignIn model =
    let
        acc =
            model.signInModel
    in
    column [ centerX, spacing 50 ]
        [ toggleHeader
        , el [ Font.bold, Font.size 32, centerX ] <| text "Sign in"
        , Input.email [ Input.focusedOnLoad ]
            { onChange = GotSignInMsg << SignInTypedEmail
            , text = Email.toString acc.email
            , placeholder = Just <| Input.placeholder [] (text "Email")
            , label = Input.labelHidden "Email"
            }
        , Input.currentPassword []
            { onChange = GotSignInMsg << SignInTypedPassword
            , text = Password.toString acc.password
            , placeholder = Just <| Input.placeholder [] (text "Passord")
            , label = Input.labelHidden "Passord"
            , show = False
            }
        , Theme.button
            { onPress = Just <| GotSignInMsg SignInUser
            , label = text "Logg inn"
            }
        ]


viewUserDash : Model -> Element Msg
viewUserDash model =
    let
        userInfo =
            model.userDashboard
    in
    column [ centerX, spacing 50 ]
        [ toggleHeader
        , el [ Font.bold, Font.size 32, centerX ] <| text "User dashboard"
        , Input.email []
            { onChange = \_ -> NoOp
            , text = ""
            , placeholder = Just <| Input.placeholder [] <| text <| Email.toString userInfo.email
            , label = Input.labelHidden "Email"
            }
        , Input.text []
            { onChange = GotUserDashboardMsg << UserDashTypedFirstName
            , text = userInfo.firstName
            , placeholder = Just <| Input.placeholder [] (text "Fornavn")
            , label = Input.labelHidden "Fornavn"
            }
        , Input.text []
            { onChange = GotUserDashboardMsg << UserDashTypedLastName
            , text = userInfo.lastName
            , placeholder = Just <| Input.placeholder [] (text "Etternavn")
            , label = Input.labelHidden "Etternavn"
            }
        , Theme.select
            { onInput = GotUserDashboardMsg << UserDashTypedDegree
            , value = Tuple.first <| Degree.toString userInfo.degree
            , options =
                List.map Degree.toString
                    [ DTEK, DSIK, DVIT, BINF, IMØ, IKT, KOGNI, INF, PROG, POST, MISC ]
            , addEmpty = True
            }
        , Theme.button
            { onPress = Just <| GotUserDashboardMsg UpdateUserInfo
            , label = text "Oppdater informasjon"
            }
        ]


viewAdminDash : Model -> Element Msg
viewAdminDash model =
    let
        (AdminDashboardModel accs userInfo regs) =
            model.adminDashboard
    in
    column [ centerX, spacing 50 ]
        [ toggleHeader
        , el [ Font.bold, Font.size 32, centerX ] <| text "Admin dashboard"
        , table [ spacing 20, centerX ]
            { data = accs
            , columns =
                [ { header = el [ Font.bold ] <| text "Email"
                  , width = fill
                  , view = \acc -> text <| Email.toString acc.email
                  }
                , { header = el [ Font.bold ] <| text "Passord"
                  , width = fill
                  , view = \acc -> text <| Password.toString acc.password
                  }
                ]
            }
        , table [ spacing 20, centerX ]
            { data = userInfo
            , columns =
                [ { header = el [ Font.bold ] <| text "Email"
                  , width = fill
                  , view = \info -> text <| Email.toString info.email
                  }
                , { header = el [ Font.bold ] <| text "Fornavn"
                  , width = fill
                  , view = \info -> text info.firstName
                  }
                , { header = el [ Font.bold ] <| text "Etternavn"
                  , width = fill
                  , view = \info -> text info.lastName
                  }
                , { header = el [ Font.bold ] <| text "Studieretning"
                  , width = fill
                  , view = \info -> text <| Tuple.first <| Degree.toString info.degree
                  }
                ]
            }
        , table [ spacing 20, centerX ]
            { data = regs
            , columns =
                [ { header = el [ Font.bold ] <| text "Email"
                  , width = fill
                  , view = \reg -> text <| Email.toString reg.email
                  }
                , { header = el [ Font.bold ] <| text "Godkjent betingelser"
                  , width = fill
                  , view =
                        \reg ->
                            if Terms.toBool reg.terms then
                                text "Ja"

                            else
                                text "Nei"
                  }
                , { header = el [ Font.bold ] <| text "Tidspunkt"
                  , width = fill
                  , view =
                        \reg ->
                            case reg.timestamp of
                                Just timestamp ->
                                    text (String.fromInt timestamp)

                                Nothing ->
                                    text "N/A"
                  }
                ]
            }
        , row [ spacing 30, centerX ]
            [ Theme.button
                { onPress = Just <| GotAdminDashboardMsg GetAllAccounts
                , label = text "Get accounts"
                }
            , Theme.button
                { onPress = Just <| GotAdminDashboardMsg GetAllUserInfo
                , label = text "Get user info"
                }
            , Theme.button
                { onPress = Just <| GotAdminDashboardMsg GetAllRegistrations
                , label = text "Get registrations"
                }
            ]
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
