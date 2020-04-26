module UserTest exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Json.Encode as Encode
import Json.Decode as Decode

import User exposing (..)
import Email exposing (..)
import Uid exposing (..)
import Degree exposing (..)
import Terms exposing (..)
import Session exposing (..)
import Ticket exposing (..)

suite : Test
suite =
    let user = User (Email "test@test.com") "Foo" "Bar" (Valid DVIT) (Terms True) (Ticket (Just True)) True
        userJson = Encode.object
                    [ ("email", (Encode.string (Email.toString user.email)))
                    , ("firstName", (Encode.string user.firstName))
                    , ("lastName", (Encode.string user.lastName))
                    , ("degree", (Encode.string (Degree.toString False user.degree)))
                    , ("terms", (Encode.bool True))
                    , ("hasTicket", (Encode.bool True))
                    , ("submittedTicket", (Encode.bool True))
                    , ("garble", (Encode.int 100))
                    , ("moregarble", (Encode.null))
                    ]
        decodedUser = case User.decode userJson of
                        Just u ->
                            u
                        Nothing ->
                            User (Email "") "" "" None (Terms False) (Ticket Nothing) False
    in
        describe "Tests for the User module"
            [ describe "The decode function works for decoding the"
                [ test "email field" <|
                    \_ -> Expect.equal user.email decodedUser.email 
                , test "firstName field" <|
                    \_ -> Expect.equal user.firstName decodedUser.firstName
                , test "lastName field" <|
                    \_ -> Expect.equal user.lastName decodedUser.lastName
                , test "degree field" <|
                    \_ -> Expect.equal user.degree decodedUser.degree
                , test "terms field" <|
                    \_ -> Expect.equal user.terms decodedUser.terms
                , test "hasTicket field" <|
                    \_ -> Expect.equal user.hasTicket decodedUser.hasTicket
                ]
            ]
