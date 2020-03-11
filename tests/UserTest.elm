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
import Session exposing (..)

suite : Test
suite =
    let user = { email = Email "test@test.com", firstName = "Foo", lastName = "Bar", degree = Valid DVIT }  
        userJson = Encode.object
                    [ ("email", (Encode.string (Email.toString user.email)))
                    , ("firstName", (Encode.string user.firstName))
                    , ("lastName", (Encode.string user.lastName))
                    , ("degree", (Encode.string (Degree.toString False user.degree)))
                    , ("garble", (Encode.int 100))
                    , ("moregarble", (Encode.null))
                    ]
        decodedUser = User.decode userJson
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
                ]
            ]
