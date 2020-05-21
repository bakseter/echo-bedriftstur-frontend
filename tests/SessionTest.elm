module SessionTest exposing (suite)

import Email exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Decode as Decode
import Json.Encode as Encode
import Session exposing (..)
import Test exposing (..)
import Uid exposing (..)


suite : Test
suite =
    let
        session =
            { uid = Uid "123abc", email = Email "test@test.com" }

        testObject =
            case Session.decode (Session.encode session) of
                Just s ->
                    s

                Nothing ->
                    Session (Uid "") (Email "")

        jsonStr =
            Encode.encode 0 (Session.encode session)

        decodeFunc field =
            Decode.decodeString (Decode.at [ field ] Decode.string)
    in
    describe "Tests for the Session module"
        [ describe "The encode and decode functions work for encoding and decoding the"
            [ test "Uid field" <|
                \_ -> Expect.equal (Uid "123abc") testObject.uid
            , test "Email field" <|
                \_ -> Expect.equal (Email "test@test.com") testObject.email
            ]
        , describe "The encode function works for encoding the"
            [ test "Collections field" <|
                \_ -> Expect.equal (Ok "users") (decodeFunc "collection" jsonStr)
            ]
        , describe "The user is not signed in when" <|
            [ test "the Uid field is empty" <|
                \_ -> Expect.equal False (Session.isSignedIn { uid = Uid "", email = session.email })
            , test "the Email field is empty" <|
                \_ -> Expect.equal False (Session.isSignedIn { uid = session.uid, email = Email "" })
            , test "both the Uid and Email field are empty" <|
                \_ -> Expect.equal False (Session.isSignedIn { uid = Uid "", email = Email "" })
            ]
        , describe "The user is signed in whenn" <|
            [ test "the Uid and Email fields have valid strin values" <|
                \_ -> Expect.equal True (Session.isSignedIn session)
            ]
        ]
