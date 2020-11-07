module SessionTest exposing (suite)

import Cred exposing (..)
import Email exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Decode as Decode
import Json.Encode as Encode
import Test exposing (..)
import Uid exposing (..)


suite : Test
suite =
    let
        cred =
            { uid = Uid "123abc", email = Email "test@test.com" }

        testObject =
            case Cred.decode (Cred.encode cred) of
                Just s ->
                    s

                Nothing ->
                    Cred (Uid "") (Email "")

        jsonStr =
            Encode.encode 0 (Cred.encode cred)

        decodeFunc field =
            Decode.decodeString (Decode.at [ field ] Decode.string)
    in
    describe "Tests for the Cred module"
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
        ]
