module ErrorTest exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Json.Encode as Encode
import Json.Decode as Decode

import Error exposing (..)

suite : Test
suite =
    let invalidEmail = (Decode.decodeValue Decode.string (Encode.string "auth/invalid-email"))
    in
        describe "Tests for the Error module"
            [ describe "Tests that check that a JSON error string gets translated from"
                [ test "\"auth/app-deleted\" to \"Det har skjedd en feil. Vennligst prøv igjen (felkode 1).\"" <|
                    \_ -> Expect.equal (Error (ErrorCode "Det har skjedd en feil. Vennligst prøv igjen (feilkode 1).")) (Error.fromJson (Encode.string "auth/app-deleted"))
                , test "\"unauthenticated\" to \"Du har ikke tilgang til å utføre denne handlingen\"" <|
                    \_ -> Expect.equal (Error (ErrorCode "Du har ikke tilgang til å utføre denne handlingen (feilkode 37).")) (Error.fromJson (Encode.string "unauthenticated"))
                , test "a random string to NoError" <|
                    \_ -> Expect.equal NoError (Error.fromJson (Encode.string "garbage"))
                ]
            , describe "TODO:"
                [ test "the ErrorCode \"auth/invalid-email\"" <|
                    \_ ->
                        case invalidEmail of
                            Ok _ ->
                                Expect.pass
                            Err err ->
                                Expect.fail (Decode.errorToString err)
                ]
            ]
