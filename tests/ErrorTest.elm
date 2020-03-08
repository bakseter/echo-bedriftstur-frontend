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
                [ test "TODO1" <|
                    \_ -> Expect.equal (Error (ErrorCode "Det har skjedd en feil. Vennligst prøv igjen")) (Error.errorFromJson (Encode.string "auth/app-deleted"))
                , test "TODO2" <|
                    \_ -> Expect.equal (Error (ErrorCode "Du har ikke tilgang til å utføre denne handlingen")) (Error.errorFromJson (Encode.string "unauthenticated"))
                , test "TODO3" <|
                    \_ -> Expect.equal NoError (Error.errorFromJson (Encode.string "garbage"))
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
