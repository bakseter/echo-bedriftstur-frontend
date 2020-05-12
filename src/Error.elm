module Error exposing (Error(..), ErrorCode(..), encode, fromJson, toString)

import Dict exposing (Dict)
import Json.Decode as Decode
import Json.Encode as Encode



-- Type representing an Error from Firebase or Firestore


type Error
    = Error ErrorCode
    | NoError



-- Type wrapper for the error codes


type ErrorCode
    = ErrorCode String



-- Converts an Error type to a error string


toString : Error -> String
toString error =
    case error of
        Error (ErrorCode str) ->
            str

        NoError ->
            ""



-- Encodes the error string as a JSON value


encode : String -> Encode.Value
encode str =
    Encode.string str



-- Decodes an error string in JSON format into an Error type.
-- Used when receiving error codes from Firebase or Firestore.


fromJson : Encode.Value -> Error
fromJson json =
    let
        jsonStr =
            Encode.encode 0 json

        maybeCode =
            Decode.decodeString Decode.string jsonStr
    in
    case maybeCode of
        Ok code ->
            case Dict.get code errorMsgList of
                Just str ->
                    Error (ErrorCode str)

                Nothing ->
                    NoError

        Err err ->
            NoError



-- Massive list of (almost) all the error codes that Firebase or Firestore returns.
-- All the error codes are paired with what is displayed to the user when the error occurs.


errorMsgList : Dict String String
errorMsgList =
    let
        stdMsg =
            "Det har skjedd en feil. Vennligst prøv igjen "
    in
    Dict.fromList
        [ ( "auth/app-deleted", stdMsg ++ "(feilkode 1)." )
        , ( "auth/app-not-authorized", stdMsg ++ "(feilkode 2)." )
        , ( "auth/argument-error", stdMsg ++ "(feilkode 3)." )
        , ( "auth/invalid-api-key", stdMsg ++ "(feilkode 4)." )
        , ( "auth/invalid-user-token", "Du er ikke logget inn. Vennligst logg inn og prøv på nytt (feilkode 5)." )
        , ( "auth/invalid-tenant-id", stdMsg ++ "(feilkode 6)." )
        , ( "auth/network-request-failed", "Det har skjedd en nettverksfeil. Vennligst sjekk at du er koblet til internett og prøv igjen (feilkode 7)." )
        , ( "auth/operation-not-allowed", stdMsg ++ "(feilkode 8)." )
        , ( "auth/requires-recent-login", stdMsg ++ "(feilkode 9)." )
        , ( "auth/too-many-requests", "Du har prøvd å logge inn for mange ganger på kort tid. Vennligst vent noen minutter og prøv igjen (feilkode 10)." )
        , ( "auth/unauthorized-domain", stdMsg ++ "(feilkode 11)." )
        , ( "auth/user-disabled", "Brukeren din har blitt deaktivert. Vennligst kontakt med på kontakt@echobedriftstur.no for mer informasjon (feilkode 12)." )
        , ( "auth/user-token-expired", "Du har blitt logget ut. Vennligst logg inn og prøv på nytt (feilkode 13)." )
        , ( "auth/web-storage-unsupported", "Du har deaktivert Web Storage. Vennligst aktiver det og prøv på nytt (feilkode 14)." )
        , ( "auth/invalid-email", "Mailen du har skrevet inn er ikke gyldig. Skriv inn en gyldig studentmail og prøv på nytt (feilkode 15)." )
        , ( "auth/expired-action-code", "Innlogginslinken har utløpt, og er ikke gyldig lenger. Prøv å logg inn på nytt (feilkode 16)." )
        , ( "auth/invalid-action-code", "Innlogginslinken er ikke gyldig. Prøv å logg inn på nytt (feilkode 17)." )
        , ( "auth/invalid-persistence-type", stdMsg ++ "(feilkode 18)." )
        , ( "auth/unsupported-persistence-type", stdMsg ++ "(feilkode 19)." )
        , ( "auth/missing-continue-uri", stdMsg ++ "(feilkode 20)." )
        , ( "auth/invalid-continue-uri", stdMsg ++ "(feilkode 21)." )
        , ( "auth/unauthorized-continue-uri", stdMsg ++ "(feilkode 22)." )
        , ( "cancelled", stdMsg ++ "(feilkode 23)." )
        , ( "unknown", stdMsg ++ "(feilkode 24)." )
        , ( "invalid-argument", "Det du har skrevet inn er ikke gyldig (feilkode 25)." )
        , ( "deadline-exceeded", stdMsg ++ "(feilkode 26)." )
        , ( "not-found", stdMsg ++ "(feilkode 27)." )
        , ( "already-exists", stdMsg ++ "(feilkode 28)." )
        , ( "permission-denied", "Du har ikke tilstrekkelig tilgang til å gjøre dette (feilkode 29)." )
        , ( "resource-exhausted", stdMsg ++ "(feilkode 30)." )
        , ( "failed-precondition", stdMsg ++ "(feilkode 31)." )
        , ( "aborted", stdMsg ++ "(feilkode 32)." )
        , ( "out-of-range", stdMsg ++ "(feilkode 33)." )
        , ( "unimplemented", stdMsg ++ "(feilkode 34)." )
        , ( "internal", "Det har skjedd en feil. Vennligst kontakt oss på kontakt@echobedriftstur.no (feilkode 35)." )
        , ( "unavailable", "Tjenesten er ikke tilgjengelig akkurat nå. Vennligst prøv igjen senere (feilkode 36)." )
        , ( "data-loss", stdMsg ++ "(feilkode 37)." )
        , ( "unauthenticated", "Du har ikke tilgang til å utføre denne handlingen (feilkode 38)." )
        , ( "json-parse-error", "Det har skjedd en feil. Vennligst kontakt oss på kontakt@echobedriftstur.no (feilkode 40)." )
        , ( "not-signed-in", "Du er ikke logget inn. Vennligst logg inn og prøv på nytt (feilkode 41)." )
        ]
