module Error exposing (..)

import Json.Encode as Encode
import Json.Decode as Decode
import Dict exposing (Dict)

type Error
    = Error ErrorCode
    | NoError

type ErrorCode
    = ErrorCode String

toString : Error -> String
toString error =
    case error of
        Error (ErrorCode str) ->
            str
        NoError ->
            ""

errorFromJson : Encode.Value -> Error
errorFromJson json =
    let jsonStr = Encode.encode 0 json
        maybeCode = Decode.decodeString Decode.string jsonStr
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

errorMsgList : Dict String String
errorMsgList =
    let stdMsg = "Det har skjedd en feil. Vennligst prøv igjen"
    in
        Dict.fromList
            [ ("auth/app-deleted", stdMsg)
            , ("auth/app-not-authorized", stdMsg)
            , ("auth/argument-error", stdMsg)
            , ("auth/invalid-api-key", stdMsg)
            , ("auth/invalid-user-token", "Du er ikke logget inn. Vennligst logg inn og prøv på nytt")
            , ("auth/invalid-tenant-id", stdMsg)
            , ("auth/network-request-failed", "Det har skjedd en nettverksfeil. Vennligst sjekk at du er koblet til internett og prøv igjen")
            , ("auth/operation-not-allowed", stdMsg)
            , ("auth/requires-recent-login", stdMsg)
            , ("auth/too-many-requests", "Du har prøvd å logge inn for mange ganger på kort tid. Vennligst vent noen minutter og prøv igjen")
            , ("auth/unauthorized-domain", stdMsg)
            , ("auth/user-disabled", "Brukeren din har blitt deaktivert.")
            , ("auth/user-token-expired", "Du har blitt logget ut. Vennligst logg inn og prøv på nytt")
            , ("auth/web-storage-unsupported", "Du har deaktivert Web Storage. Vennligst aktiver det og prøv på nytt")
            , ("auth/invalid-email", "Mailen du har skrevet inn er ikke gyldig. Skriv inn en gyldig studentmail og prøv igjen")
            , ("auth/expired-action-code", "Innlogginslinken er ikke gyldig lenger. Prøv å logg inn på nytt")
            , ("auth/invalid-persistence-type", stdMsg)
            , ("auth/unsupported-persistence-type", stdMsg)
            , ("auth/missing-continue-uri", stdMsg)
            , ("auth/invalid-continue-uri", stdMsg)
            , ("auth/unauthorized-continue-uri", stdMsg)
            , ("cancelled", stdMsg)
            , ("unknown", stdMsg)
            , ("invalid-argument", "Det du har skrevet inn er ikke gyldig")
            , ("deadline-exceeded", stdMsg)
            , ("not-found", stdMsg)
            , ("already-exists", stdMsg)
            , ("permission-denied", "Du har ikke tilstrekkelig tilgang til å gjøre dette")
            , ("resource-exhausted", stdMsg)
            , ("failed-precondition", stdMsg)
            , ("aborted", stdMsg)
            , ("out-of-range", stdMsg)
            , ("unimplemented", stdMsg)
            , ("internal", "Noe har gått veldig galt. Vennligst kontakt oss på kontakt@echobedriftstur.no")
            , ("unavailable", "Tjenesten er ikke tilgjengelig akkurat nå. Vennligst prøv igjen senere")
            , ("data-loss", stdMsg)
            , ("unauthenticated", "Du har ikke tilgang til å utføre denne handlingen")
            ]
