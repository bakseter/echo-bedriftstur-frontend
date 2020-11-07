module Error exposing (Error(..), decode, toString)

import Dict exposing (Dict)
import Json.Decode as Decode
import Json.Encode as Encode



-- Type representing an Error from Firebase or Firestore


type Error
    = AuthAppDeleted
    | AuthAppNotAuthorized
    | AuthArgumentError
    | AuthInvalidApiKey
    | AuthInvalidUserToken
    | AuthInvalidTenantId
    | AuthNetworkRequestFailed
    | AuthOperationNotAllowed
    | AuthRequiresRecentLogin
    | AuthTooManyRequests
    | AuthUnauthorizedDomain
    | AuthUserDisabled
    | AuthUserTokenExpired
    | AuthWebStorageUnsupported
    | AuthInvalidEmail
    | AuthExpiredActionCode
    | AuthInvalidActionCode
    | AuthInvalidPersistenceType
    | AuthUnsupportedPersistenceType
    | AuthMissingContinueUri
    | AuthInvalidContinueUri
    | AuthUnauthorizedContinueUri
    | Cancelled
    | Unknown
    | InvalidArgument
    | DeadlineExceeded
    | NotFound
    | AlreadyExists
    | PermissionDenied
    | ResourceExhausted
    | FailedPrecondition
    | Aborted
    | OutOfRange
    | Unimplemented
    | Internal
    | Unavailable
    | DataLoss
    | Unauthenticated
    | JsonParseError
    | NotSignedIn


decode : Encode.Value -> Maybe Error
decode json =
    case Decode.decodeValue errorDecoder json of
        Ok err ->
            err

        Err _ ->
            Nothing


errorDecoder : Decode.Decoder (Maybe Error)
errorDecoder =
    Decode.map fromString Decode.string


fromString : String -> Maybe Error
fromString str =
    case str of
        "auth/app-deleted" ->
            Just AuthAppDeleted

        "auth/app-not-authorized" ->
            Just AuthAppNotAuthorized

        "auth/argument-error" ->
            Just AuthArgumentError

        "auth/invalid-api-key" ->
            Just AuthInvalidApiKey

        "auth/invalid-user-token" ->
            Just AuthInvalidUserToken

        "auth/invalid-tenant-id" ->
            Just AuthInvalidTenantId

        "auth/network-request-failed" ->
            Just AuthNetworkRequestFailed

        "auth/operation-not-allowed" ->
            Just AuthOperationNotAllowed

        "auth/requires-recent-login" ->
            Just AuthRequiresRecentLogin

        "auth/too-many-requests" ->
            Just AuthTooManyRequests

        "auth/unauthorized-domain" ->
            Just AuthUnauthorizedDomain

        "auth/user-disabled" ->
            Just AuthUserDisabled

        "auth/user-token-expired" ->
            Just AuthUserTokenExpired

        "auth/web-storage-unsupported" ->
            Just AuthWebStorageUnsupported

        "auth/invalid-email" ->
            Just AuthInvalidEmail

        "auth/expired-action-code" ->
            Just AuthExpiredActionCode

        "auth/invalid-action-code" ->
            Just AuthInvalidActionCode

        "auth/invalid-persistence-type" ->
            Just AuthInvalidPersistenceType

        "auth/unsupported-persistence-type" ->
            Just AuthUnsupportedPersistenceType

        "auth/missing-continue-uri" ->
            Just AuthMissingContinueUri

        "auth/invalid-continue-uri" ->
            Just AuthInvalidContinueUri

        "auth/unauthorized-continue-uri" ->
            Just AuthUnauthorizedContinueUri

        "cancelled" ->
            Just Cancelled

        "unknown" ->
            Just Unknown

        "invalid-argument" ->
            Just InvalidArgument

        "deadline-exceeded" ->
            Just DeadlineExceeded

        "not-found" ->
            Just NotFound

        "already-exists" ->
            Just AlreadyExists

        "permission-denied" ->
            Just PermissionDenied

        "resource-exhausted" ->
            Just ResourceExhausted

        "failed-precondition" ->
            Just FailedPrecondition

        "aborted" ->
            Just Aborted

        "out-of-range" ->
            Just OutOfRange

        "unimplemented" ->
            Just Unimplemented

        "internal" ->
            Just Internal

        "unavailable" ->
            Just Unavailable

        "data-loss" ->
            Just DataLoss

        "unauthenticated" ->
            Just Unauthenticated

        "json-parse-error" ->
            Just JsonParseError

        "not-signed-in" ->
            Just NotSignedIn

        _ ->
            Nothing


toString : Error -> String
toString error =
    case error of
        AuthAppDeleted ->
            "auth/app-deleted"

        AuthAppNotAuthorized ->
            "auth/app-not-authorized"

        AuthArgumentError ->
            "auth/argument-error"

        AuthInvalidApiKey ->
            "auth/invalid-api-key"

        AuthInvalidUserToken ->
            "auth/invalid-user-token"

        AuthInvalidTenantId ->
            "auth/invalid-tenant-id"

        AuthNetworkRequestFailed ->
            "auth/network-request-failed"

        AuthOperationNotAllowed ->
            "auth/operation-not-allowed"

        AuthRequiresRecentLogin ->
            "auth/requires-recent-login"

        AuthTooManyRequests ->
            "auth/too-many-requests"

        AuthUnauthorizedDomain ->
            "auth/unauthorized-domain"

        AuthUserDisabled ->
            "auth/user-disabled"

        AuthUserTokenExpired ->
            "auth/user-token-expired"

        AuthWebStorageUnsupported ->
            "auth/web-storage-unsupported"

        AuthInvalidEmail ->
            "auth/invalid-email"

        AuthExpiredActionCode ->
            "auth/expired-action-code"

        AuthInvalidActionCode ->
            "auth/invalid-action-code"

        AuthInvalidPersistenceType ->
            "auth/invalid-persistence-type"

        AuthUnsupportedPersistenceType ->
            "auth/unsupported-persistence-type"

        AuthMissingContinueUri ->
            "auth/missing-continue-uri"

        AuthInvalidContinueUri ->
            "auth/invalid-continue-uri"

        AuthUnauthorizedContinueUri ->
            "auth/unauthorized-continue-uri"

        Cancelled ->
            "cancelled"

        Unknown ->
            "unknown"

        InvalidArgument ->
            "invalid-argument"

        DeadlineExceeded ->
            "deadline-exceeded"

        NotFound ->
            "not-found"

        AlreadyExists ->
            "already-exists"

        PermissionDenied ->
            "permission-denied"

        ResourceExhausted ->
            "resource-exhausted"

        FailedPrecondition ->
            "failed-precondition"

        Aborted ->
            "aborted"

        OutOfRange ->
            "out-of-range"

        Unimplemented ->
            "unimplemented"

        Internal ->
            "internal"

        Unavailable ->
            "unavailable"

        DataLoss ->
            "data-loss"

        Unauthenticated ->
            "unauthenticated"

        JsonParseError ->
            "json-parse-error"

        NotSignedIn ->
            "not-signed-in"



{-
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
-}
