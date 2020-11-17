module ErrorTest exposing (suite)

import Error exposing (..)
import Expect
import Json.Encode as Encode
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Tests for the Error module"
        [ describe "Tests that a JSON error string gets translated from"
            [ test "\"auth/app-deleted\" to AuthAppDeleted" <|
                \_ -> Expect.equal (Just AuthAppDeleted) (Error.decode (Encode.string "auth/app-deleted"))
            , test "\"auth/app-not-authorized\" to AuthAppNotAuthorized" <|
                \_ -> Expect.equal (Just AuthAppNotAuthorized) (Error.decode (Encode.string "auth/app-not-authorized"))
            , test "\"auth/argument-error\" to AuthArgumentError" <|
                \_ -> Expect.equal (Just AuthArgumentError) (Error.decode (Encode.string "auth/argument-error"))
            , test "\"auth/invalid-api-key\" to AuthInvalidApiKey" <|
                \_ -> Expect.equal (Just AuthInvalidApiKey) (Error.decode (Encode.string "auth/invalid-api-key"))
            , test "\"auth/invalid-user-token\" to AuthInvalidUserToken" <|
                \_ -> Expect.equal (Just AuthInvalidUserToken) (Error.decode (Encode.string "auth/invalid-user-token"))
            , test "\"auth/invalid-tenant-id\" to AuthInvalidTenantId" <|
                \_ -> Expect.equal (Just AuthInvalidTenantId) (Error.decode (Encode.string "auth/invalid-tenant-id"))
            , test "\"auth/network-request-failed\" to AuthNetworkRequestFailed" <|
                \_ -> Expect.equal (Just AuthNetworkRequestFailed) (Error.decode (Encode.string "auth/network-request-failed"))
            , test "\"auth/operation-not-allowed\" to AuthOperationNotAllowed" <|
                \_ -> Expect.equal (Just AuthOperationNotAllowed) (Error.decode (Encode.string "auth/operation-not-allowed"))
            , test "\"auth/requires-recent-login\" to AuthRequiresRecentLogin" <|
                \_ -> Expect.equal (Just AuthRequiresRecentLogin) (Error.decode (Encode.string "auth/requires-recent-login"))
            , test "\"auth/too-many-requests\" to AuthTooManyRequests" <|
                \_ -> Expect.equal (Just AuthTooManyRequests) (Error.decode (Encode.string "auth/too-many-requests"))
            , test "\"auth/unauthorized-domain\" to AuthUnauthorizedDomain" <|
                \_ -> Expect.equal (Just AuthUnauthorizedDomain) (Error.decode (Encode.string "auth/unauthorized-domain"))
            , test "\"auth/user-disabled\" to AuthUserDisabled" <|
                \_ -> Expect.equal (Just AuthUserDisabled) (Error.decode (Encode.string "auth/user-disabled"))
            , test "\"auth/user-token-expired\" to AuthUserTokenExpired" <|
                \_ -> Expect.equal (Just AuthUserTokenExpired) (Error.decode (Encode.string "auth/user-token-expired"))
            , test "\"auth/web-storage-unsupported\" to AuthWebStorageUnsupported" <|
                \_ -> Expect.equal (Just AuthWebStorageUnsupported) (Error.decode (Encode.string "auth/web-storage-unsupported"))
            , test "\"auth/invalid-email\" to AuthInvalidEmail" <|
                \_ -> Expect.equal (Just AuthInvalidEmail) (Error.decode (Encode.string "auth/invalid-email"))
            , test "\"auth/expired-action-code\" to AuthExpiredActionCode" <|
                \_ -> Expect.equal (Just AuthExpiredActionCode) (Error.decode (Encode.string "auth/expired-action-code"))
            , test "\"auth/invalid-action-code\" to AuthInvalidActionCode" <|
                \_ -> Expect.equal (Just AuthInvalidActionCode) (Error.decode (Encode.string "auth/invalid-action-code"))
            , test "\"auth/invalid-persistence-type\" to AuthInvalidPersistenceType" <|
                \_ -> Expect.equal (Just AuthInvalidPersistenceType) (Error.decode (Encode.string "auth/invalid-persistence-type"))
            , test "\"auth/unsupported-persistence-type\" to AuthUnsupportedPersistenceType" <|
                \_ -> Expect.equal (Just AuthUnsupportedPersistenceType) (Error.decode (Encode.string "auth/unsupported-persistence-type"))
            , test "\"auth/missing-continue-uri\" to AuthMissingContinueUri" <|
                \_ -> Expect.equal (Just AuthMissingContinueUri) (Error.decode (Encode.string "auth/missing-continue-uri"))
            , test "\"auth/invalid-continue-uri\" to AuthInvalidContinueUri" <|
                \_ -> Expect.equal (Just AuthInvalidContinueUri) (Error.decode (Encode.string "auth/invalid-continue-uri"))
            , test "\"auth/unauthorized-continue-uri\" to AuthUnauthorizedContinueUri" <|
                \_ -> Expect.equal (Just AuthUnauthorizedContinueUri) (Error.decode (Encode.string "auth/unauthorized-continue-uri"))
            , test "\"cancelled\" to Cancelled" <|
                \_ -> Expect.equal (Just Cancelled) (Error.decode (Encode.string "cancelled"))
            , test "\"unknown\" to Unknown" <|
                \_ -> Expect.equal (Just Unknown) (Error.decode (Encode.string "unknown"))
            , test "\"invalid-argument\" to InvalidArgument" <|
                \_ -> Expect.equal (Just InvalidArgument) (Error.decode (Encode.string "invalid-argument"))
            , test "\"deadline-exceeded\" to DeadlineExceeded" <|
                \_ -> Expect.equal (Just DeadlineExceeded) (Error.decode (Encode.string "deadline-exceeded"))
            , test "\"not-found\" to NotFound" <|
                \_ -> Expect.equal (Just NotFound) (Error.decode (Encode.string "not-found"))
            , test "\"already-exists\" to AlreadyExists" <|
                \_ -> Expect.equal (Just AlreadyExists) (Error.decode (Encode.string "already-exists"))
            , test "\"permission-denied\" to PermissionDenied" <|
                \_ -> Expect.equal (Just PermissionDenied) (Error.decode (Encode.string "permission-denied"))
            , test "\"resource-exhausted\" to ResourceExhausted" <|
                \_ -> Expect.equal (Just ResourceExhausted) (Error.decode (Encode.string "resource-exhausted"))
            , test "\"failed-precondition\" to FailedPrecondition" <|
                \_ -> Expect.equal (Just FailedPrecondition) (Error.decode (Encode.string "failed-precondition"))
            , test "\"aborted\" to Aborted" <|
                \_ -> Expect.equal (Just Aborted) (Error.decode (Encode.string "aborted"))
            , test "\"out-of-range\" to OutOfRange" <|
                \_ -> Expect.equal (Just OutOfRange) (Error.decode (Encode.string "out-of-range"))
            , test "\"unimplemented\" to Unimplemented" <|
                \_ -> Expect.equal (Just Unimplemented) (Error.decode (Encode.string "unimplemented"))
            , test "\"internal\" to Internal" <|
                \_ -> Expect.equal (Just Internal) (Error.decode (Encode.string "internal"))
            , test "\"unavailable\" to Unavailable" <|
                \_ -> Expect.equal (Just Unavailable) (Error.decode (Encode.string "unavailable"))
            , test "\"data-loss\" to DataLoss" <|
                \_ -> Expect.equal (Just DataLoss) (Error.decode (Encode.string "data-loss"))
            , test "\"unauthenticated\" to Unauthenticated" <|
                \_ -> Expect.equal (Just Unauthenticated) (Error.decode (Encode.string "unauthenticated"))
            , test "\"json-parse-error\" to JsonParseError" <|
                \_ -> Expect.equal (Just JsonParseError) (Error.decode (Encode.string "json-parse-error"))
            , test "\"not-signed-in\" to NotSignedIn" <|
                \_ -> Expect.equal (Just NotSignedIn) (Error.decode (Encode.string "not-signed-in"))
            , test "a random string to Nothing" <|
                \_ -> Expect.equal Nothing (Error.decode (Encode.string "garbage"))
            ]
        ]
