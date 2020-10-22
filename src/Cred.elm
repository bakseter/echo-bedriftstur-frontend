module Cred exposing (Cred, decode, encode, isSignedIn)

import Email exposing (Email(..))
import Json.Decode as Decode
import Json.Encode as Encode
import Uid exposing (Uid(..))


type alias Cred =
    { uid : Uid
    , email : Email
    }



-- Checks if the user is signed in, aka the Session fields are not empty


isSignedIn : Maybe Cred -> Bool
isSignedIn cred =
    case cred of
        Just _ ->
            True

        Nothing ->
            False


encode : Cred -> Encode.Value
encode cred =
    Encode.object
        [ ( "collection", Encode.string "users" )
        , ( "uid", Encode.string (Uid.toString cred.uid) )
        , ( "email", Encode.string (Email.toString cred.email) )
        ]



-- Uses the userDecoder function to turn
-- a JSON object into a Cred record.


decode : Encode.Value -> Maybe Cred
decode json =
    let
        jsonStr =
            Encode.encode 0 json
    in
    case Decode.decodeString credDecoder jsonStr of
        Ok cred ->
            Just cred

        Err _ ->
            Nothing



-- Decodes a JSON object into a Cred type.
-- Uses the decoders in the Uid and Email modules.


credDecoder : Decode.Decoder Cred
credDecoder =
    Decode.map2 Cred
        (Uid.orNullDecoder "uid")
        (Email.orNullDecoder "email")
