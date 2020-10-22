module Cred exposing (Session, decode, encode, isSignedIn)

import Browser.Navigation
import Email exposing (Email(..))
import Json.Decode as Decode
import Json.Encode as Encode
import Uid exposing (Uid(..))



{-
   Type representing a user session.
   Basically tells us if the user is logged in or not.
   We get the two field (uid and email) from Firebase when
   it detects that the user has or is logged in.
-}


type alias Session =
    { navKey : Browser.Navigation.Key
    , apiKey : String
    }



-- Checks if the user is signed in, aka the Session fields are not empty


isSignedIn : Session -> Bool
isSignedIn session =
    case ( session.uid, session.email ) of
        ( Just _, Just _ ) ->
            True

        _ ->
            False



-- Encodes a Session type as a JSON object


encode : Session -> Maybe Encode.Value
encode session =
    case ( session.uid, session.email ) of
        ( Just uid, Just email ) ->
            Just <|
                Encode.object
                    [ ( "collection", Encode.string "users" )
                    , ( "uid", Encode.string (Uid.toString uid) )
                    , ( "email", Encode.string (Email.toString email) )
                    ]

        _ ->
            Nothing



-- Uses the userDecoder function to turn
-- a JSON object into a Session record.


decode : Encode.Value -> Maybe Session
decode json =
    let
        jsonStr =
            Encode.encode 0 json
    in
    case Decode.decodeString sessionDecoder jsonStr of
        Ok session ->
            Just session

        Err _ ->
            Nothing



-- Decodes a JSON object into a Session type.
-- Uses the decoders in the Uid and Email modules.


sessionDecoder : Decode.Decoder Session
sessionDecoder =
    Decode.map2 Session
        (Decode.map Just <| Uid.orNullDecoder "uid")
        (Decode.map Just <| Email.orNullDecoder "email")
