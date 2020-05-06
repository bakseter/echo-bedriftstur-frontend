module Session exposing (Session, empty, isSignedIn, encode, decode)

import Json.Encode as Encode
import Json.Decode as Decode

import Uid exposing (Uid(..))
import Email exposing (Email(..))

{-
    Type representing a user session.
    Basically tells us if the user is logged in or not.
    We get the two field (uid and email) from Firebase when
    it detects that the user has or is logged in.
-}
type alias Session =
    { uid : Uid
    , email : Email
    }

-- Returns an empty Session record
empty : Session
empty =
    Session (Uid "") (Email "")

-- Checks if the user is signed in, aka the Session fields are not empty
isSignedIn : Session -> Bool
isSignedIn session =
    (Uid.toString session.uid) /= "" &&
    (Email.toString session.email) /= ""

-- Encodes a Session type as a JSON object
encode : Session -> Encode.Value
encode user =
    Encode.object
        [ ("collection", Encode.string "users")
        , ("uid", Encode.string (Uid.toString user.uid))
        , ("email", Encode.string (Email.toString user.email))
        ]
                      
-- Uses the userDecoder function to turn
-- a JSON object into a Session record.
decode : Encode.Value -> Maybe Session
decode json =
    let jsonStr = Encode.encode 0 json
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
        (Uid.orNullDecoder "uid")
        (Email.orNullDecoder "email")
