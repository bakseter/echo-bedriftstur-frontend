module Email exposing (Email(..), encode, orNullDecoder, toString)

import Json.Decode as Decode
import Json.Encode as Encode



{-
   Type representing an Email address.
   The email string is wrapped in this type to ensure that an actual
   email string is always given when a function takes an email address as an argument
   If we were to hand a function like this

      sendMessage : Email -> String -> Bool

   a string instead of an Email type, it would never compile in the first place.
   Wrapping the email string like this ensures mistakes like this will never, ever happen.
-}


type Email
    = Email String



-- Converts an Email type to a string


toString : Email -> String
toString (Email email) =
    email



-- Encodes an Email type as a JSON object


encode : Email -> Encode.Value
encode email =
    Encode.object
        [ ( "email", Encode.string (toString email) ) ]



{-
   Decodes an Email type from a JSON value.
   Returns an Email type with the email as a string value inside the Email type if successful.
   Returns an Email type with an empty string if fails.
-}


orNullDecoder : String -> Decode.Decoder Email
orNullDecoder field =
    Decode.oneOf
        [ Decode.map Email (Decode.at [ field ] Decode.string)
        , Decode.at [ field ] (Decode.null (Email ""))
        , Decode.succeed (Email "")
        ]
