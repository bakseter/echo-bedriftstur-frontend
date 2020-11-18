module Email exposing (Email(..), isValid, orNullDecoder, toString)

import Json.Decode as Decode



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
toString (Email str) =
    str


isValid : Email -> Bool
isValid (Email str) =
    let
        validEnding =
            "@student.uib.no"
    in
    String.right (String.length validEnding) str == validEnding && String.length str > String.length validEnding


orNullDecoder : String -> Decode.Decoder Email
orNullDecoder field =
    Decode.oneOf
        [ Decode.map Email <| Decode.at [ field ] Decode.string
        , Decode.at [ field ] (Decode.null (Email ""))
        , Decode.succeed (Email "")
        ]
