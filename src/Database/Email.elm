module Database.Email exposing (Email(..), decoder, isValid, toString)

import Json.Decode as Decode



{-
   Type representing an Email address.
   The email string is wrapped in this type to ensure that an actual
   email is always given when a function takes an email address as an argument.
   If we were to hand a function like this

      sendMessage : Email -> Bool

   a string instead of an Email type, it would never compile in the first place.
   Wrapping the email string like this ensures mistakes like this will never, ever happen.
-}


type Email
    = Email String


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


decoder : String -> Decode.Decoder Email
decoder field =
    Decode.map Email <| Decode.field field Decode.string
