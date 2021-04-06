module Database.Account.Password exposing (Password(..), decoder, toString)

import Json.Decode as Decode


type Password
    = Password String


toString : Password -> String
toString (Password str) =
    str


decoder : Decode.Decoder Password
decoder =
    -- TODO: is this correct fieldname ???
    Decode.map Password <| Decode.field "accountPassword" Decode.string
