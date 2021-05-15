module Database.Account exposing (Account, decoder, empty, encode, updateEmail, updatePassword)

import Database.Account.Password as Password exposing (Password(..))
import Database.Email as Email exposing (Email(..))
import Json.Decode as Decode
import Json.Encode as Encode


type alias Account =
    { email : Email
    , password : Password
    }


empty : Account
empty =
    { email = Email ""
    , password = Password ""
    }


decoder : Decode.Decoder Account
decoder =
    Decode.map2 Account
        -- TODO: is this field name correct ???
        (Email.decoder "accountEmail")
        Password.decoder


encode : Account -> Encode.Value
encode acc =
    -- TODO: are these field names correct ???
    Encode.object
        [ ( "accountEmail", Encode.string <| Email.toString acc.email )
        , ( "accountPassword", Encode.string <| Password.toString acc.password )
        ]


updateEmail : Email -> Account -> Account
updateEmail email acc =
    { acc | email = email }


updatePassword : Password -> Account -> Account
updatePassword password acc =
    { acc | password = password }
