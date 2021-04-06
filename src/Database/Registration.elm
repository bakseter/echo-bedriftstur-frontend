module Database.Registration exposing (Registration, decoder, encode)

import Database.Email as Email exposing (Email)
import Database.Registration.Terms as Terms exposing (Terms)
import Json.Decode as Decode
import Json.Encode as Encode


type alias Registration =
    { email : Email
    , terms : Terms
    , timestamp : Maybe Int
    }


decoder : Decode.Decoder Registration
decoder =
    Decode.map3 Registration
        -- TODO: is this field name correct ???
        (Email.decoder "registrationEmail")
        Terms.decoder
        (Decode.maybe Decode.int)


encode : Registration -> Encode.Value
encode reg =
    Encode.object
        [ ( "registrationEmail", Encode.string <| Email.toString reg.email )
        , ( "registrationTerms", Encode.bool <| Terms.toBool reg.terms )
        ]
