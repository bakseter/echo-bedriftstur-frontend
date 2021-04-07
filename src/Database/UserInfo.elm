module Database.UserInfo exposing (UserInfo, decoder, empty, encode, updateDegree, updateEmail, updateFirstName, updateLastName)

import Database.Email as Email exposing (Email(..))
import Database.UserInfo.Degree as Degree exposing (Degree(..))
import Json.Decode as Decode
import Json.Encode as Encode


type alias UserInfo =
    { email : Email
    , firstName : String
    , lastName : String
    , degree : Degree
    }


empty : UserInfo
empty =
    { email = Email ""
    , firstName = ""
    , lastName = ""
    , degree = MISC
    }


decoder : Decode.Decoder UserInfo
decoder =
    Decode.map4 UserInfo
        (Email.decoder "userInfoEmail")
        (Decode.field "userInfoFirstName" Decode.string)
        (Decode.field "userInfoLastName" Decode.string)
        Degree.decoder


encode : UserInfo -> Encode.Value
encode user =
    Encode.object
        [ ( "userInfoFirstName", Encode.string user.firstName )
        , ( "userInfoLastName", Encode.string user.lastName )
        , ( "userInfoEmail", Encode.string <| Email.toString user.email )
        , ( "userInfoDegree", Encode.string <| Tuple.first <| Degree.toString user.degree )
        ]


updateEmail : Email -> UserInfo -> UserInfo
updateEmail email userInfo =
    { userInfo | email = email }


updateFirstName : String -> UserInfo -> UserInfo
updateFirstName firstName userInfo =
    { userInfo | firstName = firstName }


updateLastName : String -> UserInfo -> UserInfo
updateLastName lastName userInfo =
    { userInfo | lastName = lastName }


updateDegree : Degree -> UserInfo -> UserInfo
updateDegree degree userInfo =
    { userInfo | degree = degree }
