module Content exposing (Content, contentDecoder, empty, encode, updateAll, updateDegree, updateFirstName, updateLastName, updateTerms)

import Cred exposing (Cred)
import Degree exposing (Degree(..))
import Json.Decode as Decode
import Json.Encode as Encode
import Terms exposing (Terms(..))
import Uid
import Util



-- Type representing what the user can input


type alias Content =
    { firstName : String
    , lastName : String
    , degree : Degree
    , terms : Terms
    }


empty : Content
empty =
    Content "" "" DTEK (Terms.fromAbsolute False)


updateAll : String -> String -> Degree -> Terms -> Content -> Content
updateAll firstName lastName degree terms =
    updateFirstName firstName
        << updateLastName lastName
        << updateDegree degree
        << updateTerms terms


updateFirstName : String -> Content -> Content
updateFirstName firstName content =
    { content | firstName = firstName }


updateLastName : String -> Content -> Content
updateLastName lastName content =
    { content | lastName = lastName }


updateDegree : Degree -> Content -> Content
updateDegree degree content =
    { content | degree = degree }


updateTerms : Terms -> Content -> Content
updateTerms terms content =
    { content | terms = terms }


contentDecoder : Decode.Decoder Content
contentDecoder =
    Decode.map4 Content
        (Decode.field "firstName" Decode.string)
        (Decode.field "lastName" Decode.string)
        (Decode.map (Maybe.withDefault MISC << Degree.fromString) <| Decode.field "degree" Decode.string)
        (Decode.map Terms.fromAbsolute <| Decode.field "terms" Decode.bool)


encode : Cred -> Content -> Encode.Value
encode cred content =
    Encode.object
        [ ( "collection", Encode.string "users" )
        , ( "uid", Encode.string (Uid.toString cred.uid) )
        , ( "firstName", Encode.string content.firstName )
        , ( "lastName", Encode.string content.lastName )
        , ( "degree", Encode.string (Degree.toString False content.degree) )
        , ( "terms", Encode.bool (Terms.toBool content.terms) )
        ]
