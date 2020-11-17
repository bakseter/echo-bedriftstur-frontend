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



-- Returns an empty Content record


empty : Content
empty =
    Content "" "" DTEK (Terms.fromAbsolute False)



-- Updates every field of the Content record given


updateAll : String -> String -> Degree -> Terms -> Content -> Content
updateAll firstName lastName degree terms =
    updateFirstName firstName
        << updateLastName lastName
        << updateDegree degree
        << updateTerms terms



-- Updates the firstName field of the Content record


updateFirstName : String -> Content -> Content
updateFirstName firstName content =
    { content | firstName = firstName }



-- Updates the lastName field of the Content record


updateLastName : String -> Content -> Content
updateLastName lastName content =
    { content | lastName = lastName }



-- Updates the degree field of the Content record


updateDegree : Degree -> Content -> Content
updateDegree degree content =
    { content | degree = degree }



-- Updates the Terms field of the Content record


updateTerms : Terms -> Content -> Content
updateTerms terms content =
    { content | terms = terms }



{-
   Encodes the Content record as a JSON object.
   This is used to send user input to the Firestore database.
-}


contentDecoder =
    Decode.map4 Content
        (Util.stringOrNullDecoder "firstName")
        (Util.stringOrNullDecoder "lastName")
        (Degree.orNullDecoder "degree")
        (Terms.orNullDecoder "terms")


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
