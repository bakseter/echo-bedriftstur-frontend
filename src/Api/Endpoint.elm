module Api.Endpoint exposing (Endpoint(..), request, toString)

import Http


type Endpoint
    = Authentication String
    | Database String


request :
    { a
        | body : Http.Body
        , expect : Http.Expect msg
        , headers : List Http.Header
        , method : String
        , timeout : Maybe Float
        , tracker : Maybe String
        , url : Endpoint
    }
    -> Cmd msg
request data =
    Http.request
        { method = data.method
        , headers = data.headers
        , url = toString data.url
        , body = data.body
        , expect = data.expect
        , timeout = data.timeout
        , tracker = data.tracker
        }


toString : Endpoint -> String
toString endpoint =
    case endpoint of
        Authentication auth ->
            auth

        Database db ->
            db
