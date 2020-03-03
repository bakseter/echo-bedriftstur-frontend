module Page exposing (..)

import Url
import Url.Parser as Parser exposing ((</>))

type Route
    = Hjem
    | LoggInn
    | Verified
    | Program
    | Bedrifter
    | Om
    | NotFound

urlToRoute : Url.Url -> Route
urlToRoute url =
    case Parser.parse parseRoute url of
        Just route ->
            route
        Nothing ->
            NotFound

parseRoute : Parser.Parser (Route -> b) b
parseRoute =
    Parser.oneOf
        [ Parser.map Hjem Parser.top
        , Parser.map LoggInn (Parser.s "logg-inn")
        , Parser.map Verified (Parser.s "verified")
        , Parser.map Program (Parser.s "program")
        , Parser.map Bedrifter (Parser.s "bedrifter")
        , Parser.map Om (Parser.s "om")
        ]
