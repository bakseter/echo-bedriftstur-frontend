module Page exposing (..)

import Url
import Url.Parser as Parser

type Page
    = Hjem
    | LoggInn
    | Verified
    | Program
    | Bedrifter
    | Om
    | NotFound

urlToPage : Url.Url -> Page
urlToPage url =
    case Parser.parse parsePage url of
        Just page ->
            page
        Nothing ->
            NotFound

parsePage : Parser.Parser (Page -> b) b
parsePage =
    Parser.oneOf
        [ Parser.map Hjem Parser.top
        , Parser.map LoggInn (Parser.s "logg-inn")
        , Parser.map Verified (Parser.s "verified")
        , Parser.map Program (Parser.s "program")
        , Parser.map Bedrifter (Parser.s "bedrifter")
        , Parser.map Om (Parser.s "om")
        ]
