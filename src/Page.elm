module Page exposing (Page(..), urlToPage)

import Url
import Url.Parser as Parser

import Page.LoggInn as LoggInn
import Page.Verified as Verified
import Page.Program as Program
import Page.Bedrifter as Bedrifter
import Page.Om as Om

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
    Parser.oneOf
        [ Parser.map Hjem Parser.top
        , Parser.map LoggInn (Parser.s LoggInn.route)
        , Parser.map Verified (Parser.s Verified.route)
        , Parser.map Program (Parser.s Program.route)
        , Parser.map Bedrifter (Parser.s Bedrifter.route)
        , Parser.map Om (Parser.s Om.route)
        ]