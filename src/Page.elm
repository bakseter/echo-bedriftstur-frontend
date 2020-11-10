module Page exposing (Page(..), fromUrl)

import Page.Bedrifter as Bedrifter
import Page.Om as Om
import Page.Profil as Profil
import Page.Program as Program
import Url
import Url.Parser as Parser



-- Type representing all the pages on the website.


type Page
    = Hjem
    | Profil
    | Program
    | Bedrifter
    | Om
    | NotFound



-- Runs the pageParser function on a url, converting it to a Page type.


fromUrl : Url.Url -> Page
fromUrl url =
    case Parser.parse pageParser url of
        Just page ->
            page

        Nothing ->
            NotFound



-- Returns the correct Page type (according to the route of the Page type) for an url.


pageParser : Parser.Parser (Page -> b) b
pageParser =
    Parser.oneOf
        [ Parser.map Hjem Parser.top
        , Parser.map Profil (Parser.s Profil.route)
        , Parser.map Program (Parser.s Program.route)
        , Parser.map Bedrifter (Parser.s Bedrifter.route)
        , Parser.map Om (Parser.s Om.route)
        ]
