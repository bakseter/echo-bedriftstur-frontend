module Page exposing (Page(..), fromUrl)

import Page.Bedrifter as Bedrifter
import Page.Om as Om
import Page.Profil as Profil
import Page.Program as Program
import Url
import Url.Parser as Parser


type Page
    = Hjem
    | Profil
    | Program
    | Bedrifter
    | Om
    | NotFound


fromUrl : Url.Url -> Page
fromUrl url =
    Maybe.withDefault NotFound <| Parser.parse pageParser url


pageParser : Parser.Parser (Page -> b) b
pageParser =
    Parser.oneOf
        [ Parser.map Hjem Parser.top
        , Parser.map Profil (Parser.s Profil.route)
        , Parser.map Program (Parser.s Program.route)
        , Parser.map Bedrifter (Parser.s Bedrifter.route)
        , Parser.map Om (Parser.s Om.route)
        ]
