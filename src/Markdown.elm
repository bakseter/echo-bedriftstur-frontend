module Markdown exposing (..)

import Parser exposing ((|.), (|=), Parser)
import Set


type Markdown
    = H1 String
    | H2 String
    | H3 String
    | H4 String
    | Code String
    | Bold String
    | Cursive String
    | Dummy


text : Parser String
text =
    Parser.variable
        { start = not << isReserved
        , inner = not << isReserved
        , reserved = Set.fromList (List.map String.fromChar reserved)
        }


reserved : List Char
reserved =
    [ '#', '`', '-', '_' ]


isReserved : Char -> Bool
isReserved c =
    List.member c reserved


markdown : Parser Markdown
markdown =
    Parser.oneOf
        [ parseHeading H4 "####"
        , parseHeading H3 "###"
        , parseHeading H2 "##"
        , parseHeading H1 "#"
        ]


fromString : String -> Maybe (List Markdown)
fromString str =
    let
        lines =
            String.lines str

        result =
            List.map (Parser.run markdown) lines
    in
    sequence result


sequence : List (Result a b) -> Maybe (List b)
sequence list =
    let
        prod l =
            case l of
                x :: xs ->
                    case x of
                        Ok val ->
                            val :: prod xs

                        Err _ ->
                            []

                [] ->
                    []

        result =
            prod list
    in
    if List.length result == List.length list then
        Just result

    else
        Nothing


parseHeading : (String -> Markdown) -> String -> Parser Markdown
parseHeading h symbol =
    let
        start =
            Parser.succeed h
                |. Parser.symbol symbol
                |. Parser.spaces
                |= text
    in
    Parser.oneOf
        [ start
        , start
            |. Parser.spaces
            |. Parser.symbol symbol
        ]
