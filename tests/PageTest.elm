module PageTest exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Url exposing (..)
import Url.Builder as Builder

import Page exposing (..)

suite : Test
suite =
    let host = "https://echobedriftstur.no"
    in
        describe "Tests for the Page module)"
            [ describe "The urlToPage function returns a"
                [ test "LoggInn Page when given the corresponding url" <|
                    \_ -> Expect.equal LoggInn <| urlToPage <| stringToUrl <| (Builder.crossOrigin host [ "logg-inn" ] [])
                , test "Verified Page when given the corresponding url" <|
                    \_ -> Expect.equal Verified <| urlToPage <| stringToUrl <| (Builder.crossOrigin host [ "verified" ] [])
                , test "Hjem Page when given the corresponding url" <|
                    \_ -> Expect.equal Hjem <| urlToPage <| stringToUrl <| host
                , test "Program Page when given the corresponding url" <|
                    \_ -> Expect.equal Program <| urlToPage <| stringToUrl <| (Builder.crossOrigin host [ "program" ] [])
                , test "Bedrifter Page when given the corresponding url" <|
                    \_ -> Expect.equal Bedrifter <| urlToPage <| stringToUrl <| (Builder.crossOrigin host [ "bedrifter" ] [])
                , test "Om Page when given the corresponding url" <|
                    \_ -> Expect.equal Om <| urlToPage <| stringToUrl <| (Builder.crossOrigin host [ "om" ] [])
                ]
            , describe "The urlToPage function always returns a NotFound Page when given invalid url paths, such as"
                [ test "/asd123" <|
                    \_ -> Expect.equal NotFound <| urlToPage <| stringToUrl <| (Builder.crossOrigin host [ "asd123" ] [])
                , test "/asd123/qwe456" <|
                    \_ -> Expect.equal NotFound <| urlToPage <| stringToUrl <| (Builder.crossOrigin host [ "asd123", "qwe456" ] [])
                , test "/asd123?q=test" <|
                    \_ -> Expect.equal NotFound <| urlToPage <| stringToUrl <| (Builder.crossOrigin host [ "asd123" ] [ Builder.string "q" "test" ])
                , test "/asd123/qwe456?q=test" <|
                    \_ -> Expect.equal NotFound <| urlToPage <| stringToUrl <| (Builder.crossOrigin host [ "asd123", "qwe456" ] [ Builder.string "q" "test" ])
                , test "/asd123/qwe456?q=test&w=test2" <|
                    \_ -> Expect.equal NotFound <| urlToPage <| stringToUrl <| (Builder.crossOrigin host [ "asd123", "qwe456" ]  [ Builder.string "q" "test", Builder.string "w" "test2" ])
                ]
            ]

stringToUrl string =
    case Url.fromString string of
        Just url ->
            url
        Nothing ->
            { protocol = Https, host = "", port_ = Nothing, path = "", query = Nothing, fragment = Nothing }
