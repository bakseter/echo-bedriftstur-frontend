module DegreeTest exposing (suite)

import Degree exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Decode as Decode
import Json.Encode as Encode
import Test exposing (..)


suite : Test
suite =
    describe "Tests for the Degree module"
        [ describe "The \"toString\" method works for the input"
            [ test "DTEK" <|
                \_ -> Expect.equal (Degree.toString False DTEK) "Datateknologi"
            , test "DVIT" <|
                \_ -> Expect.equal (Degree.toString False DVIT) "Datavitenskap"
            , test "DSIK" <|
                \_ -> Expect.equal (Degree.toString False DSIK) "Datasikkerhet"
            , test "BINF" <|
                \_ -> Expect.equal (Degree.toString False BINF) "Bioinformatikk"
            , test "IMØ" <|
                \_ -> Expect.equal (Degree.toString False IMØ) "Informatikk-matematikk-økonomi"
            , test "IKT" <|
                \_ -> Expect.equal (Degree.toString False IKT) "Informasjons- og kommunikasjonsteknologi"
            , test "KOGNI" <|
                \_ -> Expect.equal (Degree.toString False KOGNI) "Kognitiv vitenskap med spesialisering i informatikk"
            , test "INF" <|
                \_ -> Expect.equal (Degree.toString False INF) "Master i informatikk"
            , test "PROG" <|
                \_ -> Expect.equal (Degree.toString False PROG) "Felles master i programutvikling"
            , test "POST" <|
                \_ -> Expect.equal (Degree.toString False POST) "Postbachelor"
            , test "MISC" <|
                \_ -> Expect.equal (Degree.toString False MISC) "Annet studieløp"
            ]
        , describe "The \"fromString\" method works for the shorthand input"
            [ test "\"DTEK\"" <|
                \_ -> Expect.equal (Degree.fromString "Datateknologi") (Just DTEK)
            , test "\"DVIT\"" <|
                \_ -> Expect.equal (Degree.fromString "Datavitenskap") (Just DVIT)
            , test "\"DSIK\"" <|
                \_ -> Expect.equal (Degree.fromString "Datasikkerhet") (Just DSIK)
            , test "\"BINF\"" <|
                \_ -> Expect.equal (Degree.fromString "Bioinformatikk") (Just BINF)
            , test "\"IMØ\"" <|
                \_ -> Expect.equal (Degree.fromString "Informatikk-matematikk-økonomi") (Just IMØ)
            , test "\"IKT\"" <|
                \_ -> Expect.equal (Degree.fromString "Informasjons- og kommunikasjonsteknologi") (Just IKT)
            , test "\"KOGNI\"" <|
                \_ -> Expect.equal (Degree.fromString "Kognitiv vitenskap med spesialisering i informatikk") (Just KOGNI)
            , test "\"INF\"" <|
                \_ -> Expect.equal (Degree.fromString "Master i informatikk") (Just INF)
            , test "\"PROG\"" <|
                \_ -> Expect.equal (Degree.fromString "Felles master i programutvikling") (Just PROG)
            , test "\"POST\"" <|
                \_ -> Expect.equal (Degree.fromString "Postbachelor") (Just POST)
            , test "\"MISC\"" <|
                \_ -> Expect.equal (Degree.fromString "Annet studieløp") (Just MISC)
            , test "\"LINF\"" <|
                \_ -> Expect.equal (Degree.fromString "Not a degree") Nothing
            ]
        , describe "The \"fromString\" method works for the non-shorthand input"
            [ test "\"Datateknologi\"" <|
                \_ -> Expect.equal (Degree.fromString "Datateknologi") (Just DTEK)
            , test "\"Datatvitenskap\"" <|
                \_ -> Expect.equal (Degree.fromString "Datavitenskap") (Just DVIT)
            , test "\"Datasikkerhet\"" <|
                \_ -> Expect.equal (Degree.fromString "Datasikkerhet") (Just DSIK)
            , test "\"Bioinformatikk\"" <|
                \_ -> Expect.equal (Degree.fromString "Bioinformatikk") (Just BINF)
            , test "\"Informatikk-matematikk-økonomi\"" <|
                \_ -> Expect.equal (Degree.fromString "Informatikk-matematikk-økonomi") (Just IMØ)
            , test "\"Informasjons- og kommunikasjonsteknologi\"" <|
                \_ -> Expect.equal (Degree.fromString "Informasjons- og kommunikasjonsteknologi") (Just IKT)
            , test "\"Kognitiv vitenskap med spesialisering i informatikk\"" <|
                \_ -> Expect.equal (Degree.fromString "Kognitiv vitenskap med spesialisering i informatikk") (Just KOGNI)
            , test "\"Master i informatikk\"" <|
                \_ -> Expect.equal (Degree.fromString "Master i informatikk") (Just INF)
            , test "\"Felles master i programutvkling\"" <|
                \_ -> Expect.equal (Degree.fromString "Felles master i programutvikling") (Just PROG)
            , test "\"Postbachelor\"" <|
                \_ -> Expect.equal (Degree.fromString "Postbachelor") (Just POST)
            , test "\"Annet studieløp\"" <|
                \_ -> Expect.equal (Degree.fromString "Annet studieløp") (Just MISC)
            , test "\"Not a degree\"" <|
                \_ -> Expect.equal (Degree.fromString "Not a degree") Nothing
            ]
        ]
