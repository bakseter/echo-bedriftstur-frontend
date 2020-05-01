module DegreeTest exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Json.Encode as Encode
import Json.Decode as Decode

import Degree exposing (..)

suite : Test
suite =
    describe "Tests for the Degree module"
        [ describe "The \"toString\" method works for the input"
            [ test "DTEK" <|
                \_ -> Expect.equal (Degree.toString False (Valid DTEK)) "Datateknologi"
            , test "DVIT" <|
                \_ -> Expect.equal (Degree.toString False (Valid DVIT)) "Datatvitenskap" 
            , test "DSIK" <|
                \_ -> Expect.equal (Degree.toString False (Valid DSIK)) "Datasikkerhet"
            , test "BINF" <|
                \_ -> Expect.equal (Degree.toString False (Valid BINF)) "Bioinformatikk"
            , test "IMØ" <|
                \_ -> Expect.equal (Degree.toString False (Valid IMØ)) "Informatikk-matematikk-økonomi" 
            , test "IKT" <|
                \_ -> Expect.equal (Degree.toString False (Valid IKT)) "Informasjons- og kommunikasjonsteknologi"
            , test "KOGNI" <|
                \_ -> Expect.equal (Degree.toString False (Valid KOGNI)) "Kognitiv vitenskap med spesialisering i informatikk"
            , test "INF" <|
                \_ -> Expect.equal (Degree.toString False (Valid INF)) "Master i informatikk"
            , test "PROG" <|
                \_ -> Expect.equal (Degree.toString False (Valid PROG)) "Felles master i programutvkling"
            , test "POST" <|
                \_ -> Expect.equal (Degree.toString False (Valid POST)) "Postbachelor"
            , test "MISC" <|
                \_ -> Expect.equal (Degree.toString False (Valid MISC)) "Annet studieløp"
            , test "None" <|
                \_ -> Expect.equal (Degree.toString False None) ""
            ]
        , describe "The \"fromString\" method works for the input"
            [ test "\"Datateknologi\"" <|
                \_ -> Expect.equal (Degree.fromString False "Datateknologi") (Valid DTEK)
            , test "\"Datatvitenskap\"" <|
                \_ -> Expect.equal (Degree.fromString False "Datatvitenskap") (Valid DVIT)
            , test "\"Datasikkerhet\"" <|
                \_ -> Expect.equal (Degree.fromString False "Datasikkerhet") (Valid DSIK)
            , test "\"Bioinformatikk\"" <|
                \_ -> Expect.equal (Degree.fromString False "Bioinformatikk") (Valid BINF)
            , test "\"Informatikk-matematikk-økonomi\"" <|
                \_ -> Expect.equal (Degree.fromString False "Informatikk-matematikk-økonomi") (Valid IMØ)
            , test "\"Informasjons- og kommunikasjonsteknologi\"" <|
                \_ -> Expect.equal (Degree.fromString False "Informasjons- og kommunikasjonsteknologi") (Valid IKT)
            , test "\"Kognitiv vitenskap med spesialisering i informatikk\"" <|
                \_ -> Expect.equal (Degree.fromString False "Kognitiv vitenskap med spesialisering i informatikk") (Valid KOGNI)
            , test "\"Master i informatikk\"" <|
                \_ -> Expect.equal (Degree.fromString False "Master i informatikk") (Valid INF)
            , test "\"Felles master i programutvkling\"" <|
                \_ -> Expect.equal (Degree.fromString False "Felles master i programutvkling") (Valid PROG)
            , test "\"Postbachelor\"" <|
                \_ -> Expect.equal (Degree.fromString False "Postbachelor") (Valid POST)
            , test "\"Annet studieløp\"" <|
                \_ -> Expect.equal (Degree.fromString False "Annet studieløp") (Valid MISC)
            , test "\"Not a degree\"" <|
                \_ -> Expect.equal (Degree.fromString False "Not a degree") None
            ]
        ]
