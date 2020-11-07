module ContentTest exposing (suite)

import Content exposing (..)
import Degree exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Terms exposing (..)
import Test exposing (..)


suite : Test
suite =
    let
        content =
            Content "Test" "Testesen" BINF (Terms False)
    in
    describe "Tests for the Content module"
        [ describe "The Content record is updated with the"
            [ test "updateFirstName function" <|
                \_ -> Expect.equal { content | firstName = "Foo" } (Content.updateFirstName "Foo" content)
            , test "updateLastName function" <|
                \_ -> Expect.equal { content | lastName = "Bar" } (Content.updateLastName "Bar" content)
            , test "updateDegree function" <|
                \_ -> Expect.equal { content | degree = INF } (Content.updateDegree INF content)
            , test "updateAll function" <|
                \_ ->
                    Expect.equal { content | firstName = "Example", lastName = "McTest", degree = PROG, terms = Terms True }
                        (updateAll "Example" "McTest" PROG (Terms True) content)
            ]
        ]
