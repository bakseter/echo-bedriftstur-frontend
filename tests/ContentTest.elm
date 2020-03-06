module ContentTest exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

import Content exposing (..)
import Degree exposing (..)

suite : Test
suite =
    let content = { firstName = "Test"
                  , lastName = "Testesen"
                  , degree = Valid BINF
                  }
    in
        describe "Tests for the Content module"
            [ describe "The Content record is updated with the"
                [ test "updateFirstName function" <|
                    \_ -> Expect.equal ({ content | firstName = "Foo" }) (Content.updateFirstName "Foo" content)
                , test "updateLastName function" <|
                    \_ -> Expect.equal ({ content | lastName = "Bar" }) (Content.updateLastName "Bar" content)
                , test "updateDegree function" <|
                    \_ -> Expect.equal ({ content | degree = Valid INF }) (Content.updateDegree (Valid INF) content)
                , test "updateAll function" <|
                    \_ -> Expect.equal ({ content | firstName = "Example", lastName = "McTest", degree = Valid PROG })
                                        (updateAll "Example" "McTest" (Valid PROG) content)
                ]
            ]
