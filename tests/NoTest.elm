module NoTest exposing (..)

import Expect
import Test exposing (Test, test)


suite : Test
suite =
    test "No tests implemented" <|
        \_ -> Expect.equal True True
