module ChatTest exposing (tests)

import Test exposing (Test, describe, test)
import Expect
import String


tests : Test
tests =
  describe "Chat"
    [ describe "Unit test examples"
      [ test "Addition" <|
        \() ->
          Expect.equal (3 + 7) 10
      , test "String.left" <|
        \() ->
          Expect.equal "a" (String.left 1 "abcdefg")
      , test "This test should fail - you should remove it" <|
        \() ->
          Expect.fail "Failed as expected!"
      ]
    ]
