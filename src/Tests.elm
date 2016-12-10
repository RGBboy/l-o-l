port module Tests exposing (..)

import Test exposing (..)
import Test.Runner.Node exposing (run, TestProgram)
import Json.Encode exposing (Value)
import Expect
import String


all : Test
all =
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

main : TestProgram
main =
  run emit all


port emit : ( String, Value ) -> Cmd msg
