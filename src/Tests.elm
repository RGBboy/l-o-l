port module Tests exposing (main)

import Test exposing (Test)
import Test.Runner.Node exposing (run, TestProgram)
import Json.Encode exposing (Value)

import ChatTest


all : Test
all =
  ChatTest.tests


main : TestProgram
main =
  run emit all


port emit : ( String, Value ) -> Cmd msg
