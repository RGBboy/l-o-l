module WebSocketServerTest exposing (tests)

import Test exposing (Test, describe, test)
import Expect
import String
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Navigation exposing (Location)

import WebSocketServer exposing (..)

location : Location
location = (Location "ws:" "" "" "/123" "8080" "localhost" "localhost:8080" "ws://localhost:8080" "ws://localhost:8080/123" "" "")

connectionJSON : String
connectionJSON = """
{
  "type": "Connection",
  "id": "abc",
  "location": {
    "protocol": "ws:",
    "hash": "",
    "search": "",
    "pathname": "/123",
    "port_": "8080",
    "hostname": "localhost",
    "host": "localhost:8080",
    "origin": "ws://localhost:8080",
    "href": "ws://localhost:8080/123",
    "username" : "",
    "password" : ""
  }
}
"""



disconnectionJSON : String
disconnectionJSON = """
{
  "type": "Disconnection",
  "id": "abc",
  "location": {
    "protocol": "ws:",
    "hash": "",
    "search": "",
    "pathname": "/123",
    "port_": "8080",
    "hostname": "localhost",
    "host": "localhost:8080",
    "origin": "ws://localhost:8080",
    "href": "ws://localhost:8080/123",
    "username" : "",
    "password" : ""
  }
}
"""

messageJSON : String
messageJSON = """
{
  "type": "Message",
  "id": "abc",
  "location": {
    "protocol": "ws:",
    "hash": "",
    "search": "",
    "pathname": "/123",
    "port_": "8080",
    "hostname": "localhost",
    "host": "localhost:8080",
    "origin": "ws://localhost:8080",
    "href": "ws://localhost:8080/123",
    "username" : "",
    "password" : ""
  },
  "message": "Test"
}
"""


type Msg
  = Connection Socket Location
  | Disconnection Socket Location
  | Message Socket Location String

config : Config Msg
config =
  { onConnection = Connection
  , onDisconnection = Disconnection
  , onMessage = decodeMessage
  }

decodeMessage : Socket -> Location -> Decoder Msg
decodeMessage socket location =
  Decode.string
    |> Decode.map (Message socket location)

expectDecode : Decoder a -> String -> (a -> Expect.Expectation) -> Expect.Expectation
expectDecode decoder input expectation =
  Result.withDefault (Expect.fail "Unable to decode")
    (Result.map expectation (Decode.decodeString decoder input))

tests : Test
tests =
  describe "WebSocketServer"
    [ describe ".eventDecoder"
      [ test "decodes Connection events" <|
        \() ->
          expectDecode (eventDecoder config) connectionJSON
            (flip Expect.equal (Connection "abc" location))
      , test "decodes Disconnection events" <|
        \() ->
          expectDecode (eventDecoder config) disconnectionJSON
            (flip Expect.equal (Disconnection "abc" location))
      , test "decodes Message events" <|
        \() ->
          expectDecode (eventDecoder config) messageJSON
            (flip Expect.equal (Message "abc" location "Test"))
      ]
    , describe ".close"
      [ test "close" <|
        \() ->
          let
            actual = Encode.encode 2 (close identity "a")
            expected = """{
  "type": "Close",
  "id": "a"
}"""
          in
            Expect.equal actual expected
      ]
    , describe ".sendToOne"
      [ test "sendToOne" <|
        \() ->
          let
            actual = Encode.encode 2 (sendToOne identity (Encode.string "Test") "a")
            expected = """{
  "type": "Message",
  "id": "a",
  "data": "Test"
}"""
          in
            Expect.equal actual expected
      ]
    , describe ".sendToMany"
      [ test "sendToMany" <|
        \() ->
          let
            actual = List.map (Encode.encode 2) (sendToMany identity (Encode.string "Test") ["a", "b"])
            expected =
              ["""{
  "type": "Message",
  "id": "a",
  "data": "Test"
}"""
              , """{
  "type": "Message",
  "id": "b",
  "data": "Test"
}"""
              ]
          in
            Expect.equalLists actual expected
      ]
    , describe ".sendToOthers"
      [ test "sendToOthers" <|
        \() ->
          let
            actual = List.map (Encode.encode 2) (sendToOthers identity (Encode.string "Test") "a" ["a", "b", "c"])
            expected =
              ["""{
  "type": "Message",
  "id": "b",
  "data": "Test"
}"""
              , """{
  "type": "Message",
  "id": "c",
  "data": "Test"
}"""
              ]
          in
            Expect.equalLists actual expected
      ]
    ]
