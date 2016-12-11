module WebSocketServerTest exposing (tests)

import Test exposing (Test, describe, test)
import Expect
import String
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


import WebSocketServer exposing (..)

connectionJSON : String
connectionJSON = """
{
  "type": "Connection",
  "id": "abc"
}
"""

disconnectionJSON : String
disconnectionJSON = """
{
  "type": "Disconnection",
  "id": "abc"
}
"""

messageJSON : String
messageJSON = """
{
  "type": "Message",
  "id": "abc",
  "message": "Test"
}
"""


type Msg
  = Connection Socket
  | Disconnection Socket
  | Message Socket String

config : Config Msg
config =
  { onConnection = Connection
  , onDisconnection = Disconnection
  , onMessage = decodeMessage
  }

decodeMessage : Socket -> Decoder Msg
decodeMessage socket =
  Decode.string
    |> Decode.map (Message socket)

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
            (Expect.equal (Connection "abc"))
      , test "decodes Disconnection events" <|
        \() ->
          expectDecode (eventDecoder config) disconnectionJSON
            (Expect.equal (Disconnection "abc"))
      , test "decodes Message events" <|
        \() ->
          expectDecode (eventDecoder config) messageJSON
            (Expect.equal (Message "abc" "Test"))
      ]
    , describe ".close"
      [ test "close" <|
        \() ->
          let
            actual = Encode.encode 2 (close identity "abc")
            expected = """{
  "type": "Close",
  "id": "abc"
}"""
          in
            Expect.equal expected actual
      ]
    ]
