module ClientTest exposing (tests)

import Test exposing (Test, describe, test)
import Expect
import String
import WebSocket

import Client exposing (..)



server : String
server = "wss://test.com"

model : Model
model = initModel server

tests : Test
tests =
  describe "Client"
    [ describe ".subscriptions"
      [ describe "when model has no secret"
        [ test "does not subscribe" <|
          \() ->
            let
              subscription = subscriptions { model | secret = Nothing }
            in
              Expect.equal subscription (Sub.none)
        ]
      , describe "when model has secret"
        [ test "subscribes to a websocket using secret" <|
          \() ->
            let
              secret = "abc123"
              subscription = subscriptions { model | secret = Just secret }
            in
              Expect.equal subscription (WebSocket.listen (server ++ "/" ++ secret) ServerMessage)
        ]
      ]
    ]
