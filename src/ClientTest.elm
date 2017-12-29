module ClientTest exposing (tests)

import Test exposing (Test, describe, test)
import Dict exposing (Dict)
import Expect
import Set exposing (Set)
import String
import WebSocket

import Client exposing (..)
import ClientChat



server : String
server = "wss://test.com/"

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
        [ test "subscribes to a websocket using secret and decodeInit" <|
          \() ->
            let
              secret = "abc123"
              subscription = subscriptions { model | secret = Just secret }
            in
              Expect.equal subscription (WebSocket.listen (server ++ secret) decodeInit)
        ]
      , describe "when model has chat"
        [ test "subscribes to a websocket using secret and decodeMessage" <|
          \() ->
            let
              secret = "abc123"
              id = "A"
              users = Set.fromList ["A"]
              userNames = Dict.fromList []
              posts = []
              chat = ClientChat.init id users userNames posts
              subscription = subscriptions
                { model
                | chat = Just chat
                , secret = Just secret
                }
            in
              Expect.equal subscription (WebSocket.listen (server ++ secret) decodeMessage)
        ]
      ]
    ]
