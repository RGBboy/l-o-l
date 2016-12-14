module ClientChatTest exposing (tests)

import Test exposing (Test, describe, test)
import Expect
import String
import WebSocket

import ClientChat exposing (..)



server : String
server = "wss://test.com"

model : Model
model = init server

tests : Test
tests =
  describe "ClientChat"
    [ describe ".update ClientPost"
      [ test "returns send Post command" <|
        \() ->
          let
            (_, cmd) = update (ClientPost "Test") model
          in
            Expect.equal cmd (WebSocket.send server (encodeValue "Post" "Test"))
      , test "adds optimisticPost to model" <|
        \() ->
          let
            (newModel, _) = update (ClientPost "Test") model
          in
            Expect.equal (List.length newModel.optimisticPosts) ((List.length model.optimisticPosts) + 1)
      ]
    , describe ".update ClientJoin"
      [ test "returns send Post command" <|
        \() ->
          let
            (_, cmd) = update (ClientJoin "Test") model
          in
            Expect.equal cmd (WebSocket.send server (encodeValue "Join" "Test"))
      , test "returns same model" <|
        \() ->
          let
            (newModel, _) = update (ClientJoin "Test") model
          in
            Expect.equal model newModel
      ]
    ]
