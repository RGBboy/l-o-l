module ClientChatTest exposing (tests)

import Test exposing (Test, describe, test)
import Expect

import ClientChat exposing (..)

beforeInitModel : Model
beforeInitModel = init "ABC"

clientModel : ClientModel
clientModel = initClientModel "123"

afterInitModel : Model
afterInitModel = Tuple.first (update (ServerInit clientModel) beforeInitModel)

tests : Test
tests =
  describe "ClientChat"
    [ describe ".update ClientPost"
      [ describe "before init event"
        [ test "adds optimisticPost to model" <|
          \() ->
            let
              (newModel, _) = update (ClientPost "Test") beforeInitModel
            in
              Expect.equal newModel.optimisticPosts beforeInitModel.optimisticPosts
        , test "returns no message" <|
          \() ->
            let
              (_, message) = update (ClientPost "Test") beforeInitModel
            in
              Expect.equal message Nothing
        ]
      , describe "after init event"
        [ test "adds optimisticPost to model" <|
          \() ->
            let
              (newModel, _) = update (ClientPost "Test") afterInitModel
            in
              Expect.equal (List.length newModel.optimisticPosts) ((List.length afterInitModel.optimisticPosts) + 1)
        , test "returns send Post command" <|
          \() ->
            let
              (_, message) = update (ClientPost "Test") afterInitModel
            in
              Expect.equal message (Just (ClientPost "Test"))
        ]
      ]
    , describe ".update ClientUpdateName"
      [ test "returns same model" <|
        \() ->
          let
            (newModel, _) = update (ClientUpdateName "Test") beforeInitModel
          in
            Expect.equal beforeInitModel newModel
      , test "returns send UpdateName command" <|
        \() ->
          let
            (_, message) = update (ClientUpdateName "Test") beforeInitModel
          in
            Expect.equal message (Just (ClientUpdateName "Test"))
      ]
    , describe ".update ServerInit"
      [ test "updates chat" <|
        \() ->
          let
            (newModel, _) = update (ServerInit clientModel) beforeInitModel
          in
            Expect.equal newModel.chat (Just clientModel)
      , test "returns no message" <|
        \() ->
          let
            (_, message) = update (ServerInit clientModel) beforeInitModel
          in
            Expect.equal message Nothing
      ]
    ]
