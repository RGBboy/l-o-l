module ServerChatTest exposing (tests)

import Test exposing (Test, describe, test)
import Expect.Extra as Expect
import Expect
import String
import Set
import Dict

import ServerChat exposing (..)

initialUpdate : (Model, List Output)
initialUpdate = update (Connection "A" "secretA") init

serverWithConnection : Model
serverWithConnection = Tuple.first initialUpdate


tests : Test
tests =
  describe "ServerChat"
    [ describe ".update Connection"
      [ test "adds connection to model.connections" <|
        \() ->
          let
            (newModel, _) = initialUpdate
          in
            Expect.equal serverWithConnection.connections (Set.fromList ["A"])
      , test "adds user to model.users" <|
        \() ->
          let
            (newModel, _) = initialUpdate
          in
            Expect.equal serverWithConnection.users (Dict.fromList [("A", "secretA")])
      , test "returns init message for new connection" <|
        \() ->
          let
            (newModel, messages) = initialUpdate
            outputInit = OutputInit (clientModel newModel "secretA")
          in
            Expect.contain ("A", outputInit) messages
      , test "returns connection message for other connections" <|
        \() ->
          let
            (_, messages) = initialUpdate
                |> Tuple.first
                |> update (Connection "B" "secretB")
          in
            Expect.contain ("A", OutputConnection "secretB") messages

      ]
    , describe ".update Disconnection"
      [ test "removes connection to model.connections" <|
        \() ->
          let
            (newModel, _) = initialUpdate
              |> Tuple.first
              |> update (Disconnection "A")
          in
            Expect.equal newModel.connections Set.empty
      , test "keeps users" <|
        \() ->
          let
            (newModel, _) = initialUpdate
              |> Tuple.first
              |> update (Disconnection "A")
          in
            Expect.equal newModel.users serverWithConnection.users
      , test "returns no messages for connections" <|
        \() ->
          let
            (_, messages) = initialUpdate
              |> Tuple.first
              |> update (Connection "B" "secretB")
              |> Tuple.first
              |> update (Disconnection "A")
          in
            Expect.equal messages []
      ]
    , describe ".update Post"
      [ test "adds post to model.posts" <|
        \() ->
          let
            (newModel, _) = initialUpdate
              |> Tuple.first
              |> update (Post "A" "Test")
          in
            Expect.equal newModel.posts [("secretA", "Test")]
      , test "returns post message to all connections" <|
        \() ->
          let
            (_, messages) = initialUpdate
              |> Tuple.first
              |> update (Post "A" "Test")
          in
            Expect.contain ("A", OutputPost "secretA" "Test") messages
      ]
    , describe ".update UpdateName"
      [ test "updates name in model.userNames" <|
        \() ->
          let
            (newModel, _) = initialUpdate
              |> Tuple.first
              |> update (UpdateName "A" "Test")
          in
            Expect.equal newModel.userNames (Dict.fromList [("secretA", "Test")])
      , test "returns UpdateName message to all connections" <|
        \() ->
          let
            (_, messages) = initialUpdate
              |> Tuple.first
              |> update (Connection "B" "secretB")
              |> Tuple.first
              |> update (UpdateName "A" "Test")
          in
            Expect.all
              [ \messages -> Expect.contain ("A", OutputUpdateName "secretA" "Test") messages
              , \messages -> Expect.contain ("B", OutputUpdateName "secretA" "Test") messages
              ]
              messages
      ]
    ]
