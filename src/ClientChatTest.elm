module ClientChatTest exposing (tests)

import Test exposing (Test, describe, test)
import Expect
import Expect.Extra as Expect
import Dict exposing (Dict)
import Set exposing (Set)
import Json.Decode as Decode

import ClientChat exposing (..)

initUsers : Set Public
initUsers = Set.fromList [ "A", "B" ]

initUserNames : Dict Public String
initUserNames = Dict.fromList [ ("A", "Alpha"), ("B", "Beta")]

initPosts : List (Public, String)
initPosts = [ ("A", "message 1"), ("B", "message 2"), ("B", "message 3")]

initModel : Model
initModel = init "A" initUsers initUserNames initPosts

initJSON : String
initJSON = """
{
  "id": "A",
  "users": [
    "A",
    "B"
  ],
  "userNames": {
    "A": "Alpha",
    "B": "Beta"
  },
  "posts": [
    { "id": "A", "post": "message 1" },
    { "id": "B", "post": "message 2" },
    { "id": "B", "post": "message 3" }
  ]
}
"""

tests : Test
tests =
  describe "ClientChat"
    [ describe ".init"
      [ test "returns Model" <|
        Expect.all
          [ always (Expect.equal initModel.id "A")
          , always (Expect.equal initModel.users initUsers)
          , always (Expect.equal initModel.userNames initUserNames)
          , always (Expect.equal initModel.posts initPosts)
          , always (Expect.equal initModel.optimisticPosts [])
          ]
      ]
    , describe ".post"
      [ test "adds optimisticPost to model" <|
        \() ->
          let
            (newModel, _) = post "Test" initModel
          in
            Expect.equal (List.length newModel.optimisticPosts) ((List.length initModel.optimisticPosts) + 1)
      , test "returns send Post command" <|
        \() ->
          let
            (_, message) = post "Test" initModel
          in
            Expect.equal message (ClientPost "Test")
      ]
    , describe ".updateName"
      [ test "returns same model" <|
        \() ->
          let
            (newModel, _) = updateName "Test" initModel
          in
            Expect.equal initModel newModel
      , test "returns send UpdateName command" <|
        \() ->
          let
            (_, message) = updateName "Test" initModel
          in
            Expect.equal message (ClientUpdateName "Test")
      ]
    , describe ".update ServerConnection"
      [ test "updates users" <|
        \() ->
          let
            newModel = update (ServerConnection "ABC") initModel
          in
            Expect.true "Contains ABC" (Set.member "ABC" newModel.users)
      ]
    , describe ".update ServerPost"
      [ test "updates posts" <|
        \() ->
          let
            newModel = update (ServerPost "A" "new message") initModel
          in
            Expect.contain ("A", "new message") newModel.posts
      ]
    , describe ".update ServerUpdateName"
      [ test "updates userNames" <|
        \() ->
          let
            newModel = update (ServerUpdateName "A" "Updated Name") initModel
          in
            Expect.equal (Dict.get "A" newModel.userNames) (Just "Updated Name")
      ]
    , describe ".decodeInit"
      [ test "decodes Model" <|
        \() ->
          let
            result = Decode.decodeString decodeInit initJSON
            expected = initModel
          in
            Expect.equal result (Result.Ok expected)
      ]
    , describe ".decodeMessage"
      []
    ]
