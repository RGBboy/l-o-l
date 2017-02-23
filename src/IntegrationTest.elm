module IntegrationTest exposing (tests)

import Test exposing (Test, describe, test)
import Expect
import String

import Sandbox
import ServerChat



newServer = ServerChat.init

tests : Test
tests =
  describe "chat"
    [ test "client can connect to the server with a private key" <|
      \() ->
        let
          connectionA = "A"
          secretA = "sA"
          connectionB = "B"
          secretB = "sB"
          connectionC = "C"
          secretC = "sC"
          sandbox = Sandbox.init newServer [] |>
            Sandbox.bulkUpdate
              [ Sandbox.clientConnection connectionA
              , Sandbox.clientLogin connectionA secretA
              , Sandbox.clientConnection connectionB
              , Sandbox.clientLogin connectionB secretB
              , Sandbox.clientConnection connectionC
              , Sandbox.clientLogin connectionC secretC
              ]
        in
          Expect.all
            [ Expect.equal sandbox.server.connections (Set.fromList ["A", "B", "C"])
            , Expect.equal (Dict.get "A" sandbox.connections) (Dict.get "B" sandbox.connections)
            , Expect.equal (Dict.get "B" sandbox.connections) (Dict.get "C" sandbox.connections)
            ]
    , test "client can reconnect to the server with their private key and keep their data" <|
      \() ->
        Expect.equal "A" "B"
        -- Server has new assocication of private id and connection
        -- New connection gets sent an initial model
    , test "client can disconnect with a single device" <|
      \() ->
        Expect.equal "A" "B"
        -- Server removes association of connection with private and public id
        -- No messages are sent to clients
    , test "client can disconnect with a all devices" <|
      \() ->
        Expect.equal "A" "B"
        -- Server removes association of connection with private and public id
        -- Other clients are sent disconnection
    , test "client can change their name" <|
      \() ->
        Expect.equal "A" "B"
        -- Client has temporary name in its model
        -- Server is sent updated name
        -- Server model has updated name
        -- Other clients are sent updated name
        -- Other clients have updated name associated with client
    , test "client can post a message" <|
      \() ->
        Expect.equal "A" "B"
        -- Client has temporary message in its model
        -- Server is sent message
        -- Server model has message
        -- Other clients are sent message
        -- Other clients have message
    ]
