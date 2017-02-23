module Sandbox exposing
  ( Model
  , Msg
  , init
  , update)



-- MODEL

type alias Model =
  { server: ServerChat.Model
  , connections: List ClientChat.Model
  }

init : Server.Model -> List Client.Model -> Model
init = Model



-- UPDATE

type Msg
  = ClientConnection Server.ConnectionId
  | ClientDisconnection Server.ConnectionId
  | ClientLogin Server.ConnectionId Client.PrivateId

clientConnection : Server.ConnectionId -> Msg
clientConnection = ClientConnection

clientDisconnection : Server.ConnectionId -> Msg
clientDisconnection = ClientDisconnection

clientLogin : Server.ConnectionId -> Client.PrivateId -> Msg
clientLogin = ClientLogin

update : Msg -> Model -> Model
update message model =
  -- do this
  model

bulkUpdate : List Msg -> Model -> Model
bulkUpdate messages model =
  List.foldl update model messages
