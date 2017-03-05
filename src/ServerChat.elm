module ServerChat exposing
  ( Model
  , init
  , update
  , InputMsg(Connection, Disconnection, Post, UpdateName)
  , OutputMsg(OutputInit, OutputConnection, OutputDisconnection, OutputPost, OutputUpdateName)
  , Output
  )

import Set exposing (Set)
import Dict exposing (Dict)

-- MODEL

type alias Secret = String

type alias ID = String

type alias Model =
  { connections: Set ID
  , users: Dict Secret ID
  , userNames: Dict Secret String
  , posts: List (Secret, String)
  }

init : Model
init =
  { connections = Set.empty
  , users = Dict.empty
  , userNames = Dict.empty
  , posts = []
  }

maxPosts : Int
maxPosts = 8



-- UPDATE

type InputMsg
  = Connection ID Secret
  | Disconnection ID
  | Post ID String
  | UpdateName ID String

type OutputMsg
  = OutputInit Model -- Change this so that we do not expose secrets
  | OutputConnection ID
  | OutputDisconnection ID
  | OutputPost ID String
  | OutputUpdateName ID String

type alias Output = (ID, OutputMsg)

output : OutputMsg -> ID -> (ID, OutputMsg)
output message id = (id, message)

outputToAll : Set ID -> OutputMsg -> List (ID, OutputMsg)
outputToAll users message =
  List.map (output message) (Set.toList users)

update : InputMsg -> Model -> (Model, List Output)
update message model =
  case message of
    Connection id secret ->
      let
        newModel =
          { model
          | connections = Set.insert id model.connections
          , users = Dict.insert secret id model.users
          }
      in
        ( newModel, [] )
    Disconnection id ->
      let
        newModel =
          { model
          | connections = Set.remove id model.connections
          }
      in
        ( newModel
        , outputToAll newModel.connections (OutputDisconnection id)
        )
    Post id post ->
      let
        -- need to deref connection to public id
        newModel =
          { model
          | posts = List.take maxPosts <| (id, post) :: model.posts
          }
      in
        ( newModel, [] )
    UpdateName id name ->
      let
        -- need to deref connection to public id
        newModel =
          { model
          | userNames = Dict.insert id name model.userNames
          }
      in
        ( newModel, [] )
