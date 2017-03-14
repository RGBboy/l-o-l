module ServerChat exposing
  ( Model
  , init
  , ClientModel
  , clientModel
  , update
  , InputMsg(Connection, Disconnection, Post, UpdateName)
  , OutputMsg(OutputInit, OutputConnection, OutputPost, OutputUpdateName)
  , Output
  )

import Set exposing (Set)
import Dict exposing (Dict)

-- MODEL

type alias Secret = String

type alias Public = Secret -- Change to be something else

type alias Connection = String

type alias Model =
  { connections: Set Connection
  , users: Dict Connection Secret
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

type alias ClientModel =
  { id: Public
  , users: Set Public
  , userNames: Dict Public String
  , posts: List (Public, String)
  }

clientModel : Model -> Public -> ClientModel
clientModel model id =
  { id = id
  , users = Set.fromList (Dict.values model.users)
  , userNames = model.userNames
  , posts = model.posts
  }

maxPosts : Int
maxPosts = 8



-- UPDATE

type InputMsg
  = Connection Connection Secret
  | Disconnection Connection
  | Post Connection String
  | UpdateName Connection String

type OutputMsg
  = OutputInit ClientModel
  | OutputConnection Public
  | OutputPost Public String
  | OutputUpdateName Public String

type alias Output = (Connection, OutputMsg)

output : OutputMsg -> Connection -> (Connection, OutputMsg)
output message connection = (connection, message)

outputToOthers : Set Connection -> Connection -> OutputMsg -> List (Connection, OutputMsg)
outputToOthers connections connection message =
  outputToAll (Set.remove connection connections) message

outputToAll : Set Connection -> OutputMsg -> List (Connection, OutputMsg)
outputToAll connections message =
  List.map (output message) (Set.toList connections)

update : InputMsg -> Model -> (Model, List Output)
update message model =
  case message of
    Connection connection secret ->
      let
        newModel =
          { model
          | connections = Set.insert connection model.connections
          , users = Dict.insert connection secret model.users
          }
        initClient = OutputInit (clientModel newModel secret) -- change this to public id instead of secret
      in
        ( newModel
        , (output initClient connection) :: (outputToOthers model.connections connection (OutputConnection secret))
        )
    Disconnection connection ->
      let
        newModel =
          { model
          | connections = Set.remove connection model.connections
          }
      in
        ( newModel
        , []
        )
    Post connection post ->
      let
        -- need to deref connection to public id
        user = Dict.get connection model.users
        newPost = Maybe.map (\public -> (public, post)) user
        newPosts = Maybe.map List.singleton newPost
          |> Maybe.withDefault []
        newModel =
          { model
          | posts = List.take maxPosts <| (List.append newPosts model.posts)
          }
        output = (uncurry OutputPost) >> outputToAll newModel.connections
        newMessages = Maybe.map output newPost
          |> Maybe.withDefault []
      in
        ( newModel
        , newMessages
        )
    UpdateName connection name ->
      let
        -- need to deref connection to public id
        user = Dict.get connection model.users
        newUserName = Maybe.map (\public -> (public, name)) user
        newUserNames = Maybe.map (uncurry Dict.singleton) newUserName
          |> Maybe.withDefault Dict.empty
        newModel =
          { model
          | userNames = Dict.union newUserNames model.userNames
          }
        output = (uncurry OutputUpdateName) >> outputToAll newModel.connections
        newMessages = Maybe.map output newUserName
          |> Maybe.withDefault []
      in
        ( newModel
        , newMessages
        )
