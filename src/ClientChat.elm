module ClientChat exposing
  ( Model
  , init
  , ClientModel
  , initClientModel
  , update
  , Msg(ClientPost, ClientUpdateName, ServerInit, ServerConnection, ServerPost, ServerUpdateName)
  , posts
  , userNames
  )

import List.Extra as ListExtra
import Set exposing (Set)
import Dict exposing (Dict)
import Json.Decode as Decode
import Json.Encode as Encode


-- MODEL

type alias Private = String

type alias Public = String

type alias ClientModel =
  { id: Public
  , users: Set Public
  , userNames: Dict Public String
  , posts: List (Public, String)
  }

initClientModel : Public -> ClientModel
initClientModel id =
  { id = id
  , users = Set.empty
  , userNames = Dict.empty
  , posts = []
  }

type alias Model =
  { secret: Private
  , chat: Maybe ClientModel
  , optimisticPosts: List (Public, String)
  }

init : Private -> Model
init secret =
  { secret = secret
  , chat = Nothing
  , optimisticPosts = []
  }

userNames : Model -> Dict Public String
userNames model =
  Maybe.map .userNames model.chat
    |> Maybe.withDefault Dict.empty

posts : Model -> List (Public, String)
posts model =
  let
    posts = Maybe.map .posts model.chat
      |> Maybe.withDefault []
  in
    List.append model.optimisticPosts posts

-- UPDATE

type Msg
  = ClientPost String
  | ClientUpdateName String
  | ServerInit ClientModel
  | ServerConnection Public
  | ServerPost Public String
  | ServerUpdateName Public String

updatePost : String -> Model -> ClientModel -> (Model, Maybe Msg)
updatePost post model clientModel =
  ( { model
    | optimisticPosts = (clientModel.id, post) :: model.optimisticPosts
    }
  , Just (ClientPost post)
  )

update : Msg -> Model -> (Model, Maybe Msg)
update message model =
  case message of
    ClientPost post ->
      Maybe.map (updatePost post model) model.chat
        |> Maybe.withDefault (model, Nothing)
    ClientUpdateName name ->
      ( model
      , Just message
      )
    ServerInit init ->
      ( { model
        | chat = Just init
        }
      , Nothing
      )
    ServerConnection id ->
      ( model
      , Nothing
      )
    ServerPost id post ->
      ( model
      , Nothing
      )
    ServerUpdateName id name ->
      ( model
      , Nothing
      )
      -- let
      --   chat = Debug.log "CHAT" (Chat.update message model.chat)
      --   optimisticPosts =
      --     case message of
      --       Chat.Post socket post ->
      --         removeFirst ((==) (socket, post)) model.optimisticPosts
      --       _ -> model.optimisticPosts
      -- in
      --   ( { model
      --     | chat = chat
      --     , optimisticPosts = optimisticPosts
      --     }
      --   , Nothing
      --   )

-- removeFirst : (a -> Bool) -> List a -> List a
-- removeFirst predicate list =
--   let
--     (h, t) = ListExtra.break predicate list
--     tail = Maybe.withDefault [] (List.tail t)
--   in
--     List.append h tail


-- Encoding

-- encodeValue : String -> String -> String
-- encodeValue kind value =
--   Encode.object
--     [ ("type", Encode.string kind)
--     , ("value", Encode.string value)
--     ]
--   |> Encode.encode 2



-- Decoding

-- decodeMessage = Chat.decodeMessage
