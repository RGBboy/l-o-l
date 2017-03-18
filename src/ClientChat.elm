module ClientChat exposing
  ( Public
  , Model
  , init
  , post
  , updateName
  , update
  , InputMsg(ServerConnection, ServerPost, ServerUpdateName)
  , OutputMsg(ClientPost, ClientUpdateName)
  , posts
  , userNames
  , decodeInit
  , decodeMessage
  )

import List.Extra as ListExtra
import Set exposing (Set)
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (custom, required, hardcoded, decode)
import Json.Encode as Encode



-- MODEL

type alias Public = String

type alias Model =
  { id: Public
  , users: Set Public
  , userNames: Dict Public String
  , posts: List (Public, String)
  , optimisticPosts: List (Public, String)
  }

init : Public -> Set Public -> Dict Public String -> List (Public, String) -> Model
init id users userNames posts =
  Model id users userNames posts []

userNames : Model -> Dict Public String
userNames model = model.userNames

posts : Model -> List (Public, String)
posts model =
  List.append model.optimisticPosts model.posts



-- UPDATE

type InputMsg
  = ServerConnection Public
  | ServerPost Public String
  | ServerUpdateName Public String

type OutputMsg
  = ClientPost String
  | ClientUpdateName String

post : String -> Model -> (Model, OutputMsg)
post message model =
  ( { model
    | optimisticPosts = (model.id, message) :: model.optimisticPosts
    }
  , ClientPost message
  )

updateName : String -> Model -> (Model, OutputMsg)
updateName name model =
  ( model
  , ClientUpdateName name
  )

update : InputMsg -> Model -> Model
update message model =
  case message of
    ServerConnection id ->
      { model
      | users = Set.insert id model.users
      }
    ServerPost id post ->
      { model
      | posts = (id, post) :: model.posts
      }
    ServerUpdateName id name ->
      { model
      | userNames = Dict.insert id name model.userNames
      }
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

decodePost : Decoder (Public, String)
decodePost =
  decode (,)
    |> required "id" Decode.string
    |> required "post" Decode.string

decodeInit : Decoder Model
decodeInit =
  decode Model
    |> required "id" Decode.string
    |> required "users" (decode Set.fromList |> custom (Decode.list Decode.string))
    |> required "userNames" (Decode.dict Decode.string)
    |> required "posts" (Decode.list decodePost)
    |> hardcoded []

decodeMessage : Decoder InputMsg
decodeMessage =
  Decode.field "type" Decode.string |> Decode.andThen decodeMessageType

decodeMessageType : String -> Decoder InputMsg
decodeMessageType kind =
  case kind of
    "Connection" ->
      decode ServerConnection
        |> required "id" Decode.string
    "Post" ->
      decode ServerPost
        |> required "id" Decode.string
        |> required "post" Decode.string
    "UpdateName" ->
      decode ServerUpdateName
        |> required "id" Decode.string
        |> required "name" Decode.string
    _ -> Decode.fail "Could not decode Msg"
