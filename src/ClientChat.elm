module ClientChat exposing
  ( Model
  , init
  , update
  , Msg(ServerMessage, ClientPost, ClientJoin)
  , connections
  , posts
  , users
  , listen
  , encodeValue
  )

import WebSocket
import WebSocketServer as WSS exposing (Socket)

import List.Extra as ListExtra
import Set exposing (Set)
import Dict exposing (Dict)

import Json.Decode as Decode
import Json.Encode as Encode

import Chat


-- MODEL

type alias Model =
  { server: String
  , chat: Chat.Model
  , optimisticPosts: List (Socket, String)
  }

init : String -> Model
init server =
  { server = server
  , chat = Chat.init
  , optimisticPosts = []
  }

connections : Model -> Set Socket
connections model = model.chat.connections

users : Model -> Dict Socket String
users model = model.chat.users

posts : Model -> List (Socket, String)
posts model =
  List.append model.optimisticPosts model.chat.posts

-- UPDATE

type Msg
  = ServerMessage Chat.Msg
  | ClientPost String
  | ClientJoin String

update : Msg -> Model -> (Model, Cmd a)
update message model =
  case message of
    ServerMessage message ->
      let
        chat = Debug.log "CHAT" (Chat.update message model.chat)
        optimisticPosts =
          case message of
            Chat.Post socket post ->
              removeFirst ((==) (socket, post)) model.optimisticPosts
            _ -> model.optimisticPosts
      in
        ( { model
          | chat = chat
          , optimisticPosts = optimisticPosts
          }
        , Cmd.none
        )
    ClientPost post ->
      ( { model | optimisticPosts = (model.chat.socket, post) :: model.optimisticPosts }
      , WebSocket.send model.server (encodeValue "Post" post)
      )
    ClientJoin name ->
      ( model
      , WebSocket.send model.server (encodeValue "Join" name)
      )



-- SUBSCRIPTIONS

listen : Model -> Sub Msg
listen model = Sub.map ServerMessage (WebSocket.listen model.server Chat.decodeMessage)

decodeMessage = Chat.decodeMessage

encodeValue : String -> String -> String
encodeValue kind value =
  Encode.object
    [ ("type", Encode.string kind)
    , ("value", Encode.string value)
    ]
  |> Encode.encode 2

removeFirst : (a -> Bool) -> List a -> List a
removeFirst predicate list =
  let
    (h, t) = ListExtra.break predicate list
    tail = Maybe.withDefault [] (List.tail t)
  in
    List.append h tail
