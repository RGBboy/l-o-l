module Chat exposing
  ( Model
  , init
  , update
  , Msg(Init, Connection, Disconnection, Post, Join)
  , encodeMessage
  , decodeMessage
  )

import WebSocketServer as WSS exposing (Socket)

import Set exposing (Set)
import Dict exposing (Dict)

import Json.Decode as Decode exposing (Decoder, (:=))
import Json.Decode.Pipeline exposing (custom, decode, required)
import Json.Encode as Encode



-- MODEL

type alias Model =
  { connections: Set Socket
  , posts: List (Socket, String)
  , users: Dict Socket String
  }

init : Model
init =
  { posts = []
  , connections = Set.empty
  , users = Dict.empty
  }

maxPosts : Int
maxPosts = 4



-- UPDATE

type Msg
  = Init Model
  | Connection Socket
  | Disconnection Socket
  | Post Socket String
  | Join Socket String
  | Noop

update : Msg -> Model -> Model
update message model =
  case message of
    Init init -> init
    Connection socket ->
      { model
      | connections = Set.insert socket model.connections
      , users = Dict.insert socket "" model.users
      }
    Disconnection socket ->
      let
        connections = Set.remove socket model.connections
      in
        { model
        | connections = connections
        , users = calcUsers model.posts connections model.users
        }
    Post socket post ->
      let
        newPosts = List.take maxPosts <| (socket, post) :: model.posts
      in
        { model | posts = newPosts }
    Join socket name ->
      { model | users = Dict.insert socket name model.users }
    _ -> model

toEmptyTuple : Socket -> (Socket, String)
toEmptyTuple socket = (socket, "")

-- filter users to only those that exist in either posts or connections
calcUsers : List (Socket, String) -> Set Socket -> Dict Socket String -> Dict Socket String
calcUsers posts connections users =
  let
    postDict = Dict.fromList posts
    connectionsDict = Set.toList connections |> List.map toEmptyTuple |> Dict.fromList
    unionDict = Dict.union postDict connectionsDict
  in
    Dict.intersect users unionDict



-- ENCODE

encodeMessage : Msg -> Encode.Value
encodeMessage msg =
  case msg of
    Init model ->
      Encode.object
        [ ("type", Encode.string "Init")
        , ("connections", Encode.list (List.map Encode.string (Set.toList model.connections)) )
        , ("posts", Encode.list (List.map encodeInitPost model.posts) )
        , ("users", Encode.object (Dict.toList (Dict.map (always Encode.string) model.users)) )
        ]
    Connection socket ->
      Encode.object
        [ ("type", Encode.string "Connection")
        , ("id", Encode.string socket)
        ]
    Disconnection socket ->
      Encode.object
        [ ("type", Encode.string "Disconnection")
        , ("id", Encode.string socket)
        ]
    Join socket name ->
      Encode.object
        [ ("type", Encode.string "Join")
        , ("id", Encode.string socket)
        , ("value", Encode.string name)
        ]
    Post socket message ->
      Encode.object
        [ ("type", Encode.string "Post")
        , ("id", Encode.string socket)
        , ("value", Encode.string message)
        ]
    _ -> Encode.null

encodeInitPost : (Socket, String) -> Encode.Value
encodeInitPost (socket, post) =
  Encode.object
    [ ("id", Encode.string socket)
    , ("post", Encode.string post)
    ]



-- DECODE

decodeMessage : String -> Msg
decodeMessage value =
  Result.withDefault Noop (Decode.decodeString decodeMsg value)

decodeMsg : Decoder Msg
decodeMsg =
  Decode.customDecoder
    (("type" := Decode.string) `Decode.andThen` decodeMsgType)
    (Result.fromMaybe "Could not decode msg")

decodeMsgType : String -> Decoder (Maybe Msg)
decodeMsgType kind =
  case kind of
    "Init" ->
      decode (Just << Init)
        |> custom (decode Model
          |> required "connections" (decode Set.fromList |> custom (Decode.list Decode.string))
          |> required "posts" (Decode.list decodeInitPost)
          |> required "users" (Decode.dict Decode.string))
    "Connection" ->
      decode (Just << Connection)
        |> required "id" Decode.string
    "Disconnection" ->
      decode (Just << Disconnection)
        |> required "id" Decode.string
    "Join" ->
      decode Just
        |> custom (decode Join
          |> required "id" Decode.string
          |> required "value" Decode.string)
    "Post" ->
      decode Just
        |> custom (decode Post
          |> required "id" Decode.string
          |> required "value" Decode.string)
    _ -> decode Nothing

decodeInitPost : Decoder (Socket, String)
decodeInitPost =
  decode (,)
    |> required "id" Decode.string
    |> required "post" Decode.string
