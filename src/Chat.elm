module Chat exposing
  ( Model
  , init
  , update
  , updateSocket
  , Msg(Init, Connection, Disconnection, Post, Join)
  , encodeMessage
  , decodeMessage
  , decodeLocation
  )

import WebSocketServer as WSS exposing (Socket)

import List.Extra as ListExtra
import Set exposing (Set)
import Dict exposing (Dict)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (custom, decode, required)
import Json.Encode as Encode

import Navigation exposing (Location)

-- MODEL

type alias Model =
  { socket: Socket
  , connections: Set Socket
  , posts: List (Socket, String)
  , users: Dict Socket String
  }

createModel : Socket -> Set Socket -> List (Socket, String) -> Dict Socket String -> Model
createModel socket connections posts users =
  { socket = socket
  , connections = connections
  , posts = posts
  , users = users
  }

init : Model
init =
  { socket = ""
  , connections = Set.empty
  , posts = []
  , users = Dict.empty
  }

maxPosts : Int
maxPosts = 8



-- UPDATE

type Msg
  = Init Model
  | Connection Socket Location
  | Disconnection Socket
  | Post Socket String
  | Join Socket String
  | Noop

removeFirst : (a -> Bool) -> List a -> List a
removeFirst predicate list =
  let
    (h, t) = ListExtra.break predicate list
    tail = Maybe.withDefault [] (List.tail t)
  in
    List.append h tail

update : Msg -> Model -> Model
update message model =
  case message of
    Init init -> init
    Connection socket location ->
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
      { model | posts = List.take maxPosts <| (socket, post) :: model.posts }
    Join socket name ->
      { model | users = Dict.insert socket name model.users }
    Noop -> model

updateSocket : Socket -> Model -> Model
updateSocket socket model =
  { model | socket = socket }

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
        , ("socket", Encode.string model.socket)
        , ("connections", Encode.list (List.map Encode.string (Set.toList model.connections)) )
        , ("posts", Encode.list (List.map encodeInitPost model.posts) )
        , ("users", Encode.object (Dict.toList (Dict.map (always Encode.string) model.users)) )
        ]
    Connection socket location ->
      Encode.object
        [ ("type", Encode.string "Connection")
        , ("id", Encode.string socket)
        , ("location", encodeLocation location) -- Remove this as it should not be sent to the client
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

encodeLocation : Location -> Encode.Value
encodeLocation location =
  Encode.object
    [ ("protocol", Encode.string location.protocol)
    , ("hash", Encode.string location.hash)
    , ("search", Encode.string location.search)
    , ("pathname", Encode.string location.pathname)
    , ("port_", Encode.string location.port_)
    , ("hostname", Encode.string location.hostname)
    , ("host", Encode.string location.host)
    , ("origin", Encode.string location.origin)
    , ("href", Encode.string location.href)
    , ("username", Encode.string location.username)
    , ("password", Encode.string location.password)
    ]

-- DECODE

decodeMessage : String -> Msg
decodeMessage value =
  Result.withDefault Noop (Decode.decodeString decodeMsg value)

decodeMsg : Decoder Msg
decodeMsg =
  Decode.field "type" Decode.string |> Decode.andThen decodeMsgType

decodeMsgType : String -> Decoder Msg
decodeMsgType kind =
  case kind of
    "Init" ->
      decode Init
        |> custom (decode createModel
          |> required "socket" Decode.string
          |> required "connections" (decode Set.fromList |> custom (Decode.list Decode.string))
          |> required "posts" (Decode.list decodePost)
          |> required "users" (Decode.dict Decode.string))
    "Connection" ->
      decode Connection
        |> required "id" Decode.string
        |> required "location" decodeLocation
    "Disconnection" ->
      decode Disconnection
        |> required "id" Decode.string
    "Join" ->
      decode Join
        |> required "id" Decode.string
        |> required "value" Decode.string
    "Post" ->
      decode Post
        |> required "id" Decode.string
        |> required "value" Decode.string
    _ -> Decode.fail "Could not decode Msg"

decodeLocation : Decoder Location
decodeLocation =
  decode Location
    |> required "protocol" Decode.string
    |> required "hash" Decode.string
    |> required "search" Decode.string
    |> required "pathname" Decode.string
    |> required "port_" Decode.string
    |> required "hostname" Decode.string
    |> required "host" Decode.string
    |> required "origin" Decode.string
    |> required "href" Decode.string
    |> required "username" Decode.string
    |> required "password" Decode.string

decodePost : Decoder (Socket, String)
decodePost =
  decode (,)
    |> required "id" Decode.string
    |> required "post" Decode.string
