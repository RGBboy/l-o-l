port module Server exposing (..)

import Html exposing (..)
import Html.App as App

import WebSocketServer as WSS exposing (Socket, sendToOne, sendToMany)

import Set exposing (Set)
import Dict exposing (Dict)

import Json.Decode as Decode exposing (Decoder, (:=))
import Json.Decode.Pipeline exposing (decode, required)
import Json.Encode as Encode



main : Program Never
main =
  App.program
    { init = init
    , update = update
    , view = always (Html.text "") -- hack for server program to work
    , subscriptions = subscriptions
    }

maxConnections : Int
maxConnections = 8



-- PORTS

port inputWSS : (Decode.Value -> msg) -> Sub msg

port outputWSS : Encode.Value -> Cmd msg



-- MODEL

type alias Model =
  { connections: Set Socket
  , posts: List (Socket, String)
  , users: Dict Socket String
  }

type MessageBody
  = Post String
  | Join String

init : (Model, Cmd msg)
init =
  ( { connections = Set.empty
    , posts = []
    , users = Dict.empty
    }
  , Cmd.none)



-- UPDATE

update : WSS.Event MessageBody -> Model -> (Model, Cmd msg)
update message model =
  case message of
    WSS.Connection socket -> onConnection socket model
    WSS.Disconnection socket -> onDisconnection socket model
    WSS.Message socket message -> onMessage socket message model
    WSS.Error -> (model, Cmd.none)

onConnection : Socket -> Model -> (Model, Cmd msg)
onConnection socket model =
  if (Set.size model.connections) <= maxConnections then
    let
      connections = Set.insert socket model.connections
      newModel = { model | connections = connections }
    in
      ( newModel
      , Cmd.batch
        [ sendToOne outputWSS (encodeMsg (Init newModel)) socket
        , sendToMany outputWSS (encodeMsg (Connection socket)) (Set.toList model.connections)
        ]
      )
  else
    (model, WSS.close outputWSS socket)

onDisconnection : Socket -> Model -> (Model, Cmd msg)
onDisconnection socket model =
  if (Set.member socket model.connections) then
    let
      exists = Set.member socket model.connections
      connections = Set.remove socket model.connections
    in
      ( { model | connections = connections }
      , sendToMany outputWSS (encodeMsg (Disconnection socket)) (Set.toList model.connections)
      )
  else
    (model, Cmd.none)

onMessage : Socket -> MessageBody -> Model -> (Model, Cmd msg)
onMessage socket message model =
  case message of
    Post post ->
      ( { model | posts = (socket, post) :: model.posts }
      , sendToMany outputWSS (encodeMsg (Message (socket, message))) (Set.toList model.connections)
      )
    Join name ->
      ( { model | users = Dict.insert socket name model.users }
      , sendToMany outputWSS (encodeMsg (Message (socket, message))) (Set.toList model.connections)
      )



-- SUBSCRIPTIONS

subscriptions : Model -> Sub (WSS.Event MessageBody)
subscriptions model = inputWSS (WSS.decodeEvent decodeMessageBody)

decodeMessageBody : Decoder MessageBody
decodeMessageBody =
  Decode.customDecoder
    (("type" := Decode.string) `Decode.andThen` decodeMessageBodyType)
    (Result.fromMaybe "Could not decode message body")

decodeMessageBodyType : String -> Decoder (Maybe MessageBody)
decodeMessageBodyType kind =
  case kind of
    "Post" ->
      decode (Just << Post)
        |> required "value" Decode.string

    "Join" ->
      decode (Just << Join)
        |> required "value" Decode.string
    _ -> decode Nothing



-- OUTPUT

type OutputMsg
  = Init Model
  | Connection Socket
  | Disconnection Socket
  | Message (Socket, MessageBody)

encodeInitPost : (Socket, String) -> Encode.Value
encodeInitPost (socket, post) =
  Encode.object
    [ ("id", Encode.string socket)
    , ("post", Encode.string post)
    ]

encodeMessageBody : MessageBody -> Encode.Value
encodeMessageBody body =
  case body of
    Post post ->
      Encode.object
        [ ("type", Encode.string "Post")
        , ("value", Encode.string post)
        ]
    Join name ->
      Encode.object
        [ ("type", Encode.string "Join")
        , ("value", Encode.string name)
        ]

encodeMsg : OutputMsg -> Encode.Value
encodeMsg msg =
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
    Message (socket, body) ->
      Encode.object
        [ ("type", Encode.string "Message")
        , ("id", Encode.string socket)
        , ("message", encodeMessageBody body)
        ]
