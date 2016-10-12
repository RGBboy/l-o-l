port module Server exposing (..)

import Set exposing (Set)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode

import Html exposing (..)
import Html.App as App

import WebSocketServer as WSS exposing (Socket, sendToOne, sendToMany)

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
  , messages: List (Socket, String)
  }

init : (Model, Cmd msg)
init =
  ( { connections = Set.empty
    , messages = []
    }
  , Cmd.none)



-- UPDATE

update : WSS.Event String -> Model -> (Model, Cmd msg)
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

onMessage : Socket -> String -> Model -> (Model, Cmd msg)
onMessage socket message model =
  ( { model | messages = (socket, message) :: model.messages }
  , sendToMany outputWSS (encodeMsg (Message socket message)) (Set.toList model.connections)
  )

messageDecoder : Decoder String
messageDecoder = Decode.string

subscriptions : Model -> Sub (WSS.Event String)
subscriptions model = inputWSS (WSS.decodeEvent messageDecoder)



-- OUTPUT

type OutputMsg
  = Init Model
  | Connection Socket
  | Disconnection Socket
  | Message Socket String

encodeMessage : (Socket, String) -> Encode.Value
encodeMessage (socket, message) =
  Encode.object
    [ ("id", Encode.string socket)
    , ("message", Encode.string message)
    ]

encodeMsg : OutputMsg -> Encode.Value
encodeMsg msg =
  case msg of
    Init model ->
      Encode.object
        [ ("type", Encode.string "Init")
        , ("messages", Encode.list (List.map encodeMessage model.messages) )
        , ("connections", Encode.list (List.map Encode.string (Set.toList model.connections)) )
        ]
    Connection id ->
      Encode.object
        [ ("type", Encode.string "Connection")
        , ("id", Encode.string id)
        ]
    Disconnection id ->
      Encode.object
        [ ("type", Encode.string "Disconnection")
        , ("id", Encode.string id)
        ]
    Message id message ->
      Encode.object
        [ ("type", Encode.string "Message")
        , ("id", Encode.string id)
        , ("message", Encode.string message)
        ]
