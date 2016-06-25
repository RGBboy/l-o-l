port module Server exposing (..)

import Set exposing (Set)

import Json.Decode as Decode exposing (Decoder, (:=))
import Json.Decode.Pipeline exposing (decode, required)
import Json.Encode as Encode

import WebSocketServer exposing (Socket, sendToOne, sendToMany)

-- Input port of messages from clients

port input : (Decode.Value -> msg) -> Sub msg

main : Program Never
main =
  WebSocketServer.program
    messageDecoder
    input
    { init = init
    , update = update
    , connection = onConnection
    , disconnection = onDisconnection
    , message = onMessage
    , subscriptions = subscriptions
    }

-- Model

type alias Model =
  { connections: Set Socket
  , messages: List String
  }

init : (Model, Cmd Msg)
init =
  ( { connections = Set.empty
    , messages = []
    }
  , Cmd.none)

-- Update

type Msg =
  Error

onConnection : Socket -> Model -> (Model, Cmd Msg)
onConnection socket model =
  let
    connections = Set.insert socket model.connections
    newModel = { model | connections = connections }
  in
    ( newModel
    , Cmd.batch
      [ sendToOne output (encodeMsg (Init newModel)) socket
      , sendToMany output (encodeMsg (Connection socket)) (Set.toList model.connections)
      ]
    )

onDisconnection : Socket -> Model -> (Model, Cmd Msg)
onDisconnection socket model =
  let
    connections = Set.remove socket model.connections
  in
    ( { model | connections = connections }
    , sendToMany output (encodeMsg (Disconnection socket)) (Set.toList model.connections)
    )

messageDecoder : Decoder String
messageDecoder = Decode.string

onMessage : Socket -> String -> Model -> (Model, Cmd Msg)
onMessage socket message model =
  ( { model | messages = message :: model.messages }
  , sendToMany output (encodeMsg (Message message)) (Set.toList model.connections)
  )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

-- Output

type Output
  = Init Model
  | Connection Socket
  | Disconnection Socket
  | Message String


encodeMsg : Output -> Encode.Value
encodeMsg msg =
  case msg of
    Init model ->
      Encode.object
        [ ("type", Encode.string "Init")
        , ("connections", Encode.list (List.map Encode.string (Set.toList model.connections)) )
        , ("messages", Encode.list (List.map Encode.string model.messages) )
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
    Message message ->
      Encode.object
        [ ("type", Encode.string "Message")
        , ("message", Encode.string message)
        ]

port output : Encode.Value -> Cmd msg
