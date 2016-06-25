module Server exposing (..)

import Set exposing (Set)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode

import WebSocketServer exposing (Socket, sendToOne, sendToMany)

main : Program Never
main =
  WebSocketServer.program
    messageDecoder
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
  , messages: List (Socket, String)
  }

init : (Model, Cmd Msg)
init =
  ( { connections = Set.empty
    , messages = []
    }
  , Cmd.none)

-- Update

type Msg =
  Noop

onConnection : Socket -> Model -> (Model, Cmd Msg)
onConnection socket model =
  let
    connections = Set.insert socket model.connections
    newModel = { model | connections = connections }
  in
    ( newModel
    , Cmd.batch
      [ sendToOne (encodeMsg (Init newModel)) socket
      , sendToMany (encodeMsg (Connection socket)) (Set.toList model.connections)
      ]
    )

onDisconnection : Socket -> Model -> (Model, Cmd Msg)
onDisconnection socket model =
  let
    connections = Set.remove socket model.connections
  in
    ( { model | connections = connections }
    , sendToMany (encodeMsg (Disconnection socket)) (Set.toList model.connections)
    )

messageDecoder : Decoder String
messageDecoder = Decode.string

onMessage : Socket -> String -> Model -> (Model, Cmd Msg)
onMessage socket message model =
  ( { model | messages = (socket, message) :: model.messages }
  , sendToMany (encodeMsg (Message socket message)) (Set.toList model.connections)
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
  | Message Socket String

encodeMessage : (Socket, String) -> Encode.Value
encodeMessage (socket, message) =
  Encode.object
    [ ("id", Encode.string socket)
    , ("message", Encode.string message)
    ]

encodeMsg : Output -> Encode.Value
encodeMsg msg =
  case msg of
    Init model ->
      Encode.object
        [ ("type", Encode.string "Init")
        , ("connections", Encode.list (List.map Encode.string (Set.toList model.connections)) )
        , ("messages", Encode.list (List.map encodeMessage model.messages) )
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
