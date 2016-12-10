module WebSocketServer exposing
  ( Socket
  , Config
  , close
  , sendToOne
  , sendToMany
  , sendToOthers
  , eventDecoder
  )

import Set exposing (Set)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required)
import Json.Encode as Encode

type alias Socket = String

type alias Config msg =
  { onConnection : Socket -> msg
  , onDisconnection: Socket -> msg
  , onMessage : Socket -> Decode.Value -> msg
  }

-- COMMANDS

close : (Encode.Value -> Cmd msg) -> Socket -> Cmd msg
close outputPort socket =
  outputPort (encodeClose socket)

sendToOne : (Encode.Value -> Cmd msg) -> Encode.Value -> Socket -> Cmd msg
sendToOne outputPort message socket =
  outputPort (encodeMessage socket message)

sendToMany : (Encode.Value -> Cmd msg) -> Encode.Value -> List Socket -> Cmd msg
sendToMany outputPort message sockets =
  Cmd.batch (List.map (sendToOne outputPort message) sockets)

sendToOthers : (Encode.Value -> Cmd msg) -> Encode.Value -> Socket -> List Socket -> Cmd msg
sendToOthers outputPort message socket sockets =
  let
    others = List.filter ((==) socket) sockets
  in
    sendToMany outputPort message others

encodeClose : Socket -> Encode.Value
encodeClose socket =
  Encode.object
    [ ("type", Encode.string "Close")
    , ("id", Encode.string socket)
    ]

encodeMessage : Socket -> Encode.Value -> Encode.Value
encodeMessage socket message =
  Encode.object
    [ ("type", Encode.string "Message")
    , ("id", Encode.string socket)
    , ("data", message)
    ]

-- DECODER

eventDecoder : Config msg -> Decoder msg
eventDecoder config =
  Decode.field "type" Decode.string
    |> Decode.andThen (msgTypeDecoder config)

msgTypeDecoder : Config msg -> String -> Decoder msg
msgTypeDecoder config kind =
  case kind of
    "Connection" ->
      decode config.onConnection
        |> required "id" Decode.string
    "Disconnection" ->
      decode config.onDisconnection
        |> required "id" Decode.string
    "Message" ->
      decode config.onMessage
        |> required "id" Decode.string
        |> required "message" Decode.value
    _ -> Decode.fail "Could not decode Msg"
