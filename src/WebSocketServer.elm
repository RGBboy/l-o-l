module WebSocketServer exposing
  ( Socket
  , Config
  , close
  , sendToOne
  , sendToMany
  , sendToOthers
  , eventDecoder
  )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode



type alias Socket = String

type alias Config msg =
  { onConnection : Socket -> msg
  , onDisconnection: Socket -> msg
  , onMessage: Socket -> Decoder msg
  }



-- COMMANDS

close : (Encode.Value -> Cmd msg) -> Socket -> Cmd msg
close outputPort = encodeClose >> outputPort

sendToOne : (Encode.Value -> Cmd msg) -> Encode.Value -> Socket -> Cmd msg
sendToOne outputPort = curry (encodeMessage >> outputPort)

sendToMany : (Encode.Value -> Cmd msg) -> Encode.Value -> List Socket -> Cmd msg
sendToMany outputPort message sockets =
  Cmd.batch (List.map (sendToOne outputPort message) sockets)

sendToOthers : (Encode.Value -> Cmd msg) -> Encode.Value -> Socket -> List Socket -> Cmd msg
sendToOthers outputPort message socket sockets =
  let
    others = List.filter ((==) socket) sockets
  in
    sendToMany outputPort message others



-- ENCODE

encodeClose : Socket -> Encode.Value
encodeClose socket =
  Encode.object
    [ ("type", Encode.string "Close")
    , ("id", Encode.string socket)
    ]

encodeMessage : (Encode.Value, Socket) -> Encode.Value
encodeMessage (message, socket) =
  Encode.object
    [ ("type", Encode.string "Message")
    , ("id", Encode.string socket)
    , ("data", message)
    ]



-- DECODE

eventDecoder : Config msg -> Decoder msg
eventDecoder config =
  Decode.field "type" Decode.string
    |> Decode.andThen (msgTypeDecoder config)

msgTypeDecoder : Config msg -> String -> Decoder msg
msgTypeDecoder config kind =
  case kind of
    "Connection" ->
      Decode.field "id" Decode.string
        |> Decode.map config.onConnection
    "Disconnection" ->
      Decode.field "id" Decode.string
        |> Decode.map config.onDisconnection
    "Message" ->
      Decode.field "id" Decode.string
        |> Decode.andThen (config.onMessage >> Decode.field "message")
    _ -> Decode.fail ("Could not decode msg of type " ++ kind)
