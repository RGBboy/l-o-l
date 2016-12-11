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

close : (Encode.Value -> a) -> Socket -> a
close outputPort = encodeClose >> outputPort

sendToOne : (Encode.Value -> a) -> Encode.Value -> Socket -> a
sendToOne outputPort = curry (encodeMessage >> outputPort)

sendToMany : (Encode.Value -> a) -> Encode.Value -> List Socket -> List a
sendToMany outputPort message sockets =
  List.map (sendToOne outputPort message) sockets

sendToOthers : (Encode.Value -> a) -> Encode.Value -> Socket -> List Socket -> List a
sendToOthers outputPort message socket sockets =
  let
    others = List.filter ((/=) socket) sockets
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
