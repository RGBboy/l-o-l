module WebSocketServer exposing
  ( Socket
  , Event
  , close
  , sendToOne
  , sendToMany
  , Update
  , update
  , decodeEvent
  )

import Set exposing (Set)

import Json.Decode as Decode exposing (Decoder, (:=))
import Json.Decode.Pipeline exposing (decode, required)
import Json.Encode as Encode

type alias Socket = String

type Event a
  = Connection Socket
  | Disconnection Socket
  | Message Socket a
  | Error

type alias Update a b msg =
  { onConnection : Socket -> a -> (a, Cmd msg)
  , onDisconnection: Socket -> a -> (a, Cmd msg)
  , onMessage : Socket -> b -> a -> (a, Cmd msg)
  }

update : Update a b msg -> Event b -> a -> (a, Cmd msg)
update config message model =
  case message of
    Connection socket -> config.onConnection socket model
    Disconnection socket -> config.onDisconnection socket model
    Message socket message -> config.onMessage socket message model
    Error -> (model, Cmd.none)

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

decodeEvent : Decoder a -> Decode.Value -> Event a
decodeEvent decodeMessage value =
  Result.withDefault Error (Decode.decodeValue (msgDecoder decodeMessage) value)

msgDecoder : Decoder a -> Decoder (Event a)
msgDecoder decodeMessage =
  ("type" := Decode.string) `Decode.andThen` (msgTypeDecoder decodeMessage)

msgTypeDecoder : Decoder a -> String -> Decoder (Event a)
msgTypeDecoder decodeMessage kind =
  case kind of
    "Connection" ->
      decode Connection
        |> required "id" Decode.string
    "Disconnection" ->
      decode Disconnection
        |> required "id" Decode.string
    "Message" ->
      decode Message
        |> required "id" Decode.string
        |> required "message" decodeMessage
    _ -> decode Error
