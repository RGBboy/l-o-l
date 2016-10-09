module WebSocketServer exposing
  ( Socket
  , Event(Connection, Disconnection, Message, Error)
  , sendToOne
  , sendToMany
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


-- COMMANDS

sendToOne : (Encode.Value -> Cmd msg) -> Encode.Value -> Socket -> Cmd msg
sendToOne outputPort message socket =
  outputPort (encodeAddressedMsg socket message)

sendToMany : (Encode.Value -> Cmd msg) -> Encode.Value -> List Socket -> Cmd msg
sendToMany outputPort message sockets =
  Cmd.batch (List.map (sendToOne outputPort message) sockets)

encodeAddressedMsg : Socket -> Encode.Value -> Encode.Value
encodeAddressedMsg socket message =
  Encode.object
    [ ("to", Encode.string socket)
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
