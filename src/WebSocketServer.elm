port module WebSocketServer exposing
  ( Socket
  , Event(Connection, Disconnection, Message, Error)
  , programWithFlags
  , program
  , sendToOne
  , sendToMany
  )

import Set exposing (Set)

import Html exposing (Html)
import Html.App as App

import Json.Decode as Decode exposing (Decoder, (:=))
import Json.Decode.Pipeline exposing (decode, required)
import Json.Encode as Encode

-- Ports

port inputWSS : (Decode.Value -> msg) -> Sub msg

port outputWSS : Encode.Value -> Cmd msg

-- Programs

type alias Socket = String

type Msg a b
  = WSSEvent (Event a)
  | UserMsg b

type Event a
  = Connection Socket
  | Disconnection Socket
  | Message Socket a
  | Error

programWithFlags
  : Decoder a
  ->
    { init : flags -> (model, Cmd msg)
    , update : msg -> model -> (model, Cmd msg)
    , onEvent : Event a -> model -> (model, Cmd msg)
    , subscriptions : model -> Sub msg
    }
  -> Program flags
programWithFlags decoder app =
  let
    update msg model =
      updateHelp UserMsg <|
        case msg of
          WSSEvent event -> app.onEvent event model
          UserMsg userMsg ->
            app.update userMsg model

    subs model =
      Sub.batch
        [ inputWSS ((decodeInput decoder) >> WSSEvent)
        , Sub.map UserMsg (app.subscriptions model)
        ]

    init flags =
      updateHelp UserMsg (app.init flags)
  in
    App.programWithFlags
      { init = init
      , view = always (Html.text "") -- hack for server program to work
      , update = update
      , subscriptions = subs
      }


updateHelp : (a -> b) -> (model, Cmd a) -> (model, Cmd b)
updateHelp func (model, cmds) =
  (model, Cmd.map func cmds)

program
  : Decoder a
  ->
    { init : (model, Cmd msg)
    , update : msg -> model -> (model, Cmd msg)
    , onEvent : Event a -> model -> (model, Cmd msg)
    , subscriptions : model -> Sub msg
    }
  -> Program Never
program decoder app =
  programWithFlags decoder { app | init = \_ -> app.init }

-- Commands

sendToOne : Encode.Value -> Socket -> Cmd msg
sendToOne message socket =
  outputWSS (encodeAddressedMsg socket message)

sendToMany : Encode.Value -> List Socket -> Cmd msg
sendToMany message sockets =
  Cmd.batch (List.map (sendToOne message) sockets)

encodeAddressedMsg : Socket -> Encode.Value -> Encode.Value
encodeAddressedMsg id message =
  Encode.object
    [ ("to", Encode.string id)
    , ("data", message)
    ]

-- Subscriptions

decodeInput : Decoder a -> Decode.Value -> Event a
decodeInput decodeMessage value =
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
