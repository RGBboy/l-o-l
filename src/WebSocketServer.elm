port module WebSocketServer exposing (..)

import Set exposing (Set)

import Html exposing (Html)
import Html.App as App

import Json.Decode as Decode exposing (Decoder, (:=))
import Json.Decode.Pipeline exposing (decode, required)
import Json.Encode as Encode


-- Programs

type alias Socket = String

type Msg a msg
  = Connection Socket
  | Disconnection Socket
  | Message Socket a
  | Error
  | UserMsg msg

programWithFlags
  : Decoder a
  -> ((Decode.Value -> Msg a msg) -> Sub (Msg a msg))
  ->
    { init : flags -> (model, Cmd msg)
    , update : msg -> model -> (model, Cmd msg)
    , connection : Socket -> model -> (model, Cmd msg)
    , disconnection : Socket -> model -> (model, Cmd msg)
    , message : Socket -> a -> model -> (model, Cmd msg)
    , subscriptions : model -> Sub msg
    }
  -> Program flags
programWithFlags decoder input app =
  let
    update msg model =
      updateHelp UserMsg <|
        case msg of
          Connection socket ->
            app.connection socket model
          Disconnection socket ->
            app.disconnection socket model
          Message socket value ->
            app.message socket value model
          Error -> (model, Cmd.none)
          UserMsg userMsg ->
            app.update userMsg model

    subs model =
      Sub.batch
        [ input (decodeInput decoder)
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
  -> ((Decode.Value -> Msg a msg) -> Sub (Msg a msg))
  ->
    { init : (model, Cmd msg)
    , update : msg -> model -> (model, Cmd msg)
    , connection : Socket -> model -> (model, Cmd msg)
    , disconnection : Socket -> model -> (model, Cmd msg)
    , message : Socket -> a -> model -> (model, Cmd msg)
    , subscriptions : model -> Sub msg
    }
  -> Program Never
program decoder input app =
  programWithFlags decoder input { app | init = \_ -> app.init }



-- Commands

sendToOne : (Encode.Value -> Cmd msg) -> Encode.Value -> Socket -> Cmd msg
sendToOne output message socket =
  output (encodeAddressedMsg socket message)

sendToMany : (Encode.Value -> Cmd msg) -> Encode.Value -> List Socket -> Cmd msg
sendToMany output message sockets =
  Cmd.batch (List.map (sendToOne output message) sockets)

encodeAddressedMsg : Socket -> Encode.Value -> Encode.Value
encodeAddressedMsg id message =
  Encode.object
    [ ("to", Encode.string id)
    , ("data", message)
    ]

-- Subscriptions

decodeInput : Decoder a -> Decode.Value -> Msg a b
decodeInput decodeMessage value =
  Result.withDefault Error (Decode.decodeValue (msgDecoder decodeMessage) value)

msgDecoder : Decoder a -> Decoder (Msg a b)
msgDecoder decodeMessage =
  ("type" := Decode.string) `Decode.andThen` (msgTypeDecoder decodeMessage)

msgTypeDecoder : Decoder a -> String -> Decoder (Msg a b)
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
