port module Server exposing (..)

import Platform exposing (Program)

import WebSocketServer as WSS exposing (Socket, sendToOne, sendToMany)

import Set exposing (Set)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required)
import Json.Encode as Encode

import Navigation exposing (Location)
import UrlParser

import ServerChat



main : Program Never Model Msg
main =
  Platform.program
    { init = init
    , update = update
    , subscriptions = subscriptions
    }

maxConnections : Int
maxConnections = 8



-- PORTS

port inputWSS : (Decode.Value -> msg) -> Sub msg

port outputWSS : Encode.Value -> Cmd msg



-- MODEL

type alias Model = ServerChat.Model

init : (Model, Cmd msg)
init =
  (ServerChat.init, Cmd.none)

onConnection : Socket -> Location -> Msg
onConnection socket location =
  let
    secret = UrlParser.parsePath UrlParser.string location
      |> Maybe.withDefault ""
  in
    ServerChat.Connection socket secret
      |> ServerMsg

onDisconnection : Socket -> Location -> Msg
onDisconnection socket _ =
  ServerChat.Disconnection socket
    |> ServerMsg

onMessage : Socket -> Location -> String -> Msg
onMessage socket _ message =
  Decode.decodeString (ServerChat.decodeInputMsg socket) message
    |> Result.map ServerMsg
    |> Result.withDefault Noop

-- UPDATE

type Msg
  = ServerMsg ServerChat.InputMsg
  | Noop

sendMessage : (Socket, ServerChat.OutputMsg) -> Cmd msg
sendMessage (socket, message) =
  sendToOne outputWSS (Encode.encode 2 (ServerChat.encodeOutputMsg message)) socket

sendMessages : List (Socket, ServerChat.OutputMsg) -> Cmd msg
sendMessages messages =
  Cmd.batch <|
    List.map sendMessage messages

update : Msg -> Model -> (Model, Cmd msg)
update message model =
  case message of
    ServerMsg m ->
      let
        (newModel, messages) = ServerChat.update m model
      in
        ( newModel
        , sendMessages messages
        )
    Noop -> (model, Cmd.none)



-- SUBSCRIPTIONS

decodeConfig =
  { onConnection = onConnection
  , onDisconnection = onDisconnection
  , onMessage = onMessage
  }

decodeMsg : Decode.Value -> Msg
decodeMsg value =
  Decode.decodeValue (WSS.eventDecoder decodeConfig) value
    |> Result.withDefault Noop

subscriptions : Model -> Sub Msg
subscriptions model = inputWSS decodeMsg
