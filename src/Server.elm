port module Server exposing (..)

import Platform exposing (Program)

import WebSocketServer as WSS exposing (Socket, sendToOne, sendToMany)

import Set exposing (Set)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required)
import Json.Encode as Encode

import Chat



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

type alias Model = Chat.Model

type Msg
  = Connection Socket
  | Disconnection Socket
  | Post Socket String
  | Join Socket String
  | Noop

init : (Model, Cmd msg)
init =
  (Chat.init, Cmd.none)



-- UPDATE

decodeConfig : WSS.Config Msg
decodeConfig =
  { onConnection = Connection
  , onDisconnection = Disconnection
  , onMessage = decodeClientMessage
  }

update : Msg -> Model -> (Model, Cmd msg)
update message model =
  case message of
    Connection socket ->
      if (Set.size model.connections) <= maxConnections then
        let
          newModel = Chat.updateSocket socket (Chat.update (Chat.Connection socket) model)
        in
          ( newModel
          , Cmd.batch
            [ sendToOne outputWSS (Chat.encodeMessage (Chat.Init newModel)) socket
            , sendToMany outputWSS (Chat.encodeMessage (Chat.Connection socket)) (Set.toList model.connections)
            ]
          )
      else
        (model, WSS.close outputWSS socket)
    Disconnection socket ->
      -- Handle if we closed the connection immediately
      if (Set.member socket model.connections) then
        ( Chat.update (Chat.Disconnection socket) model
        , sendToMany outputWSS (Chat.encodeMessage (Chat.Disconnection socket)) (Set.toList model.connections)
        )
      else
        (model, Cmd.none)
    Post socket post ->
      ( Chat.update (Chat.Post socket post) model
      , sendToMany outputWSS (Chat.encodeMessage (Chat.Post socket post)) (Set.toList model.connections)
      )
    Join socket name ->
      ( Chat.update (Chat.Join socket name) model
      , sendToMany outputWSS (Chat.encodeMessage (Chat.Join socket name)) (Set.toList model.connections)
      )
    Noop -> (model, Cmd.none)



-- SUBSCRIPTIONS

decodeMsg : Decode.Value -> Msg
decodeMsg value =
  Result.withDefault Noop (Decode.decodeValue (WSS.eventDecoder decodeConfig) value)

subscriptions : Model -> Sub Msg
subscriptions model = inputWSS decodeMsg

decodeClientMessage : Socket -> Decode.Value -> Msg
decodeClientMessage socket value =
  Result.withDefault Noop (Decode.decodeValue (decodeClientInput socket) value)

decodeClientInput : Socket -> Decoder Msg
decodeClientInput socket =
  Decode.field "type" Decode.string |> Decode.andThen (decodeClientInputType socket)

decodeClientInputType : String -> String -> Decoder Msg
decodeClientInputType socket kind =
  case kind of
    "Post" ->
      decode (Post socket)
        |> required "value" Decode.string

    "Join" ->
      decode (Join socket)
        |> required "value" Decode.string
    _ -> Decode.fail "Could not decode Msg"
