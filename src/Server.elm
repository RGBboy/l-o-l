port module Server exposing (..)

import Platform exposing (Program)

import WebSocketServer as WSS exposing (Socket, sendToOne, sendToMany)

import Set exposing (Set)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required)
import Json.Encode as Encode

import Chat



main : Program Never Model (WSS.Event ClientInput)
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

type ClientInput
  = ClientPost String
  | ClientJoin String

init : (Model, Cmd msg)
init =
  (Chat.init, Cmd.none)



-- UPDATE

updateConfig : WSS.Update Model ClientInput msg
updateConfig =
  { onConnection = onConnection
  , onDisconnection = onDisconnection
  , onMessage = onMessage
  }

update : WSS.Event ClientInput -> Model -> (Model, Cmd msg)
update = WSS.update updateConfig

onConnection : Socket -> Model -> (Model, Cmd msg)
onConnection socket model =
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

onDisconnection : Socket -> Model -> (Model, Cmd msg)
onDisconnection socket model =
  -- Handle if we closed the connection immediately
  if (Set.member socket model.connections) then
    ( Chat.update (Chat.Disconnection socket) model
    , sendToMany outputWSS (Chat.encodeMessage (Chat.Disconnection socket)) (Set.toList model.connections)
    )
  else
    (model, Cmd.none)

onMessage : Socket -> ClientInput -> Model -> (Model, Cmd msg)
onMessage socket message model =
  case message of
    ClientPost post ->
      ( Chat.update (Chat.Post socket post) model
      , sendToMany outputWSS (Chat.encodeMessage (Chat.Post socket post)) (Set.toList model.connections)
      )
    ClientJoin name ->
      ( Chat.update (Chat.Join socket name) model
      , sendToMany outputWSS (Chat.encodeMessage (Chat.Join socket name)) (Set.toList model.connections)
      )



-- SUBSCRIPTIONS

subscriptions : Model -> Sub (WSS.Event ClientInput)
subscriptions model = inputWSS (WSS.decodeEvent decodeClientInput)

decodeClientInput : Decoder ClientInput
decodeClientInput =
  Decode.field "type" Decode.string |> Decode.andThen decodeClientInputType


decodeClientInputType : String -> Decoder ClientInput
decodeClientInputType kind =
  case kind of
    "Post" ->
      decode ClientPost
        |> required "value" Decode.string

    "Join" ->
      decode ClientJoin
        |> required "value" Decode.string
    _ -> Decode.fail "Could not decode Msg"
