port module Server exposing (..)

import Set exposing (Set)

import Html exposing (Html)
import Html.App as App

import Json.Decode as Decode exposing (Decoder, (:=))
import Json.Decode.Pipeline exposing (decode, required)
import Json.Encode as Encode

main : Program Never
main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- View

-- hack for server program to work
view : Model -> Html Msg
view = always (Html.text "")

-- Model

type alias Model =
  { connections: Set String
  , messages: List String
  }

init : (Model, Cmd Msg)
init =
  ( { connections = Set.empty
    , messages = []
    }
  , Cmd.none)

-- Update

type Msg
  = Error
  | Init Model
  | Connection String
  | Disconnection String
  | Message String

encodeMsg : Msg -> Encode.Value
encodeMsg msg =
  case msg of
    Init model ->
      Encode.object
        [ ("type", Encode.string "Init")
        , ("connections", Encode.list (List.map Encode.string (Set.toList model.connections)) )
        , ("messages", Encode.list (List.map Encode.string model.messages) )
        ]
    Connection id ->
      Encode.object
        [ ("type", Encode.string "Connection")
        , ("id", Encode.string id)
        ]
    Disconnection id ->
      Encode.object
        [ ("type", Encode.string "Disconnection")
        , ("id", Encode.string id)
        ]
    Message message ->
      Encode.object
        [ ("type", Encode.string "Message")
        , ("message", Encode.string message)
        ]
    _ -> Encode.null

encodeAddressedMsg : String -> Msg -> Encode.Value
encodeAddressedMsg id msg =
  Encode.object
    [ ("to", Encode.string id)
    , ("data", encodeMsg msg)
    ]

port output : Encode.Value -> Cmd msg

-- Tag a command with who it is for?
sendToOne : Msg -> String -> Cmd msg
sendToOne msg id =
  output (encodeAddressedMsg id msg)

sendToMany : Msg -> List String -> Cmd msg
sendToMany msg ids =
  Cmd.batch (List.map (sendToOne msg) ids)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Connection id ->
      let
        newModel = { model | connections = Set.insert id model.connections }
      in
        ( newModel
        , Cmd.batch
          [ sendToOne (Init newModel) id
          , sendToMany msg (Set.toList model.connections)
          ]
        )
    Disconnection id ->
      let
        connections = Set.remove id model.connections
      in
        ( { model | connections = connections }
        , sendToMany msg (Set.toList connections)
        )
    Message message ->
      ( { model | messages = message :: model.messages }
      , sendToMany msg (Set.toList model.connections)
      )
    _ -> (model, Cmd.none)

decodeInput : Decode.Value -> Msg
decodeInput value =
  Result.withDefault Error (Decode.decodeValue msgDecoder value)

msgDecoder : Decoder Msg
msgDecoder =
  ("type" := Decode.string) `Decode.andThen` msgTypeDecoder

msgTypeDecoder : String -> Decoder Msg
msgTypeDecoder kind =
  case kind of
    "Connection" ->
      decode Connection
        |> required "id" Decode.string
    "Disconnection" ->
      decode Disconnection
        |> required "id" Decode.string
    "Message" ->
      decode Message
        |> required "message" Decode.string
    _ -> decode Error

-- Input port of messages from clients
port input : (Decode.Value -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions model =
  let
    model = Debug.log "Model" model
  in
    input decodeInput
