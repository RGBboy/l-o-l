port module Server exposing (..)

import Set exposing (Set)

import Html exposing (Html)
import Html.App as App

import Json.Decode as Decode exposing (Decoder, (:=))
import Json.Decode.Extra
import Json.Encode as Encode

(|:) = Json.Decode.Extra.apply

-- import Debug

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
  | Connection String
  | Message String

encode : Msg -> Encode.Value
encode msg =
  case msg of
    Error -> Encode.null
    Connection id ->
      Encode.object
        [ ("type", Encode.string "Connection")
        , ("id", Encode.string id)
        ]
    Message message ->
      Encode.object
        [ ("type", Encode.string "Message")
        , ("message", Encode.string message)
        ]

port output : Encode.Value -> Cmd msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Error -> (model, Cmd.none)
    Connection id ->
      ( { model | connections = Set.insert id model.connections }
      , output (encode msg) )
    Message message ->
      ( { model | messages = message :: model.messages }
      , output (encode msg) )

decode : Decode.Value -> Msg
decode value =
  Result.withDefault Error (Decode.decodeValue decodeMsg value)

decodeMsg : Decoder Msg
decodeMsg =
  ("type" := Decode.string) `Decode.andThen` decodeMsgType

decodeMsgType : String -> Decoder Msg
decodeMsgType kind =
  case kind of
    "Connection" ->
      Decode.succeed Connection
        |: ("id" := Decode.string)
    "Message" ->
      Decode.succeed Message
        |: ("message" := Decode.string)
    _ -> Decode.succeed Error

-- Input port of messages from clients
port input : (Decode.Value -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions model =
  input decode
