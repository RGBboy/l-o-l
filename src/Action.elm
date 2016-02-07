module Action where

import Json.Decode as Decode exposing (Decoder, (:=))
import Json.Decode.Extra
import Json.Encode as Encode
import Result.Extra exposing (extract)
import Set exposing (Set)

(|:) = Json.Decode.Extra.apply

type alias Id = String

type alias Message = String

type alias Model
  = { connections: Set Id
    , messages: List Message
    }

type Action
  = Error String
  | Connect Id
  | Disconnect Id
  | Init Id Model
  | Post String

init : Model
init
  = { connections = Set.empty
    , messages = []
    }

update : Action -> Model -> Model
update action model =
  case action of
    Error _ -> model
    Connect id -> { model | connections = Set.insert id model.connections }
    Disconnect id -> { model | connections = Set.remove id model.connections }
    Init id model -> model
    Post message -> { model | messages = message :: model.messages }

encode : Action -> Encode.Value
encode action =
  case action of
    Error message ->
      Encode.object
        [ ("type", Encode.string "Error")
        , ("message", Encode.string message)
        ]
    Connect id ->
      Encode.object
        [ ("type", Encode.string "Connection")
        , ("id", Encode.string id)
        ]

    Disconnect id ->
      Encode.object
        [ ("type", Encode.string "Disconnection")
        , ("id", Encode.string id)
        ]

    Init id { connections, messages } ->
      Encode.object
        [ ("type", Encode.string "Init")
        , ("id", Encode.string id)
        , ("model", Encode.object
          [ ("connections", Encode.list (List.map Encode.string (Set.toList connections)))
          , ("messages", Encode.list (List.map Encode.string messages))
          ])
        ]

    Post message ->
      Encode.object
        [ ("type", Encode.string "Post")
        , ("message", Encode.string message)
        ]

decode : Decode.Value -> Action
decode =
  Decode.decodeValue (("type" := Decode.string) `Decode.andThen` actionInfo)
    >> extract (\e -> Error e)

decodeModel : Decoder Model
decodeModel =
  Decode.succeed Model
    |: ("connections" := Decode.map Set.fromList (Decode.list Decode.string))
    |: ("messages" := (Decode.list Decode.string))

actionInfo : String -> Decoder Action
actionInfo kind =
  case kind of
    "Error" ->
      Decode.succeed Error
        |: ("message" := Decode.string)
    "Connection" ->
      Decode.succeed Connect
        |: ("id" := Decode.string)
    "Disconnection" ->
      Decode.succeed Disconnect
        |: ("id" := Decode.string)
    "Init" ->
      Decode.succeed Init
        |: ("id" := Decode.string)
        |: ("model" := decodeModel)
    "Post" ->
      Decode.succeed Post
        |: ("message" := Decode.string)
    _ ->
      Decode.fail (kind ++ " is not a recognized type for actions")
