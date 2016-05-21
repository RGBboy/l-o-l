import Html as H exposing (Html)
import Html.App as Html
import Html.Attributes as A
import Html.Events as E
import WebSocket

import Set exposing (Set)

import Json.Decode as Decode exposing (Decoder, (:=))
import Json.Decode.Extra
import Json.Encode as Encode

(|:) = Json.Decode.Extra.apply

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


server : String
server =
  "ws://localhost:8080"



-- MODEL

type alias User = String

type alias Model =
  { input: String
  , messages: List String
  , connections: Set String
  }

type alias ServerModel =
  { messages: List String
  , connections: Set String
  }

init : (Model, Cmd Msg)
init =
  ( { input = ""
    , messages = []
    , connections = Set.empty
    }
  , Cmd.none
  )

-- UPDATE

type Msg
  = Input String
  | Send
  | Error
  | Init ServerModel
  | Connection String
  | Disconnection String
  | Message String


update : Msg -> Model -> (Model, Cmd Msg)
update message model =
  case message of
    Input value ->
      ( { model | input = value }
      , Cmd.none
      )
    Send ->
      ( { model | input = "" }
      ,  WebSocket.send server (encode (Message model.input))
      )
    Init init ->
      ( { model
        | connections = init.connections
        , messages = init.messages
        }
      , Cmd.none
      )
    Connection id ->
      ( { model | connections = Set.insert id model.connections }
      , Cmd.none
      )
    Disconnection id ->
      ( { model | connections = Set.remove id model.connections }
      , Cmd.none
      )
    Message message ->
      ( { model | messages = message :: model.messages }
      , Cmd.none
      )
    _ -> (model, Cmd.none)


-- SUBSCRIPTIONS

encode : Msg -> String
encode = encodeMsg >> (Encode.encode 2)

encodeMsg : Msg -> Encode.Value
encodeMsg msg =
  case msg of
    Message message ->
      Encode.object
        [ ("type", Encode.string "Message")
        , ("message", Encode.string message)
        ]
    _ -> Encode.null

decode : String -> Msg
decode value =
  Result.withDefault Error (Decode.decodeString decodeMsg value)

decodeMsg : Decoder Msg
decodeMsg =
  ("type" := Decode.string) `Decode.andThen` decodeMsgType

decodeMsgType : String -> Decoder Msg
decodeMsgType kind =
  case kind of
    "Init" ->
      Decode.map Init
        (Decode.object2 ServerModel
          ("messages" := (Decode.list Decode.string))
          ("connections" := Decode.map Set.fromList (Decode.list Decode.string)))
    "Connection" ->
      Decode.succeed Connection
        |: ("id" := Decode.string)
    "Disconnection" ->
      Decode.succeed Disconnection
        |: ("id" := Decode.string)
    "Message" ->
      Decode.succeed Message
        |: ("message" := Decode.string)
    _ -> Decode.succeed Error

subscriptions : Model -> Sub Msg
subscriptions model =
  WebSocket.listen server decode



-- VIEW

connectionView : String -> Html Msg
connectionView id =
  H.div []
    [ H.span []
        [ H.text ("Connection " ++ id)
        ]
    ]

connectionsView : Set String -> Html Msg
connectionsView connections =
  H.div []
    [ H.div []
        [ H.h2 []
            [ H.text "Connections"
            ]
        ]
    , H.div []
        (List.map connectionView (Set.toList connections))
    ]

messageView : String -> Html Msg
messageView message =
  H.div []
    [ H.span []
        [ H.text message
        ]
    ]

messagesView : List String -> Html Msg
messagesView messages =
  H.div []
    [ H.div []
        [ H.h2 []
            [ H.text "Messages"
            ]
        ]
    , H.div []
        (List.map messageView messages)
    ]

onEnter : Msg -> H.Attribute Msg
onEnter message =
    E.on "keydown"
      (Decode.map
        (always message)
        (Decode.customDecoder E.keyCode is13)
      )

is13 : Int -> Result String ()
is13 code =
  if code == 13 then Ok () else Err "not the right key code"

view : Model -> Html Msg
view model =
  H.div []
    [ H.h1 []
        [ H.text "Network Experiment"
        ]
    , connectionsView model.connections
    , messagesView model.messages
    , H.input
        [ A.placeholder "Message..."
        , A.value model.input
        , E.onInput Input
        , onEnter Send
        ]
        []
    ]
