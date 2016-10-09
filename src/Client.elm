import Html as H exposing (Html)
import Html.App as Html
import Html.Attributes as A
import Html.Events as E
import WebSocket
import WebSocketServer exposing (Socket)

import Set exposing (Set)

import Json.Decode as Decode exposing (Decoder, (:=))
import Json.Decode.Pipeline exposing (decode, required)
import Json.Encode as Encode

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
  , messages: List (Socket, String)
  , connections: Set Socket
  }

type alias ServerModel =
  { messages: List (Socket, String)
  , connections: Set Socket
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
  | Connection Socket
  | Disconnection Socket
  | MessageIn Socket String
  | MessageOut String


update : Msg -> Model -> (Model, Cmd Msg)
update message model =
  case message of
    Input value ->
      ( { model | input = value }
      , Cmd.none
      )
    Send ->
      ( { model | input = "" }
      ,  WebSocket.send server (encode (MessageOut model.input))
      )
    Init init ->
      ( { model
        | connections = init.connections
        , messages = init.messages
        }
      , Cmd.none
      )
    Connection socket ->
      ( { model | connections = Set.insert socket model.connections }
      , Cmd.none
      )
    Disconnection socket ->
      ( { model | connections = Set.remove socket model.connections }
      , Cmd.none
      )
    MessageIn socket message ->
      ( { model | messages = (socket, message) :: model.messages }
      , Cmd.none
      )
    _ -> (model, Cmd.none)



-- SUBSCRIPTIONS

encode : Msg -> String
encode = encodeMsg >> (Encode.encode 2)

encodeMsg : Msg -> Encode.Value
encodeMsg msg =
  case msg of
    MessageOut message ->
      Encode.string message
    _ -> Encode.null

decodeInput : String -> Msg
decodeInput value =
  Result.withDefault Error (Decode.decodeString msgDecoder value)

msgDecoder : Decoder Msg
msgDecoder =
  ("type" := Decode.string) `Decode.andThen` msgTypeDecoder

msgTypeDecoder : String -> Decoder Msg
msgTypeDecoder kind =
  case kind of
    "Init" ->
      Decode.map Init
        (decode ServerModel
          |> required "messages" (Decode.list messageDecoder)
          |> required "connections" (Decode.map Set.fromList (Decode.list Decode.string)))
    "Connection" ->
      decode Connection
        |> required "id" Decode.string
    "Disconnection" ->
      decode Disconnection
        |> required "id" Decode.string
    "Message" ->
      decode MessageIn
        |> required "id" Decode.string
        |> required "message" Decode.string
    _ -> decode Error

messageDecoder : Decoder (String, String)
messageDecoder =
  decode (,)
    |> required "id" Decode.string
    |> required "message" Decode.string

subscriptions : Model -> Sub Msg
subscriptions model =
  WebSocket.listen server decodeInput



-- VIEW

connectionView : Socket -> Html Msg
connectionView socket =
  H.div []
    [ H.span []
        [ H.text ("Connection " ++ socket)
        ]
    ]

connectionsView : Set Socket -> Html Msg
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

messageView : (Socket, String) -> Html Msg
messageView (socket, message) =
  H.div []
    [ H.span []
        [ H.text socket
        , H.text ": "
        , H.text message
        ]
    ]

messagesView : List (Socket, String)-> Html Msg
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
        [ H.text "Elm Chat"
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
