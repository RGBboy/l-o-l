import Html as H exposing (Html)
import Html.App as Html
import Html.Attributes as A
import Html.Events as E
import WebSocket
import WebSocketServer exposing (Socket)

import Set exposing (Set)
import Dict exposing (Dict)
import Result exposing (Result)

import Json.Decode as Decode exposing (Decoder, (:=))
import Json.Decode.Pipeline exposing (custom, decode, required)
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

type Status
  = Disconnected
  | Connected

type MessageBody
  = Post String
  | Join String

type alias Message = (Socket, MessageBody)

type alias Model =
  { input: String
  , posts: List (Socket, String)
  , connections: Dict Socket String
  , name: String
  , status: Status
  }

type alias ServerModel =
  { messages: List Message
  , connections: List Socket
  }

init : (Model, Cmd Msg)
init =
  ( { input = ""
    , posts = []
    , connections = Dict.empty
    , name = ""
    , status = Disconnected
    }
  , Cmd.none
  )



-- UPDATE

type Msg
  = Input String
  | Send
  | InputName String
  | Connect
  | Disconnect
  | Init ServerModel
  | Connection Socket
  | Disconnection Socket
  | NameIn Socket String
  | NameOut String
  | MessageIn Message
  | Error


reduceInit : (Socket, MessageBody) -> (List (Socket, String), Dict Socket String) -> (List (Socket, String), Dict Socket String)
reduceInit (socket, body) (posts, connections) =
  case body of
    Post message ->
      ((socket, message) :: posts, connections)
    Join name ->
      (posts, Dict.insert socket name connections)

keyValueTuple : k -> (k, String)
keyValueTuple k = (k, "")

update : Msg -> Model -> (Model, Cmd Msg)
update message model =
  case message of
    Input value ->
      ( { model | input = value }
      , Cmd.none
      )
    Send ->
      ( { model | input = "" }
      ,  WebSocket.send server (encode (Post model.input))
      )
    InputName value ->
      ( { model | name = value }
      , Cmd.none
      )
    Connect ->
      ( { model
          | name = ""
          , status = Connected
        }
      , WebSocket.send server (encode (Join model.name))
      )
    Disconnect ->
      ( { model | status = Disconnected }
      , Cmd.none
      )
    Init init ->
      let
        (posts, connections) = List.foldl reduceInit (model.posts, Dict.fromList (List.map keyValueTuple init.connections)) init.messages
      in
        ( { model
          | connections = connections
          , posts = posts
          }
        , Cmd.none
        )
    Connection socket ->
      ( { model | connections = Dict.insert socket "" model.connections }
      , Cmd.none
      )
    Disconnection socket ->
      ( { model | connections = Dict.remove socket model.connections }
      , Cmd.none
      )
    MessageIn (socket, body) ->
      case body of
        Post post ->
          ( { model | posts = (socket, post) :: model.posts }
          , Cmd.none
          )
        Join name ->
          ( { model | connections = Dict.insert socket name model.connections }
          , Cmd.none
          )
    _ -> (model, Cmd.none)



-- SUBSCRIPTIONS

encode : MessageBody -> String
encode = encodeMessageBody >> (Encode.encode 2)

encodeMessageBody : MessageBody -> Encode.Value
encodeMessageBody message =
  case message of
    Post message ->
      Encode.object
        [ ("type", Encode.string "Post")
        , ("value", Encode.string message)
        ]
    Join name ->
      Encode.object
        [ ("type", Encode.string "Join")
        , ("value", Encode.string name)
        ]

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
          |> required "messages" (Decode.list decodeMessage)
          |> required "connections" (Decode.list Decode.string))
    "Connection" ->
      decode Connection
        |> required "id" Decode.string
    "Disconnection" ->
      decode Disconnection
        |> required "id" Decode.string
    "Message" ->
      decode MessageIn
        |> custom decodeMessage
    _ -> decode Error

decodeMessage : Decoder (Socket, MessageBody)
decodeMessage =
  decode (,)
    |> required "id" Decode.string
    |> required "message" decodeMessageBody

decodeMessageBody : Decoder MessageBody
decodeMessageBody =
  Decode.customDecoder
    (("type" := Decode.string) `Decode.andThen` decodeMessageBodyType)
    (Result.fromMaybe "Could not decode message body")

decodeMessageBodyType : String -> Decoder (Maybe MessageBody)
decodeMessageBodyType kind =
  case kind of
    "Post" ->
      decode (Just << Post)
        |> required "value" Decode.string

    "Join" ->
      decode (Just << Join)
        |> required "value" Decode.string
    _ -> decode Nothing


subscriptions : Model -> Sub Msg
subscriptions model =
  if model.status == Connected then
    WebSocket.listen server decodeInput
  else
    Sub.none



-- VIEW

connectionView : (Socket, String) -> Html Msg
connectionView (_, name) =
  let
    name' =
      if (name == "") then
        "Unknown"
      else name
  in
    H.div []
      [ H.span []
          [ H.text (name')
          ]
      ]

connectionsView : Dict Socket String -> Html Msg
connectionsView connections =
  H.div []
    [ H.div []
        [ H.h2 []
            [ H.text "Users"
            ]
        ]
    , H.div []
        (List.map connectionView (Dict.toList connections))
    ]

postView : Dict Socket String -> (Socket, String) -> Html Msg
postView connections (socket, post) =
  let
    name = Maybe.withDefault socket (Dict.get socket connections)
  in
    H.div []
      [ H.span []
          [ H.text name
          , H.text ": "
          , H.text post
          ]
      ]

postsView : Dict Socket String -> List (Socket, String) -> Html Msg
postsView connections messages =
  H.div []
    [ H.div []
        [ H.h2 []
            [ H.text "Messages"
            ]
        ]
    , H.div []
        (List.map (postView connections) messages)
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

connectedView : Model -> Html Msg
connectedView model =
  H.div []
    [ connectionsView model.connections
    , postsView model.connections model.posts
    , H.input
        [ A.placeholder "Message..."
        , A.value model.input
        , E.onInput Input
        , onEnter Send
        ]
        []
    ]

disconnectedView : Model -> Html Msg
disconnectedView model =
  H.div []
    [ H.input
        [ A.placeholder "Name..."
        , A.value model.name
        , E.onInput InputName
        , onEnter Connect
        ]
        []
    ]

view : Model -> Html Msg
view model =
  let
    subview =
      case model.status of
        Connected -> connectedView model
        Disconnected -> disconnectedView model
  in
    H.div []
      [ H.h1 []
          [ H.text "Elm Chat"
          ]
      , subview
      ]
