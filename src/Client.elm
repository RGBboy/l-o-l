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

type alias Model =
  { input: String
  , posts: List (Socket, String)
  , connections: Set Socket
  , users: Dict Socket String
  , name: String
  , status: Status
  }

type alias ServerModel =
  { connections: Set Socket
  , posts: List (Socket, String)
  , users: Dict Socket String
  }

init : (Model, Cmd Msg)
init =
  ( { input = ""
    , posts = []
    , connections = Set.empty
    , users = Dict.empty
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
  | Message (Socket, MessageBody)
  | Noop

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
      ( { model
        | connections = init.connections
        , posts = init.posts
        , users = init.users
        }
      , Cmd.none
      )
    Connection socket ->
      ( { model
        | connections = Set.insert socket model.connections
        , users = Dict.insert socket "" model.users
        }
      , Cmd.none
      )
    Disconnection socket ->
      ( { model | connections = Set.remove socket model.connections }
      , Cmd.none
      )
    Message (socket, body) ->
      case body of
        Post post ->
          ( { model | posts = (socket, post) :: model.posts }
          , Cmd.none
          )
        Join name ->
          ( { model | users = Dict.insert socket name model.users }
          , Cmd.none
          )
    _ -> (model, Cmd.none)



-- SUBSCRIPTIONS

encode : MessageBody -> String
encode = encodeMessageBody >> (Encode.encode 2)

encodeMessageBody : MessageBody -> Encode.Value
encodeMessageBody body =
  case body of
    Post post ->
      Encode.object
        [ ("type", Encode.string "Post")
        , ("value", Encode.string post)
        ]
    Join name ->
      Encode.object
        [ ("type", Encode.string "Join")
        , ("value", Encode.string name)
        ]

decodeInput : String -> Msg
decodeInput value =
  Result.withDefault Noop (Decode.decodeString decodeMsg value)

decodeMsg : Decoder Msg
decodeMsg =
  Decode.customDecoder
    (("type" := Decode.string) `Decode.andThen` decodeMsgType)
    (Result.fromMaybe "Could not decode msg")

decodeMsgType : String -> Decoder (Maybe Msg)
decodeMsgType kind =
  case kind of
    "Init" ->
      decode (Just << Init)
        |> custom (decode ServerModel
          |> required "connections" (decode Set.fromList |> custom (Decode.list Decode.string))
          |> required "posts" (Decode.list decodeInitPost)
          |> required "users" (Decode.dict Decode.string))
    "Connection" ->
      decode (Just << Connection)
        |> required "id" Decode.string
    "Disconnection" ->
      decode (Just << Disconnection)
        |> required "id" Decode.string
    "Message" ->
      decode (Just << Message)
        |> custom decodeMessage
    _ -> decode Nothing

decodeInitPost : Decoder (Socket, String)
decodeInitPost =
  decode (,)
    |> required "id" Decode.string
    |> required "post" Decode.string

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

connectionView : Dict Socket String -> Socket -> Html Msg
connectionView names socket =
  let
    name' = Maybe.withDefault "Unknown" (Dict.get socket names)
  in
    H.div []
      [ H.span []
          [ H.text (name')
          ]
      ]

connectionsView : Dict Socket String -> Set Socket -> Html Msg
connectionsView names connections =
  H.div []
    [ H.div []
        [ H.h2 []
            [ H.text "Users Online"
            ]
        ]
    , H.div []
        (List.map (connectionView names) (Set.toList connections))
    ]

postView : Dict Socket String -> (Socket, String) -> Html Msg
postView users (socket, post) =
  let
    name' = Maybe.withDefault "Unknown" (Dict.get socket users)
  in
    H.div []
      [ H.span []
          [ H.text name'
          , H.text ": "
          , H.text post
          ]
      ]

postsView : Dict Socket String -> List (Socket, String) -> Html Msg
postsView users messages =
  H.div []
    [ H.div []
        [ H.h2 []
            [ H.text "Messages"
            ]
        ]
    , H.div []
        (List.map (postView users) messages)
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
    [ connectionsView model.users model.connections
    , postsView model.users model.posts
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
