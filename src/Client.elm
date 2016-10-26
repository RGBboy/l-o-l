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
import Json.Encode as Encode

import Chat



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

type Status
  = Disconnected
  | Connected

type alias Model =
  { chat: Maybe Chat.Model
  , input: String
  , name: String
  , status: Status
  }

init : (Model, Cmd Msg)
init =
  ( { chat = Nothing
    , input = ""
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
  | Message Chat.Msg
  | Noop

update : Msg -> Model -> (Model, Cmd Msg)
update message model =
  case message of
    Input value ->
      ( { model | input = value }
      , Cmd.none
      )
    Send ->
      case model.chat of
        Nothing -> (model, Cmd.none)
        Just chat ->
          ( { model
            | input = ""
            , chat = Just (Chat.update (Chat.OptimisticPost chat.socket model.input) chat)
            }
          ,  WebSocket.send server (encodeValue "Post" model.input)
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
      , WebSocket.send server (encodeValue "Join" model.name)
      )
    Disconnect ->
      ( { model
        | status = Disconnected
        , chat = Nothing
        }
      , Cmd.none
      )
    Message message ->
      case model.chat of
        Nothing ->
          ( { model | chat = Just (Chat.update message Chat.init) }
          , Cmd.none
          )
        Just chat ->
          ( { model | chat = Just (Chat.update message chat) }
          , Cmd.none
          )
    _ -> (model, Cmd.none)



-- SUBSCRIPTIONS

encodeValue : String -> String -> String
encodeValue kind value =
  Encode.object
    [ ("type", Encode.string kind)
    , ("value", Encode.string value)
    ]
  |> Encode.encode 2

subscriptions : Model -> Sub Msg
subscriptions model =
  if model.status == Connected then
    Sub.map Message (WebSocket.listen server Chat.decodeMessage)
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

connectionsView : Chat.Model -> Html Msg
connectionsView chat =
  H.div []
    [ H.div []
        [ H.h2 []
            [ H.text "Users Online"
            ]
        ]
    , H.div []
        (List.map (connectionView chat.users) (Set.toList chat.connections))
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

postsView : Chat.Model -> Html Msg
postsView chat =
  let
    posts = List.append chat.optimisticPosts chat.posts
  in
    H.div []
      [ H.div []
          [ H.h2 []
              [ H.text "Messages"
              ]
          ]
      , H.div []
          (List.map (postView chat.users) posts)
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

maybeToList : Maybe a -> List a
maybeToList maybe =
  case maybe of
    Just a -> [a]
    Nothing -> []

connectedView : Model -> Html Msg
connectedView model =
  H.div [] <|
  List.append (maybeToList (Maybe.map postsView model.chat)) <|
  List.append (maybeToList (Maybe.map connectionsView model.chat)) <|
  [ H.input
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
