import Html as H exposing (Html)
import Html.App as Html
import Html.Attributes as A
import Html.Events as E
import WebSocket
import WebSocketServer exposing (Socket)

import Set exposing (Set)
import Dict exposing (Dict)
import Result exposing (Result)
import Time exposing (Time)

import Json.Decode as Decode exposing (Decoder, (:=))
import Json.Encode as Encode

import Chat



main =
  Html.programWithFlags
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL

type Status
  = Disconnected
  | Connected

type alias Model =
  { server: String
  , chat: Maybe Chat.Model
  , input: String
  , name: String
  , status: Status
  , time: Time
  }

init : String -> (Model, Cmd Msg)
init server =
  ( { server = server
    , chat = Nothing
    , input = ""
    , name = ""
    , status = Disconnected
    , time = 0
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
  | Tick Time
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
          ,  WebSocket.send model.server (encodeValue "Post" model.input)
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
      , WebSocket.send model.server (encodeValue "Join" model.name)
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
    Tick time ->
      ( { model | time = time }
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
    Sub.batch
      [ Sub.map Message (WebSocket.listen model.server Chat.decodeMessage)
      , Time.every (100 * Time.millisecond) Tick
      ]
  else
    Time.every (100 * Time.millisecond) Tick



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
    H.div [] (List.map (connectionView chat.users) (Set.toList chat.connections))

postView : Dict Socket String -> (Socket, String) -> Html Msg
postView users (socket, post) =
  let
    name' = Maybe.withDefault "Unknown" (Dict.get socket users)
  in
    H.div []
      [ H.span
          [ A.class "db f6 b mt2 mb1 mid-gray" ]
          [ H.text name'
          , H.text ":"
          ]
      , H.span
          [ A.class "db pl2"
          , A.style [ ("word-break", "break-all") ] -- tachyons doesn't have flex reverse
          ]
          [ H.text post ]
      ]

postsView : Chat.Model -> Html Msg
postsView chat =
  let
    posts = List.append chat.optimisticPosts chat.posts
  in
    H.div
      [ A.class "flex h4 pa2 overflow-container"
      , A.style [ ("flex-direction", "column-reverse") ] -- tachyons doesn't have flex reverse
      ]
      (List.map (postView chat.users) posts)

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
  H.div
    [ A.class "ba b--light-gray" ]
    [ Maybe.withDefault
        (H.div
          [ A.class "h4 pa2 overflow-container" ]
          []
        )
        (Maybe.map postsView model.chat)
    , H.div
        [ A.class "w-100 bt b--light-gray" ]
        [ H.input
          [ A.class "w-100 pa2 bw0"
          , A.type' "text"
          , A.placeholder "Message..."
          , A.value model.input
          , E.onInput Input
          , onEnter Send
          ]
          []
        ]
    ]

disconnectedView : Model -> Html Msg
disconnectedView model =
  H.div
    [ A.class "w-50 center" ]
    [ H.input
        [ A.class "w-100 pa2 tc"
        , A.type' "text"
        , A.placeholder "Name..."
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
    frame = ((Time.inMilliseconds model.time) / 100 |> round |> (%)) 50
    title =
      case frame of
        3 -> "l-•-l"
        2 -> "l-o-l"
        1 -> "l-•-l"
        _ -> "l-o-l"
  in
    H.div
      [ A.class "flex flex-column justify-center h-100 mw6 center" ]
      [ H.div
          [ A.class "flex flex-column h5 " ]
          [ H.h1
              [ A.class "f2 tc mb2" ]
              [ H.text "l-o-l" ]
          , subview
          ]
      ]
