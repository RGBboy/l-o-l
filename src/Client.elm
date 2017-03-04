module Client exposing
  ( Model
  , initModel
  , init
  , Msg( ServerMessage )
  , subscriptions
  )

import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import WebSocket
import WebSocketServer exposing (Socket)

import Set exposing (Set)
import Dict exposing (Dict)
import Result exposing (Result)
import Time exposing (Time)

import Json.Decode as Decode exposing (Decoder)

import ClientChat as Chat



main : Program String Model Msg
main =
  H.programWithFlags
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
  { chat: Chat.Model
  , input: String
  , name: String
  , status: Status
  , server: String
  , secret: Maybe String
  , time: Time
  }

initModel : String -> Model
initModel server =
  { chat = Chat.init server
  , input = ""
  , name = ""
  , status = Disconnected
  , server = server
  , secret = Nothing
  , time = 0
  }

init : String -> (Model, Cmd Msg)
init server =
  ( initModel server
  , Cmd.none
  )



-- UPDATE

type Msg
  = InputMessage String
  | Send
  | InputName String
  | Connect
  | Disconnect
  | ServerMessage String
  | Tick Time
  | Noop

update : Msg -> Model -> (Model, Cmd Msg)
update message model =
  case message of
    InputMessage value ->
      ( { model | input = value }
      , Cmd.none
      )
    Send ->
      let
        (chat, cmd) = (Chat.update (Chat.ClientPost model.input) model.chat)
      in
        ( { model
          | input = ""
          , chat = chat
          }
        , cmd
        )
    InputName value ->
      ( { model | name = value }
      , Cmd.none
      )
    Connect ->
      let
        (chat, cmd) = (Chat.update (Chat.ClientJoin model.name) model.chat)
      in
        ( { model
          | chat = chat
          , name = ""
          , status = Connected
          }
        , cmd
        )
    Disconnect ->
      ( { model
        | status = Disconnected
        }
      , Cmd.none
      )
    -- Message message ->
    --   let
    --     (chat, cmd) = Chat.update message model.chat
    --   in
    --     ( { model | chat = chat }
    --     , cmd
    --     )
    Tick time ->
      ( { model | time = time }
      , Cmd.none
      )
    _ -> (model, Cmd.none)



-- SUBSCRIPTIONS

-- subscriptions : Model -> Sub Msg
-- subscriptions model =
--   if model.status == Connected then
--     Sub.batch
--       [ Sub.map Message (Chat.listen model.chat)
--       , Time.every (100 * Time.millisecond) Tick
--       ]
--   else
--     Time.every (100 * Time.millisecond) Tick

subscriptions : Model -> Sub Msg
subscriptions model =
  Maybe.map (\secret -> WebSocket.listen (model.server ++ "/" ++ secret) ServerMessage) model.secret
    |> Maybe.withDefault Sub.none



-- VIEW

connectionView : Dict Socket String -> Socket -> Html Msg
connectionView names socket =
  let
    name = Maybe.withDefault "Unknown" (Dict.get socket names)
  in
    H.div []
      [ H.span []
          [ H.text name
          ]
      ]

connectionsView : Chat.Model -> Html Msg
connectionsView chat =
  let
    users = Chat.users chat
    connections = Chat.connections chat
  in
    H.div [] (List.map (connectionView users) (Set.toList connections))

postView : Dict Socket String -> (Socket, String) -> Html Msg
postView users (socket, post) =
  let
    name = Maybe.withDefault "Unknown" (Dict.get socket users)
  in
    H.div []
      [ H.span
          [ A.class "db f6 b mt2 mb1 mid-gray"
          , A.style [ ("word-break", "break-all") ] -- tachyons has a bug, no word-break for now
          ]
          [ H.text name
          , H.text ":"
          ]
      , H.span
          [ A.class "db pl2"
          , A.style [ ("word-break", "break-all") ] -- tachyons has a bug, no word-break for now
          ]
          [ H.text post ]
      ]

postsView : Chat.Model -> Html Msg
postsView chat =
  let
    posts = Chat.posts chat
    users = Chat.users chat
  in
    H.div
      [ A.class "flex h4 pa2 overflow-container"
      , A.style [ ("flex-direction", "column-reverse") ] -- tachyons doesn't have flex reverse
      ]
      (List.map (postView users) posts)

onEnter : Msg -> H.Attribute Msg
onEnter message =
    E.on "keydown"
      (E.keyCode |> Decode.andThen (is13 message))

is13 : a -> Int -> Decoder a
is13 a code =
  if code == 13 then Decode.succeed a else Decode.fail "not the right key code"

connectedView : Model -> Html Msg
connectedView model =
  H.div
    [ A.class "ba b--light-gray" ]
    [ postsView model.chat
    , H.div
        [ A.class "w-100 bt b--light-gray" ]
        [ H.input
          [ A.class "w-100 pa2 bw0"
          , A.type_ "text"
          , A.placeholder "Message..."
          , A.value model.input
          , E.onInput InputMessage
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
        , A.type_ "text"
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
              [ H.text title ]
          , subview
          ]
      ]
