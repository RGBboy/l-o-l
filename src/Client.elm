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

import ClientChat exposing (Public)



main : Program String Model Msg
main =
  H.programWithFlags
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL

type alias Model =
  { chat: Maybe ClientChat.Model
  , inputPost: String
  , inputName: String
  , inputSecret: String
  , server: String
  , secret: Maybe String
  , time: Time
  }

initModel : String -> Model
initModel server =
  { chat = Nothing
  , inputPost = ""
  , inputName = ""
  , inputSecret = ""
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
  = InputPost String
  | SubmitPost
  | InputName String
  | SubmitName
  | InputSecret String
  | SubmitSecret
  | ServerInit ClientChat.Model
  | ServerMessage ClientChat.InputMsg
  | Tick Time
  | Noop

sendMessage : Model -> ClientChat.OutputMsg -> Cmd msg
sendMessage model message =
  let
    encodedMessage = ClientChat.encodeMessage message
  in
    Maybe.map (\secret -> WebSocket.send (model.server ++ secret) encodedMessage) model.secret
      |> Maybe.withDefault Cmd.none



submitPost : Model -> ClientChat.Model -> (Model, Cmd msg)
submitPost model chat =
  let
    (newChat, message) = ClientChat.post model.inputPost chat
  in
    ( { model
      | inputPost = ""
      , chat = Just newChat
      }
    , sendMessage model message
    )

submitName : Model -> ClientChat.Model -> (Model, Cmd msg)
submitName model chat =
  let
    (newChat, message) = ClientChat.updateName model.inputName chat
  in
    ( { model
      | chat = Just newChat
      }
    , sendMessage model message
    )

update : Msg -> Model -> (Model, Cmd msg)
update message model =
  case message of
    InputPost value ->
      ( { model | inputPost = value }
      , Cmd.none
      )
    SubmitPost ->
      Maybe.map (submitPost model) model.chat
        |> Maybe.withDefault (model, Cmd.none)
    InputName value ->
      ( { model | inputName = value }
      , Cmd.none
      )
    SubmitName ->
      Maybe.map (submitName model) model.chat
        |> Maybe.withDefault (model, Cmd.none)
    InputSecret value ->
      ( { model | inputSecret = value }
      , Cmd.none
      )
    SubmitSecret ->
      ( { model | secret = Just model.inputSecret }
      , Cmd.none
      )
    ServerInit chat ->
      ( { model | chat = Just chat }
      , Cmd.none
      )
    ServerMessage serverMessage ->
      ( { model
        | chat = Maybe.map (ClientChat.update serverMessage) model.chat
        }
      , Cmd.none
      )
    -- Tick time ->
    --   ( { model | time = time }
    --   , Cmd.none
    --   )
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

decodeInit : String -> Msg
decodeInit value =
  Decode.decodeString ClientChat.decodeInit value
    |> Result.map ServerInit
    |> Result.withDefault Noop

decodeMessage : String -> Msg
decodeMessage value =
  Decode.decodeString ClientChat.decodeMessage value
    |> Result.map ServerMessage
    |> Result.withDefault Noop

subscriptions : Model -> Sub Msg
subscriptions model =
  let
    tagger = Maybe.map (always decodeMessage) model.chat
      |> Maybe.withDefault decodeInit
  in
    Maybe.map (\secret -> WebSocket.listen (model.server ++ secret) tagger) model.secret
      |> Maybe.withDefault Sub.none



-- VIEW

postView : Dict Public String -> (Public, String) -> Html Msg
postView users (id, post) =
  let
    name = Maybe.withDefault "Unknown" (Dict.get id users)
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

postsView : ClientChat.Model -> Html Msg
postsView chat =
  let
    posts = ClientChat.posts chat
    users = ClientChat.userNames chat
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

connectedView : Model -> ClientChat.Model -> Html Msg
connectedView model chat =
  H.div
    [ A.class "ba b--light-gray" ]
    [ postsView chat
    , H.div
        [ A.class "w-100 bt b--light-gray" ]
        [ H.input
          [ A.class "w-100 pa2 bw0"
          , A.type_ "text"
          , A.placeholder "Message..."
          , A.value model.inputPost
          , E.onInput InputPost
          , onEnter SubmitPost
          ]
          []
        , H.input
          [ A.class "w-100 pa2 bw0"
          , A.type_ "text"
          , A.placeholder "Name..."
          , A.value model.inputName
          , E.onInput InputName
          , onEnter SubmitName
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
        , A.placeholder "Secret..."
        , A.value model.inputSecret
        , E.onInput InputSecret
        , onEnter SubmitSecret
        ]
        []
    ]

view : Model -> Html Msg
view model =
  let
    subview = Maybe.map (connectedView model) model.chat
      |> Maybe.withDefault (disconnectedView model)
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
