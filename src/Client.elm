module Client exposing
  ( Model
  , initModel
  , init
  , Msg( ServerMessage )
  , subscriptions
  , decodeInit
  , decodeMessage
  )

import Components as C
import Element as El
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import WebSocket

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

connectedView : String -> String -> List (String, String) -> El.Element () variation Msg
connectedView inputPost inputName posts =
  C.panel
    [ C.posts posts
    , C.hr
    , C.input SubmitPost InputPost "Message" inputPost
    , C.hr
    , C.input SubmitName InputName "Name" inputName
    ]

disconnectedView : String -> El.Element () variation Msg
disconnectedView inputSecret =
  C.panel
    [ C.inputCenter SubmitSecret InputSecret "Secret" inputSecret
    ]

collectUsersPosts : Dict Public String -> (Public, String) -> List (String, String) -> List (String, String)
collectUsersPosts users (id, post) acc =
  let
    name = Maybe.withDefault "Unknown" (Dict.get id users)
  in
    (name, post) :: acc

usersPosts : ClientChat.Model -> List (String, String)
usersPosts chat =
  let
    posts = ClientChat.posts chat
    users = ClientChat.userNames chat
  in
    List.foldr (collectUsersPosts users) [] posts

view : Model -> Html Msg
view model =
  let
    posts = Maybe.map usersPosts model.chat
    content = Maybe.map (connectedView model.inputPost model.inputName) posts
      |> Maybe.withDefault (disconnectedView model.inputSecret)
    frame = ((Time.inMilliseconds model.time) / 100 |> round |> (%)) 50
    title =
      case frame of
        3 -> "l-•-l"
        2 -> "l-o-l"
        1 -> "l-•-l"
        _ -> "l-o-l"
  in
    C.layout
      [ C.title title
      , content
      ]
