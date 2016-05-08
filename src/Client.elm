import Html as H exposing (Html)
import Html.App as Html
import Html.Attributes as A
import Html.Events as E
import Json.Decode as Json
import WebSocket



main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


echoServer : String
echoServer =
  "ws://localhost:8080"



-- MODEL

type alias User = String

type alias Model =
  { input: String
  , messages: List String
  , connections: List String
  }

init : (Model, Cmd Msg)
init =
  ( { input = ""
    , messages = []
    , connections = []
    }
  , Cmd.none
  )

-- UPDATE


type Msg
  = Input String
  | Send
  | NewMessage String


update : Msg -> Model -> (Model, Cmd Msg)
update message model =
  case message of
    Input value ->
      ( { model | input = value }
      , Cmd.none
      )
    Send ->
      ( { model | input = "" }
      , WebSocket.send echoServer model.input
      )
    NewMessage message ->
      ( { model | messages = message :: model.messages }
      , Cmd.none
      )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  WebSocket.listen echoServer NewMessage



-- VIEW

connectionView : String -> Html Msg
connectionView id =
  H.div []
    [ H.span []
        [ H.text ("Connection " ++ id)
        ]
    ]

connectionsView : List String -> Html Msg
connectionsView connections =
  H.div [] (List.map connectionView connections)

messageView : String -> Html Msg
messageView message =
  H.div []
    [ H.span []
        [ H.text message
        ]
    ]

messagesView : List String -> Html Msg
messagesView messages =
  H.div [] (List.map messageView messages)

onEnter : Msg -> H.Attribute Msg
onEnter message =
    E.on "keydown"
      (Json.map
        (always message)
        (Json.customDecoder E.keyCode is13)
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
