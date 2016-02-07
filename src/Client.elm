module Client where

import Signal exposing (Signal)
import Action exposing (Action)
import Json.Decode as Decode
import Json.Encode as Encode
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E exposing (onClick)
import Debug
import Set exposing (Set)

connectionView : Action.Id -> Html
connectionView id =
  H.div []
    [ H.span []
        [ H.text ("Connection " ++ id)
        ]
    ]

connectionsView : Action.Model -> Html
connectionsView model =
  let
    connections = List.map connectionView (Set.toList model.connections)
  in
    H.div [] connections

messageView : Action.Message -> Html
messageView message =
  H.div []
    [ H.span []
        [ H.text message
        ]
    ]

messagesView : Action.Model -> Html
messagesView model =
  let
    messages = List.map messageView model.messages
  in
    H.div [] messages

onEnter : Signal.Address a -> a -> H.Attribute
onEnter address value =
    E.on "keydown"
      (Decode.customDecoder E.keyCode is13)
      (\_ -> Signal.message address value)

is13 : Int -> Result String ()
is13 code =
  if code == 13 then Ok () else Err "not the right key code"

view : Signal.Address ClientAction -> Model -> Html
view address model =
  H.div []
    [ H.h1 []
        [ H.text "Network Experiment"
        ]
    , connectionsView model.network
    , messagesView model.network
    , H.input
        [ A.placeholder "Message..."
        , A.value model.input
        , E.on "input" E.targetValue (Signal.message address << Input)
        , onEnter address (Network (Action.Post model.input))
        ]
        []
    ]

type ClientAction
  = Input String
  | Network Action
  | Noop

type alias Model
  = { input: String
    , network: Action.Model
    }

init : Model
init
  = { input = ""
    , network = Action.init
    }

update : ClientAction -> Model -> Model
update clientAction model =
  case clientAction of
    Input value -> { model | input = value }
    Network action -> { model | network = Action.update action model.network }
    _ -> model

-- Input port of actions from a client
-- Need to set this up to use a JSON decoder to translate to Action types
port input : Signal Decode.Value

networkActionInput : Signal ClientAction
networkActionInput =
  Signal.map (Action.decode >> (Debug.log "action input") >> Network) input

mailbox : Signal.Mailbox ClientAction
mailbox =
-- ideally we should be able to have no value to start (can we with ()?)
  Signal.mailbox Noop

toAction : ClientAction -> Maybe Action
toAction action =
  case action of
    Network action -> Just action
    _ -> Nothing

-- Output port of actions to a client
port output : Signal Encode.Value
port output = Signal.map Action.encode (Signal.filterMap toAction (Action.Error "Init") mailbox.signal)

isNotNetworkAction : ClientAction -> Bool
isNotNetworkAction action =
  case action of
    Network action -> False
    _ -> True

modelSignal : Signal Model
modelSignal =
  Signal.foldp update init (Signal.merge (Signal.filter isNotNetworkAction Noop mailbox.signal) networkActionInput)

main : Signal Html
main = Signal.map (view mailbox.address) modelSignal
