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
import Effects exposing (Effects)
import Task exposing (Task)

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
        , onEnter address Submit
        ]
        []
    ]

type ClientAction
  = Input String
  | Submit
  | Network Action
  | Noop

type alias Model =
  { input: String
  , network: Action.Model
  }

init : (Model, Effects ClientAction)
init =
  ( { input = ""
    , network = Action.init
    }
  , Effects.none
  )

--SubmitPost : Task Never a -> Effects a

updateModel : ClientAction -> Model -> (Model, Effects ClientAction)
updateModel clientAction model =
  case clientAction of
    Input value ->
      ( { model | input = value }
      , Effects.none
      )
    Submit ->
      ( { model | input = "" }
      , Effects.task (Task.succeed (Network (Action.Post model.input)))
      )
    Network action ->
      ( { model | network = Action.update action model.network }
      , Effects.none
      )
    _ -> (model, Effects.none)

-- Input port of actions from a client
-- Need to set this up to use a JSON decoder to translate to Action types
port input : Signal Decode.Value

networkActionInput : Signal (List ClientAction)
networkActionInput =
  Signal.map (Action.decode >> (Debug.log "action input") >> (List.map Network) ) input

-- mailbox : Signal.Mailbox ClientAction
-- mailbox =
-- -- ideally we should be able to have no value to start (can we with ()?)
--   Signal.mailbox Noop

toActions : List ClientAction -> List Action
toActions actions =
  case actions of
    hd :: tl ->
      case hd of
        Network action -> action :: toActions tl
        _ -> toActions tl
    [] -> []

isNotNetworkAction : ClientAction -> Bool
isNotNetworkAction action =
  case action of
    Network action -> False
    _ -> True

singleton : ClientAction -> (List ClientAction)
singleton action = [ action ]

messages : Signal.Mailbox (List ClientAction)
messages =
  Signal.mailbox []

address : Signal.Address ClientAction
address =
  Signal.forwardTo messages.address singleton

updateStep : ClientAction -> (Model, Effects ClientAction) -> (Model, Effects ClientAction)
updateStep action (oldModel, accumulatedEffects) =
  let
    (newModel, additionalEffects) = updateModel action oldModel
  in
    (newModel, Effects.batch [accumulatedEffects, additionalEffects])

update : List ClientAction -> (Model, Effects ClientAction) -> (Model, Effects ClientAction)
update actions (model, _) =
  List.foldl updateStep (model, Effects.none) actions

inputs : Signal (List ClientAction)
inputs =
  Signal.mergeMany (messages.signal :: [networkActionInput])

effectsAndModel : Signal (Model, Effects ClientAction)
effectsAndModel =
  Signal.foldp update init inputs
  -- Signal.foldp update init (Signal.merge (Signal.filter isNotNetworkAction Noop mailbox.signal) networkActionInput)

model : Signal Model
model =
  Signal.map fst effectsAndModel

networkActionOutput : Signal (List Action)
networkActionOutput =
  Signal.filter (not << List.isEmpty) [] (Signal.map toActions messages.signal)

-- Output port of actions to a client
port output : Signal Encode.Value
port output =
  Signal.map Action.encode networkActionOutput

port tasks : Signal (Task Effects.Never ())
port tasks =
  Signal.map (Effects.toTask messages.address << snd) effectsAndModel

main : Signal Html
main = Signal.map (view (Signal.forwardTo messages.address singleton)) model
