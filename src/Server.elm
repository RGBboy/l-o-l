module Server where

import Signal exposing (Signal)
import Action exposing (Action)

import Json.Decode as Decode
import Json.Encode as Encode

import Debug

-- Input port of actions from a client
-- Need to set this up to use a JSON decoder to translate to Action types
port input : Signal Decode.Value

actionsSignal : Signal (List Action)
actionsSignal =
  Signal.map Action.decode input

-- need a way to remove actions from this list as they are sent
-- need a way to limit actions to a given time and make up missing ones
-- store : Game.Action -> List Game.Action -> List Game.Action
-- store action actions =
  -- action :: actions

isConnect : Action -> Bool
isConnect action =
  case action of
    Action.Connect id -> True
    _ -> False

connectToInit : Action.Model -> Action -> Action
connectToInit model action =
  case action of
    Action.Connect id -> Action.Init id model
    a -> a

connectToInits : Action.Model -> (List Action) -> (List Action)
connectToInits model =
  List.map (connectToInit model)

update : (List Action) -> Action.Model -> Action.Model
update actions model =
  List.foldl Action.update model actions

modelSignal : Signal Action.Model
modelSignal =
  Signal.foldp update Action.init actionsSignal

connectsSignal : Signal (List Action)
connectsSignal =
  Signal.map (List.filter isConnect) actionsSignal

initSignal : Signal (List Action)
initSignal = Signal.map2 connectToInits modelSignal connectsSignal

-- Output port of actions to a client
-- Need to set this up to use a JSON encoder to translate to Action types
port outputOne : Signal Encode.Value
port outputOne = Signal.map Action.encode initSignal

port outputAll : Signal Encode.Value
port outputAll = Signal.map Action.encode actionsSignal
