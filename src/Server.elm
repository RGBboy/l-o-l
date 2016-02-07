module Server where

import Signal exposing (Signal)
import Action exposing (Action)

import Json.Decode as Decode
import Json.Encode as Encode

import Debug

-- Input port of actions from a client
-- Need to set this up to use a JSON decoder to translate to Action types
port input : Signal Decode.Value

actionSignal : Signal Action
actionSignal =
  Signal.map Action.decode input

-- need a way to remove actions from this list as they are sent
-- need a way to limit actions to a given time and make up missing ones
-- store : Game.Action -> List Game.Action -> List Game.Action
-- store action actions =
  -- action :: actions

isConnect : Action -> Bool
isConnect action =
  case action of
    Action.Connect id -> False
    _ -> True

connectToInit : Action.Model -> Action -> Action
connectToInit model action =
  case action of
    Action.Connect id -> Action.Init id model
    a -> a

modelSignal : Signal Action.Model
modelSignal =
  Signal.foldp Action.update Action.init actionSignal

noState : Action
noState = (Action.Init "" Action.init)

initSignal : Signal Action
initSignal = Signal.filter isConnect noState (Signal.map2 connectToInit modelSignal actionSignal)


-- Output port of actions to a client
-- Need to set this up to use a JSON encoder to translate to Action types
port outputOne : Signal Encode.Value
port outputOne = Signal.map Action.encode initSignal

port outputAll : Signal Encode.Value
port outputAll = Signal.map Action.encode actionSignal
