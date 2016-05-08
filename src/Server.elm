port module Server exposing (..)

import Html exposing (Html)
import Html.App as App

import Json.Decode as Decode
import Json.Encode as Encode

import Debug

main : Program Never
main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- View

-- hack for server program to work
view : Model -> Html Msg
view = always (Html.text "")

-- Model

type alias Model = List String

init : (Model, Cmd Msg)
init =
  ([], Cmd.none)

-- Update

type Msg
  = Echo String

port output : String -> Cmd msg

update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    Echo message ->
      ( (message :: model), output message )

-- Input port of messages from clients
port input : (String -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions model =
  input Echo
