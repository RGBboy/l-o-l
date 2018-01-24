module Components exposing
  ( box
  , column
  , hr
  , input
  , inputCenter
  , layout
  , panel
  , posts
  , title
  )

import Color exposing (Color)
import Element exposing (Element)
import Element.Area as Area
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Json.Decode as Decode exposing (Decoder)



-- HELPERS

baseUnit : Int
baseUnit = 16

unitHalf : Int
unitHalf = baseUnit // 2

unitQuarter : Int
unitQuarter = baseUnit // 4

unit1 : Int
unit1 = baseUnit

unit2 : Int
unit2 = baseUnit * 2

unit3 : Int
unit3 = baseUnit * 4

unit4 : Int
unit4 = baseUnit * 8

unit5 : Int
unit5 = baseUnit * 16

unit6 : Int
unit6 = baseUnit * 32

borderWidthPin : Int
borderWidthPin = 1

midGray : Color
midGray = Color.rgb 85 85 85 -- #555555

lightGray : Color
lightGray = Color.rgb 238 238 238 -- #EEEEEE

fontSize2 : Int
fontSize2 = 36 -- 2.25rem

fontSize6 : Int
fontSize6 = 14 -- .875rem

textAlignCenter : (String, String)
textAlignCenter = ("text-align", "center")

inlineStyle : List (String, String) -> Element.Attribute msg
inlineStyle styles =
  A.style styles |> Element.attribute



-- COMPONENTS

layout : List (Element msg) -> Html msg
layout children =
  Element.layout
    [ Background.color Color.white
    , Font.color midGray
    , inlineStyle
        [ ("height", "100%") ]-- needed to cause resizing on smaller screens
    ]
    <|
      Element.el
        [ Element.center
        , Element.centerY
        , inlineStyle
            [ ("height", "100%") -- needed to cause resizing on smaller screens
            , ("max-height", "320px")
            , ("width", "100%")
            , ("max-width", "480px")
            ]
        ]
    <|
      Element.column
        [ Element.spacingXY 0 16
        -- , Element.center
        , inlineStyle
            [ ("height", "100%") -- needed to cause resizing on smaller screens
            , ("width", "100%")
            ]
        ]
        children

post : (String, String) -> Element msg
post (username, message) =
  Element.column
    [ Element.spacingXY 0 unitQuarter
    , Element.height Element.shrink
    , Element.alignBottom
    ]
    [ Element.el
        [ Element.width Element.fill
        , Font.bold
        , Font.size fontSize6
        , Element.alignLeft
        ]
        <| Element.text (username ++ ":")
    , Element.paragraph
        [ Element.paddingEach { bottom = 0, left = unitHalf, right = 0, top = 0 }
        , Font.size fontSize6
        ]
        [ Element.text message ]
    ]

posts : List (String, String) -> Element msg
posts messages =
  Element.column
    [ Element.scrollbarY
    , Element.width Element.fill
    , Element.height Element.fill -- ???
    , Element.alignBottom -- ???
    , inlineStyle
        [ ("flex-direction", "column-reverse")
        , ("flex", "100000 1 auto") -- causes element to only fill remaining space
        ]
    ]
    [ Element.column
        [ Element.spacing unit1
        , Element.height Element.fill
        ]
        <| List.map post messages
    ]

onEnter : msg -> Html.Attribute msg
onEnter message =
  E.on "keydown"
    (E.keyCode |> Decode.andThen (is13 message))

is13 : a -> Int -> Decoder a
is13 a code =
  if code == 13 then Decode.succeed a else Decode.fail "not the right key code"

inputHelper : List (String, String) -> msg -> (String -> msg) -> String -> String -> Element msg
inputHelper styleList onSubmit onChange label value =
  let
    styles = List.append styleList
      [ ("width", "100%")
      , ("height", "100%")
      , ("padding", (toString unitHalf) ++ "px")
      , ("box-sizing", "border-box")
      , ("border", "none")
      , ("font-size", "inherit")
      ]
  in
    -- Have to work around style elements transform of value -> defaultValue
    -- and the unintended behavior this causes when submitting
    Element.el
      [ Font.size fontSize6
      , Element.width Element.fill
      , Element.alignBottom
      ]
      <| Element.html
          <| Html.input
            [ A.style styles
            , E.onInput onChange
            , onEnter onSubmit
            , A.type_ "text"
            , A.placeholder (label ++ "...")
            , A.value value
            ]
            []

inputCenter : msg -> (String -> msg) -> String -> String -> Element msg
inputCenter =
  inputHelper [ textAlignCenter ]

input : msg -> (String -> msg) -> String -> String -> Element msg
input =
  inputHelper []

hr : Element msg
hr =
  Element.el -- how can we create semantic elements like hr?
    [ Element.width Element.fill
    , Font.color lightGray
    , Border.widthEach { bottom = 0, left = 0, right = 0, top = borderWidthPin }
    , Element.alignBottom
    ]
    Element.empty

title : String -> Element msg
title value =
  Element.el
    [ Area.heading 1
    , Font.bold
    , Font.size fontSize2
    , Font.center
    ]
    <| Element.text value

column : List (Element msg) -> Element msg
column children =
  Element.column
    [ Element.width Element.fill
    , inlineStyle
        [ ("flex", "100000 1 auto")
        , ("height", "100%") -- needed to cause resizing on smaller screens
        ]
    ]
    children

panel : Element msg -> Element msg
panel child =
  Element.el
    [ Border.color lightGray
    , Border.width borderWidthPin
    , Element.width Element.fill
    , Element.height Element.fill
    , Element.alignBottom
    , inlineStyle
        [ ("flex", "100000 1 auto")
        , ("height", "100%") -- needed to cause resizing on smaller screens
        ]
    ]
    child

box height color =
  Element.el
    [ Element.height (height |> Element.px)
    , Element.width Element.fill
    , Background.color color
    , Border.color Color.black
    ]
    Element.empty
