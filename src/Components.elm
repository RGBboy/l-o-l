module Components exposing
  ( box
  , center
  , column
  , hr
  , input
  , inputCenter
  , layout
  , panel
  , posts
  , title
  )

import ClientChat
import Element as El
import Element.Attributes as A
import Element.Events as E
import Html as H exposing (Html)
import Html.Attributes
import Json.Decode as Decode exposing (Decoder)
import Style exposing (StyleSheet)



-- HELPERS

baseUnit : Float
baseUnit = 16

unitHalf : Float
unitHalf = baseUnit / 2

unitQuarter : Float
unitQuarter = baseUnit / 4

unit1 : Float
unit1 = baseUnit

unit2 : Float
unit2 = baseUnit * 2

unit3 : Float
unit3 = baseUnit * 4

unit4 : Float
unit4 = baseUnit * 8

unit5 : Float
unit5 = baseUnit * 16

unit6 : Float
unit6 = baseUnit * 32

midGray : String
midGray = "#555555"

lightGray : String
lightGray = "#EEEEEE"

color : String -> (String, String)
color color = ("color", color)

borderColor : String -> (String, String)
borderColor color = ("border-color", color)

borderTopPin : (String, String)
borderTopPin = ("border-top-width", "1px")

borderWidthPin : (String, String)
borderWidthPin = ("border-width", "1px")

sanSerif : (String, String)
sanSerif = ("font-family", "-apple-system, BlinkMacSystemFont, avenir next, avenir, helvetica neue, helvetica, ubuntu, roboto, noto, segoe ui, arial, sans-serif")

bold : (String, String)
bold = ("font-weight", "bold")

fontSize2 : (String, String)
fontSize2 = ("font-size", "2.25rem")

fontSize6 : (String, String)
fontSize6 = ("font-size", ".875rem")

textAlignCenter : (String, String)
textAlignCenter = ("text-align", "center")

-- COMPONENTS

stylesheet : StyleSheet () variation
stylesheet =
  Style.styleSheet
    [ Style.style () [] ]

layout : El.Element () variation msg -> Html msg
layout child =
  El.layout stylesheet child

center : El.Element () variation msg -> El.Element () variation msg
center child =
  El.row ()
    [ A.inlineStyle [ sanSerif ] -- should probably put this local to the given component
    , A.center
    , A.verticalCenter
    , A.width A.fill
    , A.height A.fill
    ]
    [ El.column ()
        [ A.height A.fill
        , A.maxHeight (unit5|> A.px)
        , A.width A.fill
        , A.maxWidth (unit6 |> A.px)
        , A.inlineStyle
            [ ( "margin", "auto" )
            ]
        ]
        [ child ]
    ]

post : (String, String) -> El.Element () variation msg
post (username, message) =
  El.textLayout ()
    []
    [ El.el ()
        [ A.inlineStyle [ bold, fontSize6, color midGray ] ]
        <| El.text (username ++ ":")
    , El.paragraph ()
        [ A.paddingLeft unitHalf ]
        [ El.text message ]
    ]

posts : String -> List (String, String) -> El.Element () variation msg
posts id messages =
  El.el ()
    [ A.id id
    , A.scrollbars
    ]
    <| El.column ()
      [ A.spacingXY 0 unit1
      , A.clip
      ]
      <| List.map post messages

onEnter : msg -> El.Attribute variation msg
onEnter message =
    E.on "keydown"
      (E.keyCode |> Decode.andThen (is13 message))

is13 : a -> Int -> Decoder a
is13 a code =
  if code == 13 then Decode.succeed a else Decode.fail "not the right key code"

inputHelper : List (String, String) -> msg -> (String -> msg) -> String -> String -> El.Element () variation msg
inputHelper styles onSubmit onChange label value =
  -- Have to work around style elements transform of value -> defaultValue
  -- and the unintended behavior this causes when submitting
  El.node "input"
    <| El.el ()
      [ A.padding unitHalf
      , A.inlineStyle styles
      , E.onInput onChange
      , onEnter onSubmit
      , Html.Attributes.type_ "text" |> A.toAttr
      , Html.Attributes.placeholder (label ++ "...") |> A.toAttr
      , Html.Attributes.value value |> A.toAttr
      ]
      El.empty

inputCenter : msg -> (String -> msg) -> String -> String -> El.Element () variation msg
inputCenter =
  inputHelper [ textAlignCenter ]

input : msg -> (String -> msg) -> String -> String -> El.Element () variation msg
input =
  inputHelper []

hr : El.Element () variation msg
hr =
  El.node "hr"
    <| El.el ()
        [ A.width (100 |> A.percent)
        , A.inlineStyle [ color lightGray, borderTopPin ]
        ]
        El.empty

title : String -> El.Element () variation msg
title value =
  El.h1 ()
    [ A.inlineStyle [ bold, fontSize2, textAlignCenter ]
    ]
    <| El.text value

column : List (El.Element () variation msg) -> El.Element () variation msg
column children =
  El.column ()
    [ A.height A.content
    , A.width A.content
    ]
    children

panel : El.Element () variation msg -> El.Element () variation msg
panel child =
  El.column ()
    [ A.inlineStyle [ borderColor lightGray, borderWidthPin ]
    , A.height A.content
    , A.width A.content
    ]
    [ child ]

box : Float -> String -> El.Element () variation msg
box height color =
  El.el ()
    [ A.height (height |> A.px)
    , A.inlineStyle [ ( "background-color", color ) ]
    ]
    El.empty
