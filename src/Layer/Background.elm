module Layer.Background exposing
    ( Model
    , init
    , view
    , encode
    , decode
    )


import Html exposing (Html, div)
import Html.Attributes  as H exposing (class)

import Json.Encode as E exposing (..)
import Json.Decode as D exposing (..)

import Math.Vector2 as Vec2 exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)

import Viewport exposing (Viewport)


type alias Color = String


type Mode
    = Fill Color
    | VerticalGradient { top : Color, bottom : Color }
    | HorizontalGradient { left : Color, right : Color }


type alias Model =
    { mode : Mode
    , opacity : Float
    }


init : Model
init =
    { mode = Fill "#171717"
    , opacity = 1.0
    }


view : Viewport {} -> Model -> Html msg
view viewport model =
    let
        ( w, h ) =
            ( Vec2.getX viewport.size
            , Vec2.getY viewport.size
            )
    in
        div [ H.class "background-layer layer" ]
            [ renderBackground ( floor w, floor h ) model.mode model.opacity
            ]


renderBackground : ( Int, Int ) ->  Mode -> Float -> Svg msg
renderBackground ( w, h ) mode opacity =
    case mode of
        Fill color ->
            svg
                [ width  <| String.fromInt w
                , height <| String.fromInt h
                ]
                [ rect
                    [ x "0"
                    , y "0"
                    , width <| String.fromInt w
                    , height <| String.fromInt h
                    , fill color
                    ]
                    []
                ]
        _ -> svg [] []


encode : Model -> E.Value
encode _ = E.object []


decode : D.Decoder Model
decode = D.succeed init
