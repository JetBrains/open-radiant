module Layer.Background exposing
    ( Model
    , Mode(..)
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
import Svg.Attributes as S exposing (style)

import Viewport exposing (Viewport)

import Gradient as G exposing (..)


type alias Color = String


type Mode
    = Fill Color
    | Gradient G.Gradient


type alias Model =
    { mode : Mode
    , opacity : Float
    }


defaultMode : Mode
defaultMode = Fill "#171717"


init : Model
init =
    { mode = defaultMode
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
    svg
        [ width  <| String.fromInt w
        , height <| String.fromInt h
        ]
        (case mode of
            Fill color ->
                [ rect
                    [ x "0"
                    , y "0"
                    , width <| String.fromInt w
                    , height <| String.fromInt h
                    , fill color
                    ]
                    [ ]
                ]
            Gradient gradient ->
                [ defs
                    [ ]
                    [ linearGradient
                        [ id "x-gradient-background"
                        , x1 "0%"
                        , y1 "0%"
                        , x2 (if gradient.orientation == G.Horizontal then "100%" else "0%")
                        , y2 (if gradient.orientation == G.Horizontal then "0%" else "100%")
                        ]
                        (gradient.stops
                            |> List.map
                                (\( sOffset, sColor ) ->
                                    stop
                                        [ offset <| String.fromFloat sOffset
                                        , stopColor sColor
                                        ]
                                        [ ]
                                )
                        )
                    ]
                , rect
                    [ x "0"
                    , y "0"
                    , width <| String.fromInt w
                    , height <| String.fromInt h
                    , fill "url(#x-gradient-background)"
                    ]
                    [ ]
                ]
        )


encode : Model -> E.Value
encode model =
    let
        encodeMode =
            case model.mode of
                Fill _ -> E.string "fill"
                Gradient _ -> E.string "gradient"
        encodeValue =
            case model.mode of
                Fill color ->
                    E.object
                        [ ( "color", E.string color ) ]
                Gradient gradient ->
                    E.object
                        [ ( "gradient", G.encode gradient ) ]
    in
        E.object
            [ ( "opacity", E.float model.opacity )
            , ( "mode", encodeMode )
            , ( "value", encodeValue )
            ]


decode : D.Decoder Model
decode =
    let
        makeValue
            = D.map2
                (\maybeColor maybeGradient ->
                    case ( maybeColor, maybeGradient ) of
                        ( Just color, _ ) -> Fill color
                        ( _, Just gradient ) -> Gradient gradient
                        ( Nothing, Nothing ) -> defaultMode
                )
                (D.maybe <| D.field "color" D.string)
                (D.maybe <| D.field "gradient" G.decode)
    in
        D.map3
            (\opacity mode modeValue ->
                { opacity = opacity
                , mode = modeValue -- TODO: ensure mode string corresponds to value?
                }
            )
            (D.field "opacity" D.float)
            (D.field "mode" D.string)
            (D.field "value" makeValue)
