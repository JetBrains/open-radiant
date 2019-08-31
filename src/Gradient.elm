module Gradient exposing
    ( Gradient
    , Stops
    , Stop
    , Color
    , Orientation(..)
    , encode
    , decode
    )


import Json.Encode as E exposing (..)
import Json.Decode as D exposing (..)


type alias Color = String


type Orientation
    = Horizontal
    | Vertical


type alias Gradient =
    { stops: Stops
    , orientation: Orientation
    }


type alias Stop =
    ( Float, Color )


type alias Stops = List Stop


encode : Gradient -> E.Value
encode { orientation, stops } =
    let
        encodeStop ( stopPos, stopColor )  =
            E.object
                [ ( "pos", E.float stopPos )
                , ( "color", E.string stopColor )
                ]
    in
        E.object
            [ ( "stops", E.list encodeStop stops )
            , ( "orientation",
                case orientation of
                    Horizontal -> E.string "horizontal"
                    Vertical -> E.string "vertical"
                )
            ]


decode : D.Decoder Gradient
decode =
    let
        makeGradientStop =
            D.map2
                Tuple.pair
                (D.field "pos" D.float)
                (D.field "color" D.string)
    in
        D.map2
            (\stops orientationStr ->
                { stops = stops
                , orientation =
                    case orientationStr of
                        "horizontal" -> Horizontal
                        "vertical" -> Vertical
                        _ -> Vertical
                }
            )
            (D.field "stops" <| D.list makeGradientStop)
            (D.field "orientation" <| D.string)
