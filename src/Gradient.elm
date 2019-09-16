module Gradient exposing
    ( Gradient
    , Stops
    , Stop
    , Color
    , Orientation(..)
    , encode
    , decode
    , decodeOrientation
    , none
    )


import Json.Encode as E exposing (..)
import Json.Decode as D exposing (..)


type alias Color = String


type Orientation
    = Horizontal
    | Vertical
    | Radial


type alias Gradient =
    { stops: Stops
    , orientation: Orientation
    }


type alias Stop =
    ( Float, Color )


type alias Stops = List Stop


none : Gradient
none =
    { stops = []
    , orientation = Horizontal
    }


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
                    Radial -> E.string "radial"
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
                , orientation = decodeOrientation orientationStr
                }
            )
            (D.field "stops" <| D.list makeGradientStop)
            (D.field "orientation" <| D.string)


decodeOrientation : String -> Orientation
decodeOrientation orientationStr =
    case orientationStr of
        "horizontal" -> Horizontal
        "vertical" -> Vertical
        "radial" -> Radial
        _ -> Vertical