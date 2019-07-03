module Layer.FluidGrid exposing
    ( Model

    )

import Array exposing (Array)
import Random exposing (..)
import Math.Vector2 as Vec2 exposing (vec2, Vec2)

type Ball = Ball Vec2

type alias Model =
    { groups : Array (Array Ball)
    , size : Vec2
    }



