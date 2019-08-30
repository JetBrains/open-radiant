module Layer.NativeMetaballs exposing
    ( Model
    -- , PortModel
    , init
    -- , export
    , view
    , generate
    , generator, initialStateGenerator
    , Variety
    , Orbit
    )


import Html as H
import Html.Attributes as H

import Math.Vector2 as Vec2 exposing (..)

import Model.Product as Product
import Model.Product exposing (ColorId(..))
import Model.Range exposing (..)
import Layer.Fluid as Fluid exposing
    ( Model
    , generate, generator
    , Variety(..), Orbit(..), Ranges
    )

import Random
import Random.Extra as Random exposing (traverse)


defaultColors = [ "#f38038", "#ed3d7d", "#341f49" ]


type alias Model = Fluid.Model


type alias Variety = Fluid.Variety
type alias Orbit = Fluid.Orbit


-- type alias PortModel =
--     { palette: Product.Palette
--     , temp : String
--     }


init : Model
init =
    let fluidDefault = Fluid.init
    in
        { fluidDefault
        | variety = Variety 0.2
        , orbit = Orbit 0.25
        }



-- export : Model -> PortModel
-- export model =
--     { palette = model.colors
--     , temp = ""
--     }


view : Model -> H.Html a
view _ =
    H.canvas [ H.id "native-metaballs-0"] [] -- FIXME: use actual layer index


initalRange : Fluid.Ranges
initalRange =
    { groups = iRange 1 10
    , balls = iRange 4 50
    , radius = fRange 50 100
    , speed = fRange 20 200
    , phase = fRange 0 360 -- Maybe useless
    , amplitude =
        { x = fRange -50 50
        , y = fRange -10 10
        }
    }


generate : (Model -> msg) -> Random.Generator Model -> Cmd msg
generate =
    Fluid.generate


generator
    :  ( Int, Int )
    -> Fluid.Variety
    -> Fluid.Orbit
    -> Product.Palette
    -> Fluid.Ranges
    -> Random.Generator Model
generator =
    Fluid.generator


initialStateGenerator : ( Int, Int ) -> Product.Palette -> Random.Generator Model
initialStateGenerator ( w, h ) palette =
    let
        -- [ color1, color2, color3 ] =
        --     case palette of
        --         [ c1, c2, c3 ]::_ -> [ c1, c2, c3 ]
        speedRange = fRange 0.2 2.0
        tRange = fRange 0 200
        amplitudeRange =
            { x = fRange -0.25 0.75
            , y = fRange -0.25 0.25
            }
        originOffset = { x = 0.6, y = 0.5 }

        generateBall { x, y, radius } =
            Random.map4
                (\speed t arcMultX arcMultY ->
                    { origin = vec2
                        (originOffset.x * toFloat w + x)
                        (originOffset.y * toFloat h + y)
                    , radius = radius
                    , speed = speed
                    , phase = t
                    , amplitude = vec2 arcMultX arcMultY
                    }
                )
                (randomFloatInRange speedRange)
                (randomFloatInRange tRange)
                (randomFloatInRange amplitudeRange.x)
                (randomFloatInRange amplitudeRange.y)

        generateStop stop =
            Random.constant
                ( stop.stop
                , Product.getPaletteColor stop.color palette
                    |> Maybe.withDefault "#000000"
                )

        generateGroup source =
            Random.map2
                Tuple.pair
                (Random.traverse generateBall source.balls)
                (Random.traverse generateStop source.gradient
                    |> Random.map
                        (\stops ->
                            { stops = stops
                            , orientation = Fluid.Vertical
                            }
                        )
                )
            |> Random.map
                (\(balls, gradient) ->
                    { balls = balls
                    , origin = vec2 0 0 -- vec2 originOffset.x originOffset.y
                    , textures = Nothing
                    , gradient = Just gradient
                    }
                )

    in
        initialGroups
            |> Random.traverse generateGroup
            |> Random.map
                    (\groups ->
                        { groups = groups
                        , forSize = Just ( w, h )
                        , variety = Variety 0.5
                        , orbit = Orbit 0.5
                        }
                    )



initialGroups =
    -- group 1
    [
        { balls =
            [ { x = -50, y = 220,  radius = 50 }
            , { x = -20, y = 85,   radius = 90 }
            , { x = -30, y = 50,   radius = 60 }
            , { x = 170, y = 170,  radius = 100 }
            , { x = 370, y = 30,   radius = 40 }
            , { x = 450, y = 150,  radius = 50 }
            , { x = 270, y = -240, radius = 70 }
            , { x = 370, y = -130, radius = 60 }
            , { x = 170, y = -70,  radius = 70 }
            ]
        , gradient =
            [ { color = ColorIII, stop = 0.2 }
            , { color = ColorI,   stop = 0.3 }
            , { color = ColorII,  stop = 0.5 }
            , { color = ColorIII, stop = 0.8 }
            ]
        }
    ,
    -- group 2
        { balls =
            [ { x = 150, y = 350, radius = 30 }
            , { x = 250, y = 250, radius = 70 }
            , { x = 380, y = 280, radius = 30 }
            , { x = 200, y = 100, radius = 25 }
            ]
        , gradient =
            [ { color = ColorII,  stop = 0.2 }
            , { color = ColorIII, stop = 0.3 }
            , { color = ColorII,  stop = 0.5 }
            , { color = ColorIII, stop = 0.7 }
            ]
        }
    ,
    -- group 3
        { balls =
            [ { x = 410, y = -80,  radius = 28 }
            , { x = 340, y = -100, radius = 70 }
            , { x = 200, y = -150, radius = 40 }
            , { x = 250, y = -200, radius = 36 }
            ]
        , gradient =
            [ { color = ColorI,   stop = 0.5 }
            , { color = ColorII,  stop = 0.6 }
            , { color = ColorIII, stop = 0.7 }
            ]
        }
    ,
    -- group 4
        { balls =
            [ { x = -410, y = -270, radius = 48 }
            , { x = -490, y = -230, radius = 34 }
            , { x = -470, y = -320, radius = 40 }
            , { x = -700, y = 250,  radius = 30 }
            , { x = -740, y = 310,  radius = 20 }
            ]
        , gradient =
            [ { color = ColorIII, stop = 0.3 }
            , { color = ColorII,  stop = 0.4 }
            , { color = ColorII,  stop = 0.68 }
            , { color = ColorIII, stop = 0.77 }
            , { color = ColorIII, stop = 0.83 }
            ]
        }
    ,
    -- group 5
        { balls =
            [ { x = -830, y = 40,  radius = 30 }
            , { x = -700, y = 90,  radius = 60 }
            , { x = -540, y = 270, radius = 50 }
            , { x = -490, y = 150, radius = 90 }
            , { x = -300, y = 240, radius = 40 }
            , { x = -200, y = 120, radius = 35 }
            , { x = -350, y = 50,  radius = 70 }
            , { x = -490, y = -40, radius = 60 }
            , { x = -270, y = -70, radius = 50 }
            ]
        , gradient =
            [ { color = ColorIII, stop = 0.3 }
            , { color = ColorII,  stop = 0.5 }
            , { color = ColorI,   stop = 0.6 }
            ]
        }
    ,
    -- group 6
        { balls =
            [ { x = 26,   y = 55,   radius = 120 }
            , { x = -110, y = -90,  radius = 60 }
            , { x = 12,   y = -214, radius = 80 }
            , { x = -300, y = -80,  radius = 120 }
            , { x = -570, y = -120, radius = 50 }
            ]
        , gradient =
            [ { color = ColorI,   stop = 0.2 }
            , { color = ColorII,  stop = 0.35 }
            , { color = ColorIII, stop = 0.55 }
            , { color = ColorII,  stop = 0.75 }
            , { color = ColorI,   stop = 1.0 }
            ]
        }
    ]
