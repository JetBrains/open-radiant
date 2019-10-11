module Layer.Fluid.Random exposing (..)

import Array
import Random
import Random.Extra as Random exposing (traverse)
import Gradient exposing (..)

import Algorithm.Gaussian as Gauss
import Algorithm.Gaussian exposing (gaussian)

import Math.Vector3 as Vec3 exposing (..)
import Math.Vector2 as Vec2 exposing (..)

import Model.Range exposing (..)
import Model.Product as Product
import Model.Product exposing (ColorId(..))

import Layer.Fluid.Model exposing (..)


generator : ( Int, Int ) -> Randomization -> Random.Generator Model
generator size randomization =
    case randomization of
        RandomizeInitial palette initialModel ->
            generateFromInitialState size defaultRange palette initialModel
        RandomizeDynamics range palette variety orbit model ->
            generateDynamics size range palette variety orbit model
        RandomizeAll range palette variety orbit ->
            generateEverything size range palette variety orbit


generateOrigin : Random.Generator Vec2
generateOrigin =
    Random.map2 vec2
        (Random.float -0.1 0.1)
        (Random.float -0.1 0.1)


generateEverything
    : ( Int, Int )
    -> Ranges
    -> Product.Palette
    -> Gauss.Variety
    -> Orbit
    -> Random.Generator Model
generateEverything ( w, h ) range palette variety orbit =
    let
        gaussInFloatRange fRange gaussX =
            Gauss.inFloatRange gaussX variety fRange |> Gauss.unwrap
        generatePosition gaussX =
            Random.map2 vec2
                -- (Random.float 0 <| toFloat w)
                -- (Random.float 0 <| toFloat h)
                (gaussX |> gaussInFloatRange (fRange 0 <| toFloat w))
                (gaussX |> gaussInFloatRange (fRange 0 <| toFloat h))
        generateRadius = gaussInFloatRange range.radius
        generateSpeed = gaussInFloatRange range.speed
        generatePhase = gaussInFloatRange range.phase
        generateAmplitude gaussX =
            Random.map2 vec2
                (gaussX |> gaussInFloatRange range.amplitude.x)
                (gaussX |> gaussInFloatRange range.amplitude.y)
        generateGroup gaussX =
            randomIntInRange range.balls
                |> Random.andThen
                    (\numCircles ->
                        Random.map5
                            Ball
                            (generatePosition gaussX)
                            (generateRadius gaussX)
                            (generateSpeed gaussX)
                            (generatePhase gaussX)
                            (generateAmplitude gaussX)
                            |> Random.list numCircles
                    )
                |> Random.andThen
                    (\balls ->
                        gradientGenerator
                            |> Random.map
                                (\gradient ->
                                    { balls = balls
                                    , gradient = gradient
                                    }
                                )
                    )
                |> Random.andThen
                    (\{ balls, gradient } ->
                        generateOrigin
                        |> Random.map
                            (\origin ->
                                { balls = balls
                                , gradient = gradient
                                , textures = Nothing
                                , origin = origin
                                }
                            )
                    )
        generateEffects gaussX =
            Random.map3
                (\blur fat ring ->
                    { blur = blur, fat = fat, ring = ring }
                )
                (gaussX |> gaussInFloatRange (fRange 0 1))
                (gaussX |> gaussInFloatRange (fRange 0 1))
                (gaussX |> gaussInFloatRange (fRange 0 1))

        makeBall { center, radius } = Ball center radius
    in
        Gauss.generateX
            |> Random.andThen
                (\gaussX ->
                    randomIntInRange range.groups
                        |> Random.map (Tuple.pair gaussX)
                )
            |> Random.andThen
                (\( gaussX, numGroups ) ->
                    Random.list numGroups (generateGroup gaussX)
                        |> Random.map (\groups -> ( gaussX, groups ))
                )
            |> Random.andThen
                (\( gaussX, groups ) ->
                    generateEffects gaussX
                        |> Random.map (\effects -> ( groups, effects ))
                )
            |> Random.map (\( groups, effects ) ->
                { groups = groups
                , forSize = Just ( w, h )
                , variety = variety
                , orbit = orbit
                , effects = effects
                })


generateFromInitialState
    :  ( Int, Int )
    -> Ranges
    -> Product.Palette
    -- -> StaticModelWithGradients
    -> StaticModel
    -> Random.Generator Model
generateFromInitialState ( w, h ) range palette initialState =
    let
        -- [ color1, color2, color3 ] =
        --     case palette of
        --         [ c1, c2, c3 ]::_ -> [ c1, c2, c3 ]
        generateBall { x, y, radius } =
            Random.map4
                (\speed t arcMultX arcMultY ->
                    { origin = vec2 x y
                    , radius = radius
                    , speed = speed
                    , phase = t
                    , amplitude = vec2 arcMultX arcMultY
                    }
                )
                (randomFloatInRange range.speed)
                (randomFloatInRange range.phase)
                (randomFloatInRange range.amplitude.x)
                (randomFloatInRange range.amplitude.y)

        generateStop prevStop =
            Random.constant prevStop

        generateGroup source =
            Random.map2
                Tuple.pair
                (Random.traverse generateBall source.balls)
                (Random.traverse generateStop source.gradient.stops
                    |> Random.map
                        (\stops ->
                            { stops = stops
                            , orientation = source.gradient.orientation
                            }
                        )
                )
            |> Random.map
                (\(balls, gradient) ->
                    { balls = balls
                    , origin = vec2 source.origin.x source.origin.y
                    , textures = Nothing
                    , gradient = gradient
                    }
                )

    in
        Gauss.generateX
            |> Random.andThen
                (\gaussX ->
                    initialState.groups
                        |> Random.traverse generateGroup
                        |> Random.map
                                (\groups ->
                                    { groups = groups
                                    , forSize = Just ( w, h )
                                    , variety = Gauss.Variety 0.5
                                    , orbit = Orbit 0.5
                                    , effects = initialState.effects
                                    }
                                )
                )


generateDynamics
     : ( Int, Int )
    -> Ranges
    -> Product.Palette
    -> Gauss.Variety
    -> Orbit
    -> StaticModel
    -> Random.Generator Model
generateDynamics ( w, h ) range palette variety orbit staticModel =
    let
        -- [ color1, color2, color3 ] =
        --     case palette of
        --         [ c1, c2, c3 ]::_ -> [ c1, c2, c3 ]

        gaussInFloatRange fRange gaussX =
            Gauss.inFloatRange gaussX variety fRange |> Gauss.unwrap

        generateBall gaussX { x, y, radius } =
            Random.map4
                (\speed t arcMultX arcMultY ->
                    { origin = vec2 x y
                    , radius = radius
                    , speed = speed
                    , phase = t
                    , amplitude = vec2 arcMultX arcMultY
                    }
                )
                (gaussX |> gaussInFloatRange range.speed)
                (gaussX |> gaussInFloatRange range.phase)
                (gaussX |> gaussInFloatRange range.amplitude.x)
                (gaussX |> gaussInFloatRange range.amplitude.y)

        generateStop prevStop =
            Random.constant prevStop

        generateGroup gaussX source =
            Random.map2
                Tuple.pair
                (Random.traverse (generateBall gaussX) source.balls)
                (Random.traverse generateStop source.gradient.stops
                    |> Random.map
                        (\stops ->
                            { stops = stops
                            , orientation = source.gradient.orientation
                            }
                        )
                )
            |> Random.map
                (\(balls, gradient) ->
                    { balls = balls
                    , origin = vec2 source.origin.x source.origin.y
                    , textures = Nothing
                    , gradient = gradient
                    }
                )
    in
        Gauss.generateX
            |> Random.andThen
                (\gaussX ->
                    staticModel.groups
                        |> Random.traverse (generateGroup gaussX)
                        |> Random.map
                                (\groups ->
                                    { groups = groups
                                    , forSize = Just ( w, h )
                                    , variety = variety
                                    , orbit = orbit
                                    , effects = staticModel.effects
                                    }
                                )
                )


generate : (Model -> msg) -> Random.Generator Model -> Cmd msg
generate = Random.generate


-- FIXME: Could be simplified using Product.ColorI, Product.ColorII, Product.ColorIII
gradientGenerator
    :  Random.Generator Product.Gradient
gradientGenerator =
    let
        -- _ = Debug.log "palette" palette
        palette = [ ColorI, ColorII, ColorIII ]
        paletteLen = List.length palette
        -- loopedPalette = [ 0, 1, 2, 3, 2, 1 ] -- just remember indices?
        loopedPalette = palette ++ (palette |> List.drop 1 |> List.reverse |> List.drop 1)
        loopedPaletteLen = List.length loopedPalette
        loopedPaletteArray = Array.fromList loopedPalette

        addColors shift stops =
            stops |>
                List.indexedMap
                    (\index stop ->
                        let
                            loopedPaletteIndex = modBy loopedPaletteLen <| index + shift
                        in
                            ( stop
                            , Array.get loopedPaletteIndex loopedPaletteArray
                                |> Maybe.withDefault ColorI
                            )
                    )

        generateStopsWithColors =
            let
                count = 4
                step = 1 / count
                generateNext prevValues index min max =
                    if index < count then
                        Random.float min max
                            |> Random.andThen
                                (\val ->
                                    generateNext
                                        (prevValues ++ [ val ])
                                        (index + 1)
                                        (min + step)
                                        (max + step)
                                )
                    else
                        Random.constant prevValues
            in
                generateNext [] 0 0 step

            |> Random.andThen
                (\stops ->
                    Random.int 0 paletteLen
                        |> Random.map (\shift -> addColors shift stops)
                )

        generateOrientation =
            Random.float 0 1
                |> Random.map
                    (\v -> if v >= 0.5 then Horizontal else Vertical)

    in
        Random.map2
            (\stopsWithColors orientation ->
                { stops = stopsWithColors
                , orientation = orientation
                }
            )
            generateStopsWithColors
            generateOrientation


generateGradient : (Product.Gradient -> msg) -> Random.Generator Product.Gradient -> Cmd msg
generateGradient = Random.generate


-- FIXME: maybe not needed anymore
generateGradientsFor : (Model -> msg) -> Product.Palette -> Model -> Cmd msg
generateGradientsFor putIntoMsg palette model =
    let
        groupsCount = List.length model.groups
    in
        Random.generate
            (\gradients ->
                putIntoMsg
                    { model
                    | groups =
                        List.map2
                            (\group gradient ->
                                { group
                                | gradient = gradient
                                }
                            )
                            model.groups
                            gradients
                    }
            )
            (Random.list groupsCount <| gradientGenerator)
