module Layer.Fluid.Export exposing (..)

import Json.Decode as D
import Json.Decode.Extra as D exposing (andMap)
import Json.Encode as E

import Math.Vector3 as Vec3 exposing (..)
import Math.Vector2 as Vec2 exposing (..)

import Model.Range exposing (..)
import Model.Product as Product
import Model.Layer.Context exposing (Context)

import Algorithm.Gaussian as Gaussian

import Layer.Fluid.Model exposing (..)
import Layer.Fluid.Model as Model exposing (defaultRange)


encodeEffectsChange : EffectsChange -> { subject : String, value : Float }
encodeEffectsChange change =
    case change of
        ChangeBlur v ->  { subject = "blur", value = v }
        ChangeFat v ->   { subject = "fat", value = v }
        ChangeRing v ->  { subject = "ring", value = v }
        ChangeNothing -> { subject = "nothing", value = -1.0 }


encode : Context -> Model -> E.Value
encode ctx model =
    let
        encodeBall ball =
            E.object
                [ ( "x", E.float <| Vec2.getX ball.origin )
                , ( "y", E.float <| Vec2.getY ball.origin )
                , ( "r", E.float ball.radius )
                , ( "speed", E.float ball.speed )
                , ( "phase", E.float ball.phase )
                , ( "ax", E.float <| Vec2.getX ball.amplitude )
                , ( "ay", E.float <| Vec2.getY ball.amplitude )
                ]
        encodeSize ( width, height )  =
            E.object
                [ ( "width", E.int width )
                , ( "height", E.int height )
                ]
        encodeGroup group =
            E.object
                [ ( "balls", E.list encodeBall group.balls )
                , ( "gradient" ,
                        Product.encodeGradient
                            ctx.palette
                            group.gradient
                    )
                , ( "origin"
                    , E.object
                    [ ( "x", E.float <| Vec2.getX group.origin )
                    , ( "y", E.float <| Vec2.getY group.origin )
                    ]
                    )
                ]
    in
        [ ( "groups", E.list encodeGroup model.groups )
        , ( "size",
                model.forSize
                    |> Maybe.map encodeSize
                    |> Maybe.withDefault E.null)
        , ( "variety", E.float <| case model.variety of Gaussian.Variety v -> v)
        , ( "orbit", E.float <| case model.orbit of Orbit v -> v)
        , ( "effects", E.object
                [ ( "blur", E.float model.effects.blur )
                , ( "fat", E.float model.effects.fat )
                , ( "ring", E.float model.effects.ring )
                ])
        , ( "atHeight",
                model.atHeight
                    |> Maybe.map E.int
                    |> Maybe.withDefault E.null)
        ] |> E.object



decode : Context -> D.Decoder Model
decode ctx =
    let
        range = Model.defaultRange
        makeBall =
            D.map7
                (\x y r speed phase ax ay ->
                    { origin = Vec2.vec2 x y
                    , radius = r
                    , speed = speed
                    , phase = phase
                    , amplitude = Vec2.vec2 ax ay
                    }
                )
                (D.field "x" D.float)
                (D.field "y" D.float)
                (D.field "r" D.float)
                (D.field "speed" D.float |> D.withDefault (getFloatMin range.speed))
                (D.field "phase" D.float |> D.withDefault 0)
                (D.field "ax" D.float |> D.withDefault (getFloatMin range.amplitude.x))
                (D.field "ay" D.float |> D.withDefault (getFloatMin range.amplitude.y))
        makeOrigin =
            D.map2
                Vec2.vec2
                (D.field "x" D.float)
                (D.field "y" D.float)
        makeGroup =
            D.map3
                (\balls gradient origin ->
                    { balls = balls
                    , textures = Nothing
                    , gradient = gradient |> Maybe.withDefault Product.emptyGradient
                    , origin = origin |> Maybe.withDefault (Vec2.vec2 0 0)
                    }
                )
                (D.field "balls" <| D.list makeBall)
                (D.field "gradient"
                    <| D.maybe <| Product.decodeGradient ctx.palette)
                (D.field "origin" <| D.maybe <| makeOrigin)
        makeSize =
            D.map2 Tuple.pair
                (D.field "width" D.int)
                (D.field "height" D.int)
    in
        D.map6
            (\groups forSize variety orbit effects atHeight ->
                { groups = groups
                , forSize = forSize
                , variety = variety
                        |> Maybe.map Gaussian.Variety
                        |> Maybe.withDefault defaultVariety
                , orbit = orbit
                        |> Maybe.map Orbit
                        |> Maybe.withDefault defaultOrbit
                , effects = effects
                        |> Maybe.withDefault defaultEffects
                , atHeight = atHeight
                        |> Maybe.andThen identity
                })
            (D.field "groups" <| D.list makeGroup)
            (D.maybe <| D.field "forSize" makeSize)
            (D.maybe <| D.field "variety" D.float)
            (D.maybe <| D.field "orbit" D.float)
            (D.maybe <| D.field "effects"
                <| D.map3
                    (\blur fat ring ->
                        { blur = blur
                        , fat = fat
                        , ring = ring
                        }
                    )
                    (D.field "blur" D.float)
                    (D.field "fat" D.float)
                    (D.field "ring" D.float))
            (D.maybe <| D.field "atHeight" <| D.maybe D.int)
