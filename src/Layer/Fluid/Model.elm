module Layer.Fluid.Model exposing (..)


import WebGL
import WebGL.Settings exposing (Setting)
import WebGL.Texture as Texture
import WebGL.Texture exposing (Texture)

import Algorithm.Base64.BMP exposing (encode24With)
import Algorithm.Base64.Image exposing (..)
import Algorithm.Gaussian as Gauss
import Algorithm.Gaussian exposing (gaussian)

import Math.Vector2 as Vec2 exposing (..)

import Model.Product as Product
import Model.Product exposing (ColorId(..))
import Model.Range exposing (..)


type Msg
    = RequestNew
    | Rebuild Model
    | RegenerateGradients
    | ChangeVariety Gauss.Variety
    | ChangeOrbit Orbit
    | LoadGradientTextures (List Base64Url)
    | ApplyTextures
        (List { gradient : TextureAndSize, data : TextureAndSize })


type alias Ball =
    { origin : Vec2
    , radius : Float
    , speed : Float
    , phase : Float
    , amplitude : Vec2
    }


type alias BallGroup =
    { balls : List Ball
    , textures :
        Maybe
            { gradient : TextureAndSize
            , data : TextureAndSize
            }
    , gradient : Product.Gradient
    , origin : Vec2
    }


type alias Effects = { blur : Float, fat : Float, ring : Float }


type EffectsChange
    = ChangeBlur Float
    | ChangeFat Float
    | ChangeRing Float
    | ChangeNothing


type alias Model =
    { groups : List BallGroup
    , forSize : Maybe ( Int, Int ) -- FIXME: if we always use the main window size
                                   -- for generating model, then it's the duplication
    , variety : Gauss.Variety
    , orbit : Orbit
    , effects : Effects
    }


type alias StaticModel =
    { groups :
        List
            { balls : List
                { radius : Float
                , x : Float
                , y : Float
                }
            , gradient : Product.Gradient
            , origin : { x : Float, y : Float }
            }
    , effects : Effects
    }


type alias Time = Float


type alias TextureAndSize = ( Texture, Vec2 )


type Base64Url = Base64Url String


type Randomization
    = RandomizeInitial Product.Palette StaticModel
    | RandomizeStatics Ranges Model
    | RandomizeDynamics Ranges Product.Palette Gauss.Variety Orbit StaticModel
    | RandomizeEverything Ranges Product.Palette Gauss.Variety Orbit


type alias Ranges =
    { groups : IntRange
    , balls : IntRange
    , radius : FloatRange
    , speed : FloatRange
    , phase : FloatRange
    , amplitude :
        { x : FloatRange
        , y : FloatRange
        }
    }

type Orbit = Orbit Float -- 0..1

-- type FocusChange
--     = GroupsCount Int
--     | BallsCount Int
--     | Speed Float
--     | Phase Float
--     | AmplitudeX Float
--     | AmplitudeY Float


defaultEffects : Effects
defaultEffects =
    { blur = 0.0
    , fat = 0.2
    , ring = 0.0
    }


-- defaultColors = [ "#f38038", "#ed3d7d", "#341f49" ]


defaultOrbit = Orbit 0.5
defaultVariety = Gauss.Variety 0.5


init : Model
init =
    { groups = [ ]
    , forSize = Nothing
    , variety = Gauss.Variety 0.5
    , orbit = Orbit 0.5
    , effects = defaultEffects
    }


defaultRange : Ranges
defaultRange =
    { groups = iRange 1 10
    , balls = iRange 4 50
    , radius = fRange 50 100
    , speed = fRange 150 250
    , phase = fRange 0 360 -- Maybe useless
    , amplitude =
        { x = fRange -30 30
        , y = fRange -30 30
        }
    }


extractStatics : Model -> StaticModel
extractStatics model =
    { groups =
        model.groups
        |> List.map (\group ->
            { balls =
                group.balls
                |> List.map (\ball ->
                        { x = Vec2.getX ball.origin
                        , y = Vec2.getY ball.origin
                        , radius = ball.radius
                        })
            , gradient = group.gradient
            , origin =
                { x = Vec2.getX group.origin
                , y = Vec2.getY group.origin
                }
            })
    , effects = model.effects
    }


applyEffectsChange : EffectsChange -> Effects -> Effects
applyEffectsChange change effects =
    case change of
        ChangeBlur v -> { effects | blur = v }
        ChangeFat v -> { effects | fat = v }
        ChangeRing v -> { effects | ring = v }
        ChangeNothing -> effects


