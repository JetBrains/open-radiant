module Model.Range exposing
    ( IntRange
    , FloatRange
    , iRange, fRange
    , getIntMin, getIntMax
    , getFloatMin, getFloatMax
    , randomIntInRange, randomFloatInRange
    , toFloatRange, toIntRange
    , lerp, lerpInt
    )


import Random


type alias IntRange = { min: Int, max : Int }
type alias FloatRange = { min: Float, max : Float }


iRange = IntRange
fRange = FloatRange


toFloatRange : IntRange -> FloatRange
toFloatRange { min, max } =
    { min = toFloat min, max = toFloat max }


toIntRange : (Float -> Int) -> FloatRange -> IntRange
toIntRange f { min, max } =
    { min = f min, max = f max }


getIntMin : IntRange -> Int
getIntMin { min } = min


getIntMax : IntRange -> Int
getIntMax { max } = max


getFloatMin : FloatRange -> Float
getFloatMin { min } = min


getFloatMax : FloatRange -> Float
getFloatMax { max } = max


randomIntInRange : IntRange -> Random.Generator Int
randomIntInRange { min, max } = Random.int min max


randomFloatInRange : FloatRange -> Random.Generator Float
randomFloatInRange { min, max } = Random.float min max


lerp : FloatRange -> Float -> Float
lerp { min, max } v = min + (v * (max - min))


lerpInt : IntRange -> Float -> Float
lerpInt range v = lerp (toFloatRange range) v
