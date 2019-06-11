module Model.Range exposing
    ( IntRange
    , FloatRange
    , iRange, fRange
    , getIntMin, getIntMax
    , getFloatMin, getFloatMax
    , randomIntInRange, randomFloatInRange
    )


import Random


type alias IntRange = { min: Int, max : Int }
type alias FloatRange = { min: Float, max : Float }


iRange = IntRange
fRange = FloatRange


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
