module Model.Range exposing
    ( IntRange(..)
    , FloatRange(..)
    , iRange, fRange
    , getIntMin, getIntMax
    , getFloatMin, getFloatMax
    , randomIntInRange, randomFloatInRange
    )


import Random


type IntRange = IntRange Int Int
type FloatRange = FloatRange Float Float


iRange = IntRange
fRange = FloatRange


getIntMin : IntRange -> Int
getIntMin (IntRange min _) = min


getIntMax : IntRange -> Int
getIntMax (IntRange _ max) = max


getFloatMin : FloatRange -> Float
getFloatMin (FloatRange min _) = min


getFloatMax : FloatRange -> Float
getFloatMax (FloatRange _ max) = max


randomIntInRange : IntRange -> Random.Generator Int
randomIntInRange (IntRange min max) = Random.int min max


randomFloatInRange : FloatRange -> Random.Generator Float
randomFloatInRange (FloatRange min max) = Random.float min max
