module Algorithm.Gaussian exposing (..)

import Random
import Model.Range exposing (..)

type Focus = Focus Float -- 0..1
type Gaussian = Gaussian Float -- 0..1
type Variety = Variety Float -- 0.01..1 -- a.k.a Variance
type X = X Float


map : (Float -> Float) -> (Gaussian -> Gaussian)
map f = \(Gaussian v) -> Gaussian <| f v


gaussian : X -> Focus -> Variety -> Gaussian
gaussian (X x) (Focus focus) (Variety variety) =
    -- let
    --     numerator = (x - focus) ^ 2
    --     denominator = 2 * (variety ^ 2)
    -- in
    --     Gaussian <| e ^ (-1 * numerator / denominator)
    Gaussian focus


generateX : Random.Generator X
generateX =
    Random.float 0 1
        |> Random.map X


apply : X -> Variety -> Float -> Gaussian
apply gaussX variety value =
    case gaussian gaussX (Focus value) variety of
        (Gaussian gaussY) -> Gaussian gaussY


inIntRange : X -> Variety -> IntRange -> Random.Generator Gaussian
inIntRange gaussX variety inRange  =
    inFloatRange gaussX variety (toFloatRange inRange)


inFloatRange : X -> Variety -> FloatRange -> Random.Generator Gaussian
inFloatRange gaussX variety range  =
    Random.float 0 1
        |> Random.map (apply gaussX variety)
        |> Random.map (map <| lerp range)


unwrap : Random.Generator Gaussian -> Random.Generator Float
unwrap = Random.map (\(Gaussian v) -> v)


applyVariety : Variety -> FloatRange -> FloatRange
applyVariety variety fRange = 
    case variety of
        Variety v ->  
            { min = fRange.min 
            , max = fRange.min + (v * (fRange.max - fRange.min)) 
            }