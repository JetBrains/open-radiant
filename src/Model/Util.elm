module Model.Util exposing (..)

import Json.Decode as D

intPairDecoder : D.Decoder (Int, Int)
intPairDecoder =
    D.map2 Tuple.pair
        (D.field "v1" D.int)
        (D.field "v2" D.int)


resultToDecoder : Result String a -> D.Decoder a
resultToDecoder result =
    case result of
        Ok res -> D.succeed res
        Err err -> D.fail err


resultToDecoder_ : (x -> String) -> Result x a -> D.Decoder a
resultToDecoder_ errToStr result =
    case result of
        Ok res -> D.succeed res
        Err err -> D.fail <| errToStr err


maybeToDecoder : String -> Maybe a -> D.Decoder a
maybeToDecoder failureReason maybe =
    case maybe of
        Just v -> D.succeed v
        Nothing -> D.fail failureReason
