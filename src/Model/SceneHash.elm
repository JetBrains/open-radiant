module Model.SceneHash exposing (..)


import Json.Decode as D
import Json.Encode as E

import Model.Util exposing (maybeToDecoder)


type SceneHash = SceneHash String String


toString : SceneHash -> String
toString (SceneHash p1 p2) = p1 ++ "-" ++ p2


decode : D.Decoder SceneHash
decode =
    D.field "hash" D.string
        |> D.map is
        |> D.andThen (maybeToDecoder "hash format is wrong")



encode : SceneHash -> E.Value
-- encode = toString >> E.string
encode hash =
    E.object
        [
            ( "hash"
            , E.string <| toString hash
            )
        ]


is : String -> Maybe SceneHash
is str =
    case String.split "-" str of
        [ p1, p2 ] ->
            case ( String.length p1, String.length p2 ) of
                ( 4, 4 ) -> Just <| SceneHash p1 p2
                _ -> Nothing
        _ -> Nothing

