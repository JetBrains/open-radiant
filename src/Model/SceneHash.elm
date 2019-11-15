module Model.SceneHash exposing (..)


import Json.Decode as D
import Json.Encode as E


type SceneHash = SceneHash String


fromString : String -> SceneHash
fromString = SceneHash


toString : SceneHash -> String
toString (SceneHash str) = str


decode : D.Decoder SceneHash
decode = D.map fromString <| D.field "hash" D.string


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
is = Just << fromString
