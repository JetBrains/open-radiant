module Model.Version exposing (..)


import Json.Encode as E
import Json.Decode as D


type Version = Version Int Int Int


current : Version
current = Version 3 0 0


encode : Version -> E.Value
encode (Version major minor build) =
    E.list E.int [ major, minor, build ]


decode : D.Decoder (Maybe Version)
decode =
    D.list D.int
        |> D.map
            (\intList ->
                case intList of
                    major::minor::build::_ ->
                        Just <| Version major minor build
                    _ -> Nothing
            )
        |> D.maybe
        |> D.map (Maybe.andThen identity)
