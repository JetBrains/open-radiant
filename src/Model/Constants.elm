module Model.Constants exposing
    ( Constants
    , makeConstants
    )


import Model.AppMode exposing (..)
import Model.SizeRule exposing (..)


type alias Constants =
    { sizes : List
        { mode: String
        , values : List
            { label: String
            , width: Int, height: Int
            , code: SizePresetCode
            }
        }
    , products : List { product : String, label: String }
    }


makeConstants : Constants
makeConstants =
    let
        getModeLabel = encodeMode
        sizePresetsConstants mode =
            getSizePresets mode
                |> List.map (\preset ->
                        case getPresetSize preset of
                            ( w, h ) ->
                                { label = getPresetLabel preset
                                , code = encodePreset preset
                                , width = w, height = h
                                }
                    )
    in
        { sizes =
            [ Production, Release, Development, Ads ]
                |> List.map (\mode ->
                      { mode = getModeLabel mode
                      , values = sizePresetsConstants mode
                      }
                   )
        , products = []
        }
