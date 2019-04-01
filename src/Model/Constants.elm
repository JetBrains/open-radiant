module Model.Constants exposing
    ( Constants
    , makeConstants
    )


import Model.AppMode exposing (..)
import Model.SizeRule exposing (..)
import Model.Product as Product

type alias ProductRec = { label : String, id: String, index: Int }


type alias Constants =
    { sizes : List
        { mode: String
        , values : List
            { label: String
            , width: Int, height: Int
            , code: SizePresetCode
            }
        }
    , products : List ProductRec
    --, blendFuncs : List String
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
        , products =
            Product.allProducts
                |> List.indexedMap
                    (\index product ->
                        { label = Product.getName product
                        , id = Product.encode product
                        , index = index
                        }
                    )
        -- TODO: see constants.js
        -- , blendFuncs =
        --     Array.map WGLB.allFuncs
        }
