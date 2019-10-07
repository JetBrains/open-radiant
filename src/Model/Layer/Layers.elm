module Model.Layer.Layers exposing (..)

import Model.Layer.Def exposing (DefId)
import Model.Layer.Layer exposing (..)
import Model.Layer.Context exposing (Context)


type alias Layers = List Layer


type alias Initial = List ( Visibility, Blend, DefId )


init : Context -> Initial -> ( Layers, Cmd Msg )
init ctx layers =
    let
        foldingF ( visibility, blend, defId ) ( prevLayers, prevCmds ) =
            case registry.byId defId of
                Just def ->
                    def.init ctx
                        |> Tuple.mapFirst (\model -> ( visibility, blend, model ))
                        |> Tuple.mapBoth
                                (\l -> l :: prevLayers)
                                (\cmd -> cmd :: prevCmds)
                Nothing ->
                    ( prevLayers, prevCmds )
    in
        List.foldl foldingF ( [], [] ) layers
            |> Tuple.mapSecond Cmd.batch
