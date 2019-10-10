module Model.Layer.Layers exposing (..)

import Model.Layer.Def exposing (DefId, Index(..))
import Model.Layer.Layer exposing (..)
import Model.Layer.Context exposing (Context)


type alias Layers = List Layer


type alias Initial =
    List
        { visibility: Visibility
        , blend: Blend
        , fromDef: DefId
        }


init
    :  (Index -> Msg -> msg)
    -> Context
    -> Initial
    -> ( Layers, Cmd msg ) -- TODO: Result (List String)
init mapMsg ctx initial =
    initial
        |> List.indexedMap Tuple.pair
        |> List.map (Tuple.mapFirst Index)
        |> List.map
            (\( Index index, { visibility, blend, fromDef }  ) ->

                registry.byId fromDef
                    |> Maybe.map (\def -> def.init ctx)
                    |> Maybe.map (\( model, cmd ) ->
                        ( layer (Index index) (ZOrder index) visibility blend model, cmd ))

            )
        |> List.filterMap identity -- filter out those layers that weren't found in the registry
        |> List.foldr -- we shouldn't lose any layers here since we just add them back
                (\( layer, cmd ) ( prevLayers, prevCmds ) ->

                    let (Layer { index } _) = layer
                    in
                        ( layer :: prevLayers
                        , Cmd.map (mapMsg <| Index index) cmd :: prevCmds
                        )

                )
                ( [], [] )
        |> Tuple.mapSecond Cmd.batch


update
    :  (Index -> Msg -> msg)
    -> Context
    -> Index
    -> Msg
    -> Layers
    -> ( Layers, Cmd msg )
update mapMsg ctx (Index layerToUpdate) msg layers =
    layers
        |> List.indexedMap Tuple.pair
        |> List.map (Tuple.mapFirst Index)
        |> List.map
            (\( Index index, layer ) ->

                if index == layerToUpdate then

                    case layer of
                        Layer _ model ->
                            registry.byModel model
                                |> Maybe.map (\def -> def.update ctx msg model)
                                |> Maybe.map (\( newModel, cmd ) ->
                                        ( layer |> replaceModel newModel, cmd ))
                                |> Maybe.withDefault ( layer, Cmd.none )

                else ( layer, Cmd.none )

            )
        |> List.foldr -- we shouldn't lose any layers here since we just add them back
                (\( layer, cmd ) ( prevLayers, prevCmds ) ->

                    let (Layer { index } _) = layer
                    in
                        ( layer :: prevLayers
                        , Cmd.map (mapMsg <| Index index) cmd :: prevCmds
                        )

                )
                ( [], [] )
        |> Tuple.mapSecond Cmd.batch


subscribe
    :  (Index -> Msg -> msg)
    -> Context
    -> Layers
    -> Sub msg
subscribe mapMsg ctx layers =
    layers
        |> List.map
            (\(Layer _ model) ->
                registry.byModel model
                    |> Maybe.map (\def -> def.subscribe ctx model)
            )
        |> List.filterMap identity
        |> List.map (Sub.map (\( msgIndex, msg ) -> mapMsg msgIndex msg))
        |> Sub.batch


modify : (Layer -> Layer) -> Index -> Layers -> Layers
modify f (Index indexToChange) =
    List.map
        (\((Layer { index } _) as layer) ->
            if index == indexToChange then
                f layer
            else layer
        )


render : Context -> Layers -> List ( Index, ZOrder, View )
render ctx layers =
    layers |>
        List.map
            (\(Layer { index, zOrder, blend } model) ->
                registry.byModel model
                    |> Maybe.map (\def ->
                        def.view ctx (Just blend) model)
                    |> Maybe.map (\view -> ( Index index, ZOrder zOrder, view ))
            )
        |> List.filterMap identity


