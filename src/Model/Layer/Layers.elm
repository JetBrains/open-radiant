module Model.Layer.Layers exposing (..)

import Random

import Model.Layer.Def exposing (DefId, Index(..), Kind, makeIndex, getIndex, Opacity(..))
import Model.Layer.Layer exposing (..)
import Model.Layer.Layer as Layer exposing (Msg)
import Model.Layer.Export exposing (encodeKind)
import Model.Layer.Context exposing (Context)
import Model.Layer.Broadcast as Broadcast exposing (Msg)


type alias Layers = List Layer


type alias Initial =
    List
        { visibility: Visibility
        , blend: Blend
        , fromDef: DefId
        }


init
    :  (Index -> Layer.Msg -> msg)
    -> Context
    -> Initial
    -> ( Layers, Cmd msg ) -- TODO: Result (List String)
init mapMsg ctx initial =
    initial
        |> List.indexedMap Tuple.pair
        |> List.map (Tuple.mapFirst makeIndex)
        |> List.map
            (\( index, { visibility, blend, fromDef }  ) ->

                registry.byId fromDef
                    |> Maybe.map (\def -> def.init index ctx)
                    |> Maybe.map (\( model, cmd ) ->
                        ( layer index (ZOrder <| getIndex index) visibility blend model, cmd ))

            )
        |> List.filterMap identity -- filter out those layers that weren't found in the registry
        |> List.foldr -- we shouldn't lose any layers here since we just add them back
                (\( layer, cmd ) ( prevLayers, prevCmds ) ->

                    let (Layer { index } _) = layer
                    in
                        ( layer :: prevLayers
                        , Cmd.map (mapMsg <| makeIndex index) cmd :: prevCmds
                        )

                )
                ( [], [] )
        |> Tuple.mapSecond Cmd.batch


update
    :  (Index -> Layer.Msg -> msg)
    -> Context
    -> Index
    -> Layer.Msg
    -> Layers
    -> ( Layers, Cmd msg )
update mapMsg ctx layerToUpdate msg =
    updateMap
        mapMsg
        (\((Layer { index } model) as layer) ->

                if index == getIndex layerToUpdate then

                    registry.byModel model
                        |> Maybe.map (\def -> def.update (makeIndex index) ctx msg model)
                        |> Maybe.map (\( newModel, cmd ) ->
                                ( layer |> replaceModel newModel, cmd ))
                        |> Maybe.withDefault ( layer, Cmd.none )

                else ( layer, Cmd.none )

        )


broadcast
    :  (Index -> Layer.Msg -> msg)
    -> Context
    -> Index
    -> Broadcast.Msg
    -> Layers
    -> ( Layers, Cmd msg )
broadcast mapMsg ctx layerToBroadcastTo broadcastMsg =
    updateMap
        mapMsg
        (\((Layer { index } model) as layer) ->

            if index == getIndex layerToBroadcastTo then

                registry.byModel model
                    |> Maybe.map (\def ->
                        def.absorb (makeIndex index) ctx broadcastMsg model)
                    |> Maybe.map (\( newModel, cmd ) ->
                        ( layer |> replaceModel newModel, cmd ))
                    |> Maybe.withDefault ( layer, Cmd.none )

            else ( layer, Cmd.none )
        )


broadcastAll
    :  (Index -> Layer.Msg -> msg)
    -> Context
    -> Broadcast.Msg
    -> Layers
    -> ( Layers, Cmd msg )
broadcastAll mapMsg ctx broadcastMsg =
    updateMap
        mapMsg
        (\((Layer { index } model) as layer) ->

            registry.byModel model
                |> Maybe.map (\def ->
                    def.absorb (makeIndex index) ctx broadcastMsg model)
                |> Maybe.map (\( newModel, cmd ) ->
                        ( layer |> replaceModel newModel, cmd ))
                |> Maybe.withDefault ( layer, Cmd.none )

        )


subscribe
    :  (Index -> Layer.Msg -> msg)
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
modify f indexToChange =
    List.map
        (\((Layer { index } _) as layer) ->
            if index == getIndex indexToChange then
                f layer
            else layer
        )


render : Context -> Layers -> List ( Index, ZOrder, View )
render ctx layers =
    layers |>
        List.map
            (\(Layer { index, zOrder, blend, visibility, opacity } model) ->
                case visibility of
                    Hidden -> Nothing
                    _ ->
                        registry.byModel model
                            |> Maybe.map (\def ->
                                def.view (makeIndex index) ctx ( Just blend, Opacity opacity ) model)
                            |> Maybe.map (\view -> ( makeIndex index, ZOrder zOrder, view ))
            )
        |> List.filterMap identity


updateMap -- do not expose
    :  (Index -> Layer.Msg -> msg)
    -> (Layer -> ( Layer, Cmd Layer.Msg ))
    -> Layers
    -> ( Layers, Cmd msg )
updateMap mapMsg mapF layers =
    layers
        |> List.map mapF
        |> List.foldr -- we shouldn't lose any layers here since we just add them back
                (\( layer, cmd ) ( prevLayers, prevCmds ) ->

                    let (Layer { index } _) = layer
                    in
                        ( layer :: prevLayers
                        , Cmd.map (mapMsg <| makeIndex index) cmd :: prevCmds
                        )

                )
                ( [], [] )
        |> Tuple.mapSecond Cmd.batch


collectStats : Layers ->
    List
        { index : Int
        , def : DefId
        , kind : Kind
        , visibility : Visibility
        }
collectStats layers =
    layers
        |> List.map (\(Layer { index, visibility } model) ->
                registry.byModel model
                    |> Maybe.map
                        (\def ->
                            { index = index
                            , def = def.id
                            , kind = def.kind
                            , visibility = visibility
                            }
                        )
            )
        |> List.filterMap identity


randomizeStats : Layers -> List ( Index, Random.Generator ( Blend, Opacity ) )
randomizeStats forLayers =
    List.map
        (\(Layer { index, blend } _) ->
            ( makeIndex index, Layer.randomStats blend )
        )
        forLayers
