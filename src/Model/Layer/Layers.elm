module Model.Layer.Layers exposing (..)

import Model.Layer.Def exposing (DefId, Index(..))
import Model.Layer.Layer exposing (..)
import Model.Layer.Context exposing (Context)


type alias Layers = List Layer


type alias Initial = List ( Visibility, Blend, DefId )


init
    :  (Index -> Msg -> msg)
    -> Context
    -> Initial
    -> ( Layers, Cmd msg ) -- TODO: Result (List String)
init mapMsg ctx initial =
    let
        foldingF index ( visibility, blend, defId ) =
            registry.byId defId
                |> Maybe.map (\def -> def.init ctx)
                |> Maybe.map (\( model, cmd ) ->
                        ( ( visibility, blend, model ), cmd ))
    in
        foldUpdate mapMsg foldingF initial


update : (Index -> Msg -> msg) -> Context -> Index -> Msg -> Layers -> ( Layers, Cmd msg )
update mapMsg ctx (Index layerToUpdate) msg layers =
    let
        foldingF (Index index) ( visibility, blend, model ) =
            if index == layerToUpdate then
                registry.byModel model
                    |> Maybe.map (\def -> def.update ctx msg model)
                    |> Maybe.map (\( newModel, cmd ) ->
                            ( ( visibility, blend, newModel ), cmd ))
            else Just ( ( visibility, blend, model ), Cmd.none )
    in
        foldUpdate mapMsg foldingF layers


subscribe : (Index -> Msg -> msg) -> Context -> Layers -> Sub msg
subscribe mapMsg ctx layers =
    let
        foldingF index ( _, _, model ) =
            registry.byModel model
                |> Maybe.map (\def ->
                    def.subscribe ctx model
                        -- |> Sub.map ((|>) index))
                        |> Sub.map (\f -> f index))
    in
        foldSubscribe mapMsg foldingF layers


modify : (Layer -> Layer) -> Index -> Layers -> Layers
modify f (Index indexToChange) =
    List.indexedMap
        (\index layer ->
            if index == indexToChange then
                f layer
            else layer
        )


render : Layers -> List ( Index, View )
render layers =
    [] -- FIXME: implement



foldUpdate
    :  (Index -> Msg -> msg)
    -> (Index -> x -> Maybe ( Layer, Cmd Msg ) )
    -> List x
    -> ( Layers, Cmd msg )
    -- FIXME: some layers could be lost if they don't match, use mapping instead
foldUpdate mapMsg locUpdate source =
    let
        foldingF x ( prevLayers, prevCmds, index ) =
            case locUpdate (Index index) x of
                Just ( layer, cmd ) ->
                    ( layer :: prevLayers
                    , Cmd.map (mapMsg <| Index index) cmd :: prevCmds
                    , index + 1
                    )
                _ ->
                    ( prevLayers, prevCmds, index + 1 )
    in
        List.foldl foldingF ( [], [], 0 ) source
            |> (\( layers, commands, _ ) -> ( layers, commands ))
            |> Tuple.mapSecond Cmd.batch


foldSubscribe
    :  (Index -> Msg -> msg)
    -> (Index -> x -> Maybe (Sub Msg))
    -> List x
    -> Sub msg
    -- FIXME: some layers could be lost if they don't match, use mapping instead
foldSubscribe mapMsg locSubscribe source =
    let
        foldingF x ( prevSubs, index ) =
            case locSubscribe (Index index) x of
                Just sub ->
                    -- ( Sub.map (\f index_ -> (mapMsg index_ <| f index_)) sub :: prevSubs
                    ( Sub.map (mapMsg <| Index index) sub :: prevSubs
                    , index + 1
                    )
                _ ->
                    ( prevSubs
                    , index + 1
                    )
    in
        List.foldl foldingF ( [], 0 ) source
            |> (\( subs, _ ) -> subs)
            |> Sub.batch

