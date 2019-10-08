module Model.Layer.Layers exposing (..)

import Model.Layer.Def exposing (DefId, Index(..))
import Model.Layer.Layer exposing (..)
import Model.Layer.Context exposing (Context)


type alias Layers = List Layer


type alias Initial = List ( Visibility, Blend, DefId )


fold
    :  (Index -> Msg -> msg)
    -> (Index -> x -> Maybe ( Layer, Cmd Msg ) )
    -> List x
    -> ( Layers, Cmd msg )
    -- FIXME: some layers could be lost if they don't match, use mapping instead
fold mapMsg f source =
    let
        foldingF x ( prevLayers, prevCmds, index ) =
            case f (Index index) x of
                Just ( layer, cmd ) ->
                    ( layer :: prevLayers
                    , Cmd.map (mapMsg <| Index index) cmd :: prevCmds, index + 1
                    )
                _ ->
                    ( prevLayers, prevCmds, index + 1 )
    in
        List.foldl foldingF ( [], [], 0 ) source
            |> (\( layers, commands, _ ) -> ( layers, commands ))
            |> Tuple.mapSecond Cmd.batch


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
        fold mapMsg foldingF initial


update : (Index -> Msg -> msg) -> Context -> Index -> Msg -> Layers -> ( Layers, Cmd msg )
update mapMsg ctx (Index indexToUpdate) msg layers =
    let
        foldingF index ( visibility, blend, model ) =
            registry.byModel model
                |> Maybe.map (\def -> def.update ctx msg model)
                |> Maybe.map (\( newModel, cmd ) ->
                        ( ( visibility, blend, newModel ), cmd ))
    in
        fold mapMsg foldingF layers


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
