module RenderQueue exposing
    ( groupLayers
    , apply
    )

import Model exposing (..)
import Viewport exposing (Viewport)
import Viewport

import Array exposing (Array)
import Array

import Html exposing (..)
import WebGL as WebGL


type alias LayerToEntities = Model -> Viewport {} -> Int -> LayerDef -> List WebGL.Entity
type alias LayerToHtml     = Model -> Viewport {} -> Int -> LayerDef -> Html Msg

type RenderQueueItem = ToCanvas (Array WebGL.Entity) | ToHtml (Array (Html Msg))
type alias RenderQueue = Array RenderQueueItem


groupLayers : LayerToEntities -> LayerToHtml -> Model -> RenderQueue
groupLayers layerToEntities layerToHtml model =
    let
        viewport = getViewportState model |> Viewport.find
        addToQueue (index, layer) renderQueue =
            let
                indexOfThelastInQueue = Array.length renderQueue - 1
                lastInQueue = renderQueue |> Array.get indexOfThelastInQueue
            in case layer.layer of
                WebGLLayer _ _ ->
                    case lastInQueue of
                        Nothing ->
                            renderQueue
                                |> Array.push
                                    (layerToEntities model viewport index layer
                                        |> Array.fromList
                                        |> ToCanvas)
                        Just (ToCanvas curEntities) ->
                            renderQueue
                                |> Array.set indexOfThelastInQueue
                                    (layerToEntities model viewport index layer
                                        |> Array.fromList
                                        |> Array.append curEntities
                                        |> ToCanvas)
                        Just _ ->
                            renderQueue
                                |> Array.push
                                    (layerToEntities model viewport index layer
                                        |> Array.fromList
                                        |> ToCanvas)
                HtmlLayer _ _ ->
                    case lastInQueue of
                        Nothing ->
                            renderQueue
                                |> Array.push
                                    (Array.empty
                                        |> Array.push (layerToHtml model viewport index layer)
                                        |> ToHtml)
                        Just (ToHtml curHtml) ->
                            renderQueue
                                |> Array.set indexOfThelastInQueue
                                    (Array.empty
                                        |> Array.push (layerToHtml model viewport index layer)
                                        |> Array.append curHtml
                                        |> ToHtml)
                        Just _ ->
                            renderQueue
                                |> Array.push
                                    (Array.empty
                                        |> Array.push (layerToHtml model viewport index layer)
                                        |> ToHtml)
    in
        model.layers
            |> List.indexedMap Tuple.pair
            |> List.filter (Tuple.second >> .on)
            |> List.foldl addToQueue Array.empty


apply : (List (Html Msg) -> Html Msg) -> (List WebGL.Entity -> Html Msg) -> RenderQueue -> Html Msg
apply wrapHtml wrapEntities queue =
    Array.toList queue
        |> List.map
            (\queueItem ->
                case queueItem of
                    ToCanvas entities -> wrapEntities <| Array.toList entities
                    ToHtml elements -> wrapHtml <| Array.toList elements
            )
        |> div []

