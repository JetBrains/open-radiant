module RenderQueue exposing
    ( groupLayers
    , apply
    )

import Model.Core exposing (..)
import Model.Layer as Layer
import Model.SizeRule exposing (..)

import Viewport exposing (Viewport)
import Viewport

import Array exposing (Array)
import Array

import Html exposing (..)
import WebGL as WebGL


-- type alias LayerToEntities = Layer.Model -> Viewport {} -> Int -> Layer.Def -> List WebGL.Entity
-- type alias LayerToHtml     = Layer.Model -> Viewport {} -> Int -> Layer.Def -> Html Msg

type alias RenderQueue = Array Layer.View


groupLayers : Model -> RenderQueue
groupLayers model =
    let
        viewport = getViewportState model |> Viewport.find
        addToQueue (index, layer) renderQueue =
            let
                indexOfThelastInQueue = Array.length renderQueue - 1
                lastInQueue = renderQueue |> Array.get indexOfThelastInQueue
            in case layer.layer of
                Layer.ToWebGL entities ->
                    case lastInQueue of
                        Just (Layer.ToWebGL curEntities) ->
                            renderQueue
                                |> Array.set indexOfThelastInQueue
                                    (entities
                                        |> Array.append curEntities
                                        |> Layer.ToWebGL)
                        _ -> renderQueue
                                |> Array.push
                                    (entities |> Layer.ToWebGL)
                Layer.ToHtml html ->
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


getViewportState : Model -> Viewport.State
getViewportState { paused, size, origin, theta } =
    { paused = paused
    , size = getRuleSizeOrZeroes size
    , origin = origin
    , theta = theta
    }
