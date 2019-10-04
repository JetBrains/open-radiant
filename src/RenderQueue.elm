module RenderQueue exposing
    ( groupLayers
    , apply
    )

import Model.Core exposing (Model)
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

type QueueItem
    = HtmlGroup (Array (Html Layer.Msg))
    | WebGLGroup (Array WebGL.Entity)
type alias RenderQueue = Array QueueItem


groupLayers : Model -> RenderQueue
groupLayers model =
    let
        viewport = getViewportState model |> Viewport.find
        addToQueue (index, layer) renderQueue =
            let
                indexOfThelastInQueue = Array.length renderQueue - 1
                lastInQueue = renderQueue |> Array.get indexOfThelastInQueue
            in case layer.layer of
                Layer.ToWebGL entity ->
                    case lastInQueue of
                        Just (WebGLGroup existingEntities) ->
                            renderQueue
                                |> Array.set indexOfThelastInQueue
                                    (Array.push entity existingEntities |> WebGLGroup)
                        _ ->
                            renderQueue
                                |> Array.push
                                    (Array.empty |> Array.push entity |> WebGLGroup)
                Layer.ToHtml html ->
                    case lastInQueue of
                        Just (HtmlGroup existingHtml) ->
                            renderQueue
                                |> Array.set indexOfThelastInQueue
                                    (Array.push html existingHtml |> HtmlGroup)
                        _ ->
                            renderQueue
                                |> Array.push
                                    (Array.empty |> Array.push html |> HtmlGroup)
    in
        model.layers
            |> List.indexedMap Tuple.pair
            |> List.filter (Tuple.second >> .on)
            |> List.foldl addToQueue Array.empty


apply
    : (List (Html Layer.Msg) -> Html Layer.Msg)
    -> (List WebGL.Entity -> Html Layer.Msg)
    -> RenderQueue
    -> Html Layer.Msg
apply wrapHtml wrapEntities queue =
    Array.toList queue
        |> List.map
            (\queueItem ->
                case queueItem of
                    WebGLGroup entities -> wrapEntities <| Array.toList entities
                    HtmlGroup htmls -> wrapHtml <| Array.toList htmls
            )
        |> div []


getViewportState : Model -> Viewport.State
getViewportState { paused, size, origin, theta } =
    { paused = paused
    , size = getRuleSizeOrZeroes size
    , origin = origin
    , theta = theta
    }
