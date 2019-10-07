module RenderQueue exposing
    ( make
    , apply
    )

import Model.Core exposing (Model, Msg(..))
import Model.Layer.Layer as Layer
import Model.Layer.Layers exposing (Layers)
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
    = HtmlGroup (Array (Layer.Index, Html Layer.Msg))
    | WebGLGroup (Array (Layer.Index, WebGL.Entity))
type alias RenderQueue = Array QueueItem


make : List ( Layer.Index, Layer.View ) -> RenderQueue
make renderedLayers =
    let
        addToQueue ( index, layerView ) renderQueue =
            let
                indexOfThelastInQueue = Array.length renderQueue - 1
                lastInQueue = renderQueue |> Array.get indexOfThelastInQueue
            in case layerView of
                Layer.ToWebGL entity ->
                    case lastInQueue of
                        Just (WebGLGroup existingEntities) ->
                            renderQueue
                                |> Array.set indexOfThelastInQueue
                                    (existingEntities
                                        |> Array.push (index, entity)
                                        |> WebGLGroup)
                        _ ->
                            renderQueue
                                |> Array.push
                                    (Array.empty
                                        |> Array.push (index, entity)
                                        |> WebGLGroup)
                Layer.ToHtml html ->
                    case lastInQueue of
                        Just (HtmlGroup existingHtml) ->
                            renderQueue
                                |> Array.set indexOfThelastInQueue
                                    (existingHtml
                                        |> Array.push (index, html)
                                        |> HtmlGroup)
                        _ ->
                            renderQueue
                                |> Array.push
                                    (Array.empty
                                        |> Array.push (index, html)
                                        |> HtmlGroup)
    in
        renderedLayers
            |> List.foldl addToQueue Array.empty


apply
    :  (List (Layer.Index, Html Layer.Msg) -> Html Msg)
    -> (List (Layer.Index, WebGL.Entity) -> Html Msg)
    -> RenderQueue
    -> Html Msg
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
