module Model.Layer.Layer exposing (..)

import Html exposing (Html)
import Html as H exposing (..)

import Array exposing (Array)

import Dict
import Dict exposing (Dict)

import Json.Decode as D
import Json.Encode as E

import WebGL as WebGL

import Model.Layer.Def exposing (..)

import Model.WebGL.Blend as WGLBlend


type alias Layer = ( Visibility, Blend, Model )


type Index = Index Int


type View
    = ToHtml (Html Msg)
    | ToWebGL WebGL.Entity


-- type alias CreateLayer = Kind {- -> Model -} -> Maybe Layer


type alias Registry =
    Model -> Maybe (Def Model View Msg Blend)


type alias Blend = String


type Visibility
    = Visible
    | Hidden
    | Locked


type Model
    = Background ()
    | Cover ()


type Msg
    = BackgroundMsg ()
    | CoverMsg ()


registry : Registry
registry = always Nothing
    -- FIXME: fill with all known types of layers


isVisible : Layer -> Bool
isVisible ( visibility, _, _) = visibility /= Hidden


adapt
     : (model -> Model)
    -> (msg -> Msg)
    -> (Kind -> view -> View)
    -> (Model -> Maybe model)
    -> (Msg -> Maybe msg)
    -> (Blend -> Maybe blend)
    -> Def model view msg blend
    -> Def Model View Msg Blend
adapt
    convertModel
    convertMsg
    convertView
    extractModel
    extractMsg
    convertBlend
    source =
    { id = source.id
    , kind = source.kind
    , init = convertModel source.init
    , encode =
        \layerModel ->
            case extractModel layerModel of
                Just m -> source.encode m
                Nothing -> E.string <| "wrong encoder for " ++ source.id
    , decode = source.decode |> D.map convertModel
    , update =
        \mainMsg layerModel ->
            case ( extractMsg mainMsg, extractModel layerModel ) of
                ( Just msg, Just model ) ->
                    source.update msg model
                        |> Tuple.mapFirst convertModel
                        |> Tuple.mapSecond (Cmd.map convertMsg)
                _ -> -- FIXME: return Maybe/Result for the case when message / model doesn't match
                    ( convertModel source.init
                    , Cmd.none
                    )
    , view =
        \layerModel maybeBlend ->
            case extractModel layerModel of
                Just model ->
                    convertView source.kind
                        <| source.view model <| convertBlend <| Maybe.withDefault "" maybeBlend
                Nothing -> ToHtml <| H.div [] []
    , subscribe =
        \layerModel ->
            case extractModel layerModel of
                Just model -> source.subscribe model |> Sub.map convertMsg
                Nothing -> Sub.none
    , gui = Nothing -- FIXME
    }


-- kinda Either, but for ports:
--    ( Just WebGLBlend, Nothing ) --> WebGL Blend
--    ( Nothing, Just String ) --> HTML Blend
--    ( Nothing, Nothing ) --> None
--    ( Just WebGLBlend, Just String ) --> ¯\_(ツ)_/¯
type alias PortBlend =
    ( Maybe WGLBlend.Blend, Maybe String )


type alias PortLayerDef =
    { kind : String
    , blend : PortBlend
    , webglOrHtml : String
    , isOn : Bool
    , name : String
    , model : String
    }


encodeKind : Kind -> String
encodeKind kind =
     case kind of
        WebGL -> "webgl"
        Canvas -> "canvas"
        JS -> "js"
        Html -> "html"
