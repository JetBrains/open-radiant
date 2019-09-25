module Model.Layer exposing (..)

import Html exposing (Html)
import Html as H exposing (..)

import Dict
import Dict exposing (Dict)

import Json.Decode as D
import Json.Encode as E

import Model.WebGL.Blend as WGLBlend

type ViewKind
    = Html
    | WebGL
    | Canvas
    | JS


type Kind
    = KBackground
    | KCover


type Index = Index Int


type alias Layer = Def Model (Html Msg) Msg Blend


-- type alias CreateLayer = Kind {- -> Model -} -> Maybe Layer


type alias Registry =
    Kind -> Maybe Layer


type alias Layers =
    List ( Visibility, Layer )


type alias Def model view msg blend =
    { id : String
    , kind : ViewKind
    , init : model
    , encode : model -> E.Value
    , decode : D.Decoder model
    , update : msg -> model -> ( model, Cmd msg )
    , view : model -> blend -> view
    , subscribe : model -> Sub msg
    }


type alias Blend = String


type Visibility
    = Visible
    | Hidden


type Model
    = Background ()
    | Cover ()


type Msg
    = BackgroundMsg ()
    | CoverMsg ()


registry : Registry
registry = always Nothing
    -- FIXME: fill with all known types of layers


adapt
     : (model -> Model)
    -> (msg -> Msg)
    -> (ViewKind -> view -> Html Msg)
    -> (Model -> Maybe model)
    -> (Msg -> Maybe msg)
    -> (Blend -> blend)
    -> Def model view msg blend
    -> Layer
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
                _ -> -- FIXME: return Maybe for the case when message / model doesn't match
                    ( convertModel source.init
                    , Cmd.none
                    )
    , view =
        \layerModel blend ->
            case extractModel layerModel of
                Just model -> convertView source.kind <| source.view model <| convertBlend blend
                Nothing -> H.div [] []
    , subscribe =
        \layerModel ->
            case extractModel layerModel of
                Just model -> source.subscribe model |> Sub.map convertMsg
                Nothing -> Sub.none
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
