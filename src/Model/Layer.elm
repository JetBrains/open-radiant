module LayerDef exposing (..)

import Html exposing (Html)
import Html as H exposing (..)

import Dict
import Dict exposing (Dict)

import Json.Decode as D
import Json.Encode as E


type Kind
    = Html
    | WebGL
    | Canvas
    | JS


type LayerKind
    = KBackground
    | KConver


-- type alias CreateLayer model view msg =
--     (String -> Maybe (LayerDef model view msg))


type alias RadiantLayer = LayerDef LayerModel (Html Msg) Msg Blend


type alias Registry =
    Dict LayerKind RadiantLayer


type alias Layers =
    List ( Visibility, RadiantLayer )


type alias LayerDef model view msg blend =
    { id : String
    , kind : Kind
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


type LayerModel
    = Background ()
    | Cover ()


type Msg
    = BackgroundMsg ()
    | CoverMsg ()


registry : Registry
registry = Dict.empty


layers : Layers
layers = []


adapt
     : (model -> LayerModel)
    -> (msg -> Msg)
    -> (Kind -> view -> Html Msg)
    -> (LayerModel -> Maybe model)
    -> (Msg -> Maybe msg)
    -> (Blend -> blend)
    -> LayerDef model view msg blend
    -> RadiantLayer
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


-- renderLayer : LayerModel -> Html Msg
-- renderLayer layerModel =
--     case layerModel of
--         Background bgModel -> Background.view bgModel blend


-- updateLayer : Msg -> LayerModel -> LayerModel
-- updateLayer msg layerModel =
--     case ( msg, layerModel ) of
--         ( BackgroundMsg bgMsg, Background bgModel ) ->
--             Background.update bgMsg bgModel


-- renderLayers : List LayerModel -> (LayerModel -> Html Msg) -> Html Msg
-- renderLayers layerModels renderF =
--     H.div [] <| List.map renderF layerModels
