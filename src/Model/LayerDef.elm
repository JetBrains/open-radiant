module LayerDef exposing (..)

import Html exposing (Html)
import Html as H exposing (..)

import Dict

import Json.Decode as D
import Json.Encode as E


type Kind
    = Html
    | WebGL
    | Canvas
    | JS


type alias CreateLayer model view msg =
    (String -> Maybe (LayerDef model view msg))


type alias LayerRegistry model view msg =
    Dict.Dict String (LayerDef model view msg)



type alias LayerDef model view msg =
    { id : String
    , kind : Kind
    , init : model
    , encode : model -> E.Value
    , decode : D.Decoder model
    , update : msg -> model -> ( model, Cmd msg )
    , view : model -> Blend -> view
    }


-- type alias Layers layer = List (Kind -> layer)

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


registry : LayerRegistry LayerModel (Html Msg) Msg
registry = Dict.empty


adapt
     : (model -> LayerModel)
    -> (msg -> Msg)
    -> (Kind -> view -> Html Msg)
    -> (LayerModel -> Maybe model)
    -> (Msg -> Maybe msg)
    -> LayerDef model view msg
    -> LayerDef LayerModel (Html Msg) Msg
adapt convertModel convertMsg convertView extractModel extractMsg source =
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
                Just model -> convertView source.kind <| source.view model blend
                Nothing -> H.div [] []
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
