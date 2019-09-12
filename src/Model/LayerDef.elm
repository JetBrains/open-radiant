module LayerDef exposing (..)

import Html exposing (Html)
import Html as H exposing (..)

import Json.Decode as D
import Json.Encode as E


type Kind
    = Html
    | WebGL
    | Canvas
    | JS


type alias LayerDef model view msg =
    { id : String
    , kind : Kind
    , init : model
    , encode : model -> E.Value
    , decode : D.Decoder model
    , update : msg -> model -> model
    , view : model -> Blend -> view
    }


-- type alias Layers layer = List (Kind -> layer)

type alias Blend = String

type LayerModel
    = Background ()
    | Cover ()


type Msg
    = BackgroundMsg ()
    | CoverMsg ()


renderLayer : LayerModel -> Html Msg
renderLayer layerModel =
    case layerModel of
        Background bgModel -> Background.view bgModel blend


updateLayer : Msg -> LayerModel -> LayerModel
updateLayer msg layerModel =
    case ( msg, layerModel ) of
        ( BackgroundMsg bgMsg, Background bgModel ) ->
            Background.update bgMsg bgModel


renderLayers : List LayerModel -> (LayerModel -> Html Msg) -> Html Msg
renderLayers layerModels renderF =
    H.div [] <| List.map renderF layerModels
