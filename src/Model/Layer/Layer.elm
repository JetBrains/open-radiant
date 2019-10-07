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

import Model.Layer.Blend.Html as HtmlBlend
import Model.Layer.Blend.WebGL as WGLBlend


type alias Layer = ( Visibility, Blend, Model )


type Index = Index Int


type View
    = ToHtml (Html Msg)
    | ToWebGL WebGL.Entity


-- type alias CreateLayer = Kind {- -> Model -} -> Maybe Layer


type alias Registry =
    { byId : DefId -> Maybe (Def Model View Msg Blend)
    , byModel : Model -> Maybe (Def Model View Msg Blend)
    }


type Blend
    = ForWebGL WGLBlend.Blend
    | ForHtml HtmlBlend.Blend
    | NoBlend


type Visibility
    = Visible
    | Hidden
    | Locked


type Model
    = Background ()
    | Cover ()
    | Unknown
    -- TODO: add mirrored FSS


type Msg
    = BackgroundMsg ()
    | CoverMsg ()


registry : Registry
registry =
    { byId = always Nothing
    , byModel = always Nothing
    }
    -- FIXME: fill with all known types of layers


isVisible : Layer -> Bool
isVisible ( visibility, _, _) = visibility /= Hidden


getId : Layer -> Maybe DefId
getId ( _, _, model ) =
    registry.byModel model |> Maybe.map .id


isOn : Layer -> Bool
isOn ( visibility, _, _ ) =
    case visibility of
        Visible -> True
        Locked -> True
        Hidden -> False


hide : Layer -> Layer
hide ( _ , blend, model ) =
    ( Hidden, blend, model )


show : Layer -> Layer
show ( _ , blend, model ) =
    ( Visible, blend, model )


lock : Layer -> Layer
lock ( _ , blend, model ) =
    ( Locked, blend, model )


unlock : Layer -> Layer
unlock ( _ , blend, model ) =
    ( Visible, blend, model )


changeBlend : Blend -> Layer -> Layer
changeBlend newBlend ( visibility, _, model ) =
    ( visibility, newBlend, model )


alterBlend : (Blend -> Blend) -> Layer -> Layer
alterBlend changeF ( visibility, curBlend, model ) =
    ( visibility, changeF curBlend, model )


alterWebGlBlend : WGLBlend.BlendChange -> Layer -> Layer
alterWebGlBlend changeF =
    alterBlend
        (\blend ->
            case blend of
                ForWebGL wglBlend -> changeF wglBlend |> ForWebGL
                _ -> blend
        )


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
    let
        adaptUpdateTuple f =
            f |> Tuple.mapFirst convertModel
              |> Tuple.mapSecond (Cmd.map convertMsg)
    in
        { id = source.id
        , kind = source.kind
        , init = adaptUpdateTuple << source.init
        , encode =
            \ctx layerModel ->
                case extractModel layerModel of
                    Just m -> source.encode ctx m
                    Nothing -> E.string <| "wrong encoder for " ++ source.id
        , decode = D.map convertModel << source.decode
        , update =
            \ctx mainMsg layerModel ->
                case ( extractMsg mainMsg, extractModel layerModel ) of
                    ( Just msg, Just model ) ->
                        adaptUpdateTuple <|
                            source.update ctx msg model
                    _ -> -- FIXME: return Maybe/Result for the case when message / model doesn't match
                        adaptUpdateTuple <| source.init ctx
        , view =
            \ctx layerModel maybeBlend ->
                case extractModel layerModel of
                    Just model ->
                        convertView source.kind
                            <| source.view ctx model
                                <| convertBlend <| Maybe.withDefault NoBlend maybeBlend
                    Nothing -> ToHtml <| H.div [] []
        , subscribe =
            \ctx layerModel ->
                case extractModel layerModel of
                    Just model -> source.subscribe ctx model |> Sub.map convertMsg
                    Nothing -> Sub.none
        , gui = Nothing -- FIXME
        }
