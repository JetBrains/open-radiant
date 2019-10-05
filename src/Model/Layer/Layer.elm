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
    { byId : String -> Maybe (Def Model View Msg Blend)
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


getId : Layer -> Maybe String
getId ( _, _, model ) =
    registry.byModel model |> Maybe.map .id


isOn : Layer -> Bool
isOn ( visibility, _, _ ) =
    case visibility of
        Visible -> True
        Locked -> True
        Hidden -> False


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
                        <| source.view model
                            <| convertBlend <| Maybe.withDefault NoBlend maybeBlend
                Nothing -> ToHtml <| H.div [] []
    , subscribe =
        \layerModel ->
            case extractModel layerModel of
                Just model -> source.subscribe model |> Sub.map convertMsg
                Nothing -> Sub.none
    , gui = Nothing -- FIXME
    }
