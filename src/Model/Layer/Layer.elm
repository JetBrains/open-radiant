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
import Model.Layer.Def as Def exposing (Index)

import Model.Layer.Blend.Html as Html exposing (Blend)
import Model.Layer.Blend.WebGL as WebGL exposing (Blend, BlendChange)

import Layer.Background.Background as Background exposing (..)
import Layer.Cover.Cover as Cover exposing (..)
import Layer.Fluid.Fluid as Fluid exposing (..)


type alias Layer = ( Visibility, Blend, Model )


type View
    = ToHtml (Html Msg)
    | ToWebGL WebGL.Entity


-- type alias CreateLayer = Kind {- -> Model -} -> Maybe Layer


type Blend
    = ForWebGL WebGL.Blend
    | ForHtml Html.Blend
    | NoBlend


type Visibility
    = Visible
    | Hidden
    | Locked


type Model
    = Background Background.Model
    | Cover Cover.Model
    | Unknown
    -- TODO: add mirrored FSS


type Msg
    = BackgroundMsg Background.Msg
    | CoverMsg ()


type Adaptation model view msg blend =
    Adaptation
        { convertModel : model -> Model
        , convertMsg : msg -> Msg
        , extractModel : Model -> Maybe model
        , extractMsg : Msg -> Maybe msg
        , convertView : view -> View
        -- , extractView : View -> Maybe view
        -- , convertBlend : blend -> Blend
        , extractBlend : Blend -> Maybe blend
        }


type alias Registry =
    { byId : DefId -> Maybe (Def Model View Msg Blend)
    , byModel : Model -> Maybe (Def Model View Msg Blend)
    , byMsg : Msg -> Maybe (Def Model View Msg Blend)
    }


register
    :  Def model view msg blend
    -> Adaptation model view msg blend
    -> Registry
    -> Registry
register def (Adaptation adaptation) registerAt =
    let
        adaptedDef = adapt (Adaptation adaptation) def
    in
        { registerAt
        | byId = \otherId ->
            if otherId == def.id
                then Just adaptedDef
                else registerAt.byId otherId
        , byModel = \model ->
            case adaptation.extractModel model of
                Just _ -> Just adaptedDef
                _ -> registerAt.byModel model
        , byMsg = \msg ->
            case adaptation.extractMsg msg of
                Just _ -> Just adaptedDef
                _ -> registerAt.byMsg msg
        }


htmlAdaptation
    :  (model -> Model)
    -> (msg -> Msg)
    -> (Model -> Maybe model)
    -> (Msg -> Maybe msg)
    -> Adaptation model (Html msg) msg Html.Blend
htmlAdaptation
    convertModel
    convertMsg
    extractModel
    extractMsg =
    (Adaptation
            { convertModel = convertModel
            , convertMsg = convertMsg
            , extractModel = extractModel
            , extractMsg = extractMsg
            , convertView = (\htmlView ->
                Html.map convertMsg htmlView
                    |> ToHtml)
            , extractBlend =
                extractHtmlBlend
            }
        )


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


alterWebGlBlend : WebGL.BlendChange -> Layer -> Layer
alterWebGlBlend changeF =
    alterBlend
        (\blend ->
            case blend of
                ForWebGL wglBlend -> changeF wglBlend |> ForWebGL
                _ -> blend
        )

extractHtmlBlend : Blend -> Maybe Html.Blend
extractHtmlBlend blend =
    case blend of
        ForHtml htmlBlend -> Just htmlBlend
        _ -> Nothing


extractWebGLBlend : Blend -> Maybe WebGL.Blend
extractWebGLBlend blend =
    case blend of
        ForWebGL webGlBlend -> Just webGlBlend
        _ -> Nothing


adapt
     : Adaptation model view msg blend
    -> Def model view msg blend
    -> Def Model View Msg Blend
adapt
    (Adaptation a)
    source =
    let
        adaptUpdateTuple f =
            f |> Tuple.mapFirst a.convertModel
              |> Tuple.mapSecond (Cmd.map a.convertMsg)
    in
        { id = source.id
        , kind = source.kind
        , init = adaptUpdateTuple << source.init
        , encode =
            \ctx layerModel ->
                case a.extractModel layerModel of
                    Just m -> source.encode ctx m
                    Nothing -> E.string <| "wrong encoder for " ++ source.id
        , decode = D.map a.convertModel << source.decode
        , update =
            \ctx mainMsg layerModel ->
                case ( a.extractMsg mainMsg, a.extractModel layerModel ) of
                    ( Just msg, Just model ) ->
                        adaptUpdateTuple <|
                            source.update ctx msg model
                    _ -> -- FIXME: return Maybe/Result for the case when message / model doesn't match
                        adaptUpdateTuple <| source.init ctx
        , view =
            \ctx maybeBlend layerModel  ->
                case a.extractModel layerModel of
                    Just model ->
                        a.convertView
                            <| source.view
                                ctx
                                (a.extractBlend <| Maybe.withDefault NoBlend maybeBlend)
                                model
                    Nothing -> ToHtml <| H.div [] []
        , subscribe =
            \ctx layerModel ->
                case a.extractModel layerModel of
                    Just model ->
                        source.subscribe ctx model
                            |> Sub.map (Tuple.mapSecond a.convertMsg)
                    Nothing -> Sub.none
        , gui = Nothing -- FIXME
        }


registry : Registry
registry =
    { byId = always Nothing
    , byModel = always Nothing
    , byMsg = always Nothing
    }

    |> register Background.def
        (htmlAdaptation
            Background
            BackgroundMsg
            (\model ->
                case model of
                    Background bgModel -> Just bgModel
                    _ -> Nothing)
            (\msg ->
                case msg of
                    BackgroundMsg bgMsg -> Just bgMsg
                    _ -> Nothing)
        )

    |> register Cover.def
        (htmlAdaptation
            Cover
            CoverMsg
            (\model ->
                case model of
                    Cover coverModel -> Just coverModel
                    _ -> Nothing)
            (\msg ->
                case msg of
                    CoverMsg coverMsg -> Just coverMsg
                    _ -> Nothing)
        )
