module Model.Layer exposing (..)

import Model.Html.Blend as HtmlBlend
import Model.WebGL.Blend as WGLBlend

import Layer.Canvas as Canvas
-- import Layer.Cover as Cover
import Layer.FSS as FSS
import Layer.Lorenz as Lorenz
import Layer.Fractal as Fractal
import Layer.Voronoi as Voronoi
import Layer.Template as Template
import Layer.Vignette as Vignette
import Layer.Metaballs as Metaballs
import Layer.Fluid as Fluid


type alias CreateLayer = LayerKind -> LayerModel -> Maybe Layer


type alias LayerIndex = Int


type LayerKind
    = Lorenz
    | Fractal
    | Template
    | Canvas
    | Voronoi
    | Fss
    | MirroredFss
    | Cover
    | Vignette
    | Metaballs
    | Fluid


-- type LayerBlend
--     = WGLB WGLBlend.Blend
--     | HTMLB HtmlBlend.Blend


type LayerModel
    = LorenzModel Lorenz.Model
    | FractalModel Fractal.Model
    | VoronoiModel Voronoi.Model
    | FssModel FSS.Model
    | TemplateModel Template.Model
    | VignetteModel Vignette.Model
    | MetaballsModel Metaballs.Model
    | CanvasModel Canvas.Model
    | CoverModel {}
    | FluidModel Fluid.Model

-- FIXME: Cover module needs Model module and so by importing it we form the cycle reference

type WebGLLayer_
    = LorenzLayer Lorenz.Mesh
    | FractalLayer Fractal.Mesh
    | VoronoiLayer Voronoi.Mesh
    | TemplateLayer Template.Mesh
    | FluidLayer Fluid.Mesh
    | FssLayer (Maybe FSS.SerializedScene) FSS.Mesh
    | MirroredFssLayer (Maybe FSS.SerializedScene) FSS.Mesh
    | VignetteLayer


type HtmlLayer_
    = CoverLayer
    | MetaballsLayer
    | CanvasLayer
    | NoContent -- TODO: get rid of `NoContent`?


type Layer
    = WebGLLayer WebGLLayer_ WGLBlend.Blend
    | HtmlLayer HtmlLayer_ HtmlBlend.Blend


-- `change` is needed since we store a sample layer model
-- to use for any layer in the main model
type alias LayerDef =
    { kind : LayerKind
    , name : String
    , layer : Layer
    , model : LayerModel
    , on : Bool
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


emptyLayer : Layer
emptyLayer =
    HtmlLayer NoContent HtmlBlend.default


initLayerModel : LayerKind -> LayerModel
initLayerModel kind =
    case kind of
        Lorenz -> LorenzModel Lorenz.init
        Fractal -> FractalModel Fractal.init
        Template -> TemplateModel Template.init
        Canvas -> CanvasModel Canvas.init
        Voronoi -> VoronoiModel Voronoi.init
        Fss -> FssModel FSS.init
        MirroredFss -> FssModel FSS.init
        Cover -> CoverModel {}
        Vignette -> VignetteModel Vignette.init
        Metaballs -> MetaballsModel Metaballs.init
        Fluid -> FluidModel Fluid.init


encodeKind : LayerKind -> String
encodeKind kind =
    case kind of
        Fss -> "fss"
        MirroredFss -> "fss-mirror"
        Lorenz -> "lorenz"
        Fractal -> "fractal"
        Template -> "template"
        Canvas -> "canvas"
        Voronoi -> "voronoi"
        Cover -> "cover"
        Vignette -> "vignette"
        Metaballs -> "metaballs"
        Fluid -> "fluid"


decodeKind : String -> Result String LayerKind
decodeKind layerTypeStr =
    case layerTypeStr of
        "fss" -> Ok Fss
        "fss-mirror" -> Ok MirroredFss
        "lorenz" -> Ok Lorenz
        "fractal" -> Ok Fractal
        "template" -> Ok Template
        "voronoi" -> Ok Voronoi
        "cover" -> Ok Cover
        "vignette" -> Ok Vignette
        "metaballs" -> Ok Metaballs
        "fluid" -> Ok Fluid
        _ -> Err layerTypeStr
