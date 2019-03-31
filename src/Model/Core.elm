module Model.Core exposing
    ( Msg(..)
    , Model
    , PortModel
    , init
    , initEmpty
    , CreateGui
    , hasErrors, addError, addErrors
    , TimeDelta, Pos, Size
    )

import Model.Layer exposing (..)
import Model.AppMode exposing (..)
import Model.Error exposing (..)
import Model.SizeRule exposing (..)
import Model.Product as Product exposing (Product)
import Model.Product
import Model.WebGL.Blend as WGLBlend
import Model.Html.Blend as HtmlBlend

import Layer.FSS as FSS
import Layer.Metaballs as Metaballs

import Gui.Gui as Gui


type alias TimeNow = Float
type alias TimeDelta = Float


type alias Pos = (Int, Int)
type alias Size = (Int, Int)
type alias CreateGui = Model -> Gui.Model Msg


type Msg
    = Bang
    | ChangeMode UiMode
    | ChangeModeAndResize UiMode SizeRule
    | Animate TimeDelta
    | GuiMessage (Gui.Msg Msg)
    | Resize SizeRule
    | RequestFitToWindow
    | Locate Pos
    | Rotate Float
    | Import String
    | Export
    | ExportZip
    | TimeTravel Float
    | BackToNow
    | Pause
    | Continue
    | TriggerPause
    | HideControls
    | ChangeProduct Product
    | TurnOn LayerIndex
    | TurnOff LayerIndex
    | MirrorOn LayerIndex
    | MirrorOff LayerIndex
    | Configure LayerIndex LayerModel
    | ChangeWGLBlend LayerIndex WGLBlend.Blend
    | AlterWGLBlend LayerIndex WGLBlend.BlendChange
    | ChangeHtmlBlend LayerIndex HtmlBlend.Blend
    | RebuildFss LayerIndex FSS.SerializedScene
    --| RebuildOnClient LayerIndex FSS.SerializedScene
    | ChangeFssRenderMode LayerIndex FSS.RenderMode
    | ChangeFaces LayerIndex FSS.Faces
    | AlterFaces LayerIndex FSS.FacesChange
    | ChangeLightSpeed LayerIndex Int
    | ChangeVignette LayerIndex FSS.Vignette
    | ChangeIris LayerIndex FSS.Iris
    | AlterAmplitude LayerIndex FSS.AmplitudeChange
    | ShiftColor LayerIndex FSS.ColorShiftPatch
    | ChangeOpacity LayerIndex FSS.Opacity
    | RebuildMetaballs LayerIndex Metaballs.Model
    | Randomize
    | ApplyRandomizer PortModel
    | SavePng
    | AddError Error
    | AddErrors Errors
    | NoOp


type alias Model = -- TODO: Result Error { ... }
    { background: String
    , mode : UiMode
    , gui : Maybe (Gui.Model Msg)
    , paused : Bool
    , autoRotate : Bool
    , fps : Int
    , theta : Float
    , omega : Float
    , layers : List LayerDef
    , size : SizeRule
    , origin : Pos
    , mouse : Pos
    , now : TimeNow
    , timeShift : TimeDelta
    , product : Product
    , controlsVisible : Bool
    , errors: Errors
    -- voronoi : Voronoi.Config
    -- fractal : Fractal.Config
    -- , lights (taken from product)
    -- , material TODO
    }


type alias PortModel =
    { background : String
    , layers : List PortLayerDef
    , mode : String
    , mouse : ( Int, Int )
    , now : Float
    , origin : (Int, Int)
    , size : (Int, Int)
    , sizeRule : Maybe String
    , theta : Float
    , omega : Float
    , product : String
    , palette : List String
    }


init
    :  UiMode
    -> List ( LayerKind, String, LayerModel )
    -> CreateLayer
    -> CreateGui
    -> Model
init uiMode initialLayers createLayer createGui =
    let
        emptyModel = initEmpty uiMode
        modelWithLayers =
            emptyModel
                |> replaceLayers initialLayers createLayer
    in
        { modelWithLayers
        | gui =
            case uiMode of
                TronUi innerUiMode ->
                    Just <| createGui { modelWithLayers | mode = innerUiMode }
                _ -> Nothing
        }


initEmpty : UiMode -> Model
initEmpty mode =
    { background = "#171717"
    , mode = mode
    , gui = Nothing
    , paused = False
    , autoRotate = False
    , fps = 0
    , theta = 0.1
    , omega = 0.0
    , layers = []
    , size = Dimensionless
    , origin = ( 0, 0 )
    , mouse = ( 0, 0 )
    , now = 0.0
    , timeShift = 0.0
    --, range = ( 0.8, 1.0 )
    , product = Product.JetBrains
    , controlsVisible = True
    , errors = Errors [ ]
    }


hasErrors : Model -> Bool
hasErrors model =
    case model.errors of
        Errors errorsList ->
            not <| List.isEmpty errorsList


addError : String -> Model -> Model
addError errorStr =
    addErrors (errorStr |> List.singleton |> Errors)


addErrors : Errors -> Model -> Model
addErrors (Errors newErrors) model =
    { model
    | errors =
        case model.errors of
            Errors currentErrors ->
                Errors <| currentErrors ++ newErrors
    }


noticeResult : (a -> Model -> Model) -> (x -> String) -> Result x a -> Model -> Model
noticeResult addValue errorToString result model =
    case result of
        Ok v -> model |> addValue v
        Err error -> model |> addError (errorToString error)


noticeResult_ : (a -> Model -> Model) -> (x -> String) -> Result (List x) a -> Model -> Model
noticeResult_ addValue errorToString result model =
    case result of
        Ok v -> model |> addValue v
        Err errors -> model |> addErrors (errors |> List.map errorToString |> Errors)


tryToCreateLayers
     : List ( LayerKind, String, LayerModel )
    -> CreateLayer
    -> Result (List ( Int, LayerKind, String )) (List LayerDef)
tryToCreateLayers source createLayer =
    source
        |> List.indexedMap
            (\index (kind, name, layerModel) ->
                layerModel
                    |> createLayer kind
                    |> Maybe.map
                        (\layer ->
                            { kind = kind
                            , layer = layer
                            , name = name
                            , model = layerModel
                            , on = True
                            }
                        )
                    |> Result.fromMaybe (index, kind, name)
            )
        |> List.foldl
            (\layerResult mainResult ->
                case ( mainResult, layerResult ) of
                    ( Ok allDefs, Ok layerDef ) -> Ok <| layerDef :: allDefs
                    ( Ok _, Err layerError ) -> Err <| List.singleton layerError
                    ( Err allErrors, Ok _ ) -> Err allErrors
                    ( Err allErrors, Err layerError ) -> Err <| layerError :: allErrors
            )
            (Ok [])


replaceLayers
     : List ( LayerKind, String, LayerModel )
    -> CreateLayer
    -> Model
    -> Model
replaceLayers source createLayer model =
    model
        |> (tryToCreateLayers source createLayer |>
                noticeResult_
                    (\layers model_ -> { model_ | layers = layers })
                    (\(index, kind, name) ->
                        "Failed to create layer: (" ++ String.fromInt index ++ ", "
                            ++ encodeKind kind ++ ") "
                            ++ name))
