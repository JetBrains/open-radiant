port module Main exposing (main)

import Browser
import Url exposing (..)
import Browser.Navigation as Nav

import Array exposing (Array)
import Task exposing (Task)

import Browser.Dom as Browser
import Browser.Events as Browser
import Browser.Navigation as Browser

import Html exposing (Html, text, div, span, input, canvas)
import Html.Attributes as H
    exposing (class, width, height, style, class, type_, min, max, value, id)
-- import Html.Events exposing (on, onInput, onMouseUp, onClick)
import Html.Events as Events exposing (onInput)
import Json.Decode as D
import Json.Encode as E

import Algorithm.Gaussian as Gaussian

import WebGL exposing (Mesh, Option)
import WebGL.Settings.Blend as B
import WebGL.Settings exposing (sampleAlphaToCoverage)
import WebGL.Settings.DepthTest as DepthTest

import Model.Core exposing (..)
import Model.Core as Model exposing (init)
import Model.AppMode exposing (..)
import Model.AppMode as Mode exposing (decode, encode)
import Model.Product exposing (Product)
import Model.Product as Product
import Model.Constants exposing (..)
import Model.Layer exposing (..)
import Model.SizeRule exposing (..)
import Model.SizeRule as SizeRule exposing (decode, encode, toRecord)
import Model.Error exposing (..)
import Model.ImportExport as IE
import Model.WebGL.Blend as WGLBlend
import Model.Html.Blend as HtmlBlend

import Gui.Gui as Gui
import Gui.Mouse exposing (Position)
import TronGui as Gui
import Viewport exposing (Viewport)
import RenderQueue as RQ

-- import Controls
import Navigation as Nav
import Gradient as Gradient

import Layer.Background as Background
import Layer.Background exposing (Mode(..), StopState(..))
import Layer.Lorenz as Lorenz
import Layer.Fractal as Fractal
import Layer.Voronoi as Voronoi
import Layer.FSS as FSS
import Layer.Template as Template
import Layer.Cover as Cover
import Layer.Canvas as Canvas
import Layer.Vignette as Vignette
import Layer.Metaballs as Metaballs
import Layer.NativeMetaballs as NativeMetaballs
import Layer.Fluid as Fluid
import Layer.FluidGrid as FluidGrid


initialMode : AppMode
initialMode = Production


type alias Flags = { forcedMode: Maybe String }


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = document
        , subscriptions = subscriptions
        , update = update
        , onUrlChange = Nav.onUrlChange
        , onUrlRequest = Nav.onUrlRequest
        }


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        mode =
            flags.forcedMode
                |> Maybe.map (Mode.decode >> Result.withDefault initialMode)
                |> Maybe.withDefault initialMode
        initialModel =
            Model.init
                    navKey
                    mode
                    (initialLayers mode)
                    createLayer
                    Gui.gui
        ( model, command ) =
            Nav.applyUrl url initialModel
                |> batchUpdate initialModel
    in
        ( model
        , Cmd.batch
            [ command
            , case model.size of
                Dimensionless ->
                    resizeToViewport
                _ ->
                    if hasFssLayers model
                        then rebuildAllFssLayersWith model
                        else Cmd.none
            ]
        )


initialLayers : AppMode -> List ( LayerKind, String, LayerModel )
initialLayers mode =
    let
        layers =
            [ ( Cover, "Cover", CoverModel Cover.init )
            , ( NativeMetaballs, "NativeMetaballs", NativeMetaballsModel NativeMetaballs.init )
            , ( Background, "Background"
              , BackgroundModel
                    { mode = StopStates On On Off
                    , opacity = 1.0
                    }
              )
            -- [ ( Fluid, "Fluid", FluidModel Fluid.init )
            -- [ ( Metaballs, "Metaballs", MetaballsModel Metaballs.init )
            -- [ ( FluidGrid, "FluidGrid", FluidGridModel FluidGrid.init )
            -- [ ( Fss, "Lower Layer", FssModel FSS.init )
            -- , ( Fss, "Mid Layer", FssModel FSS.init )
            -- , ( Fss, "Top layer"
            -- , let
            --       fssModel = FSS.init
            --   in
            --       { fssModel
            --       | renderMode = FSS.PartialLines
            --       , shareMesh = True
            --       } |> FssModel
            --  ), ( Cover, "Cover", CoverModel Cover.init )
            -- ]
            ]
    in
        layers
            |> List.filter (\(kind, _, _) ->
                case ( kind, mode ) of
                    ( Cover, Ads ) -> False
                    _ -> True
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of

        Bang ->
            ( model
            , Cmd.batch
                [ startGui
                    ( model |> IE.encodePortModel
                    , makeConstants
                    )
                , if hasMetaballLayers model
                    then generateAllMetaballs model
                    else Cmd.none
                , if hasFluidLayers model
                    then generateAllFluid model
                    else Cmd.none
                , if hasFluidGridLayers model
                    then generateAllFluidGrids model
                    else Cmd.none
                , if hasNativeMetaballsLayers model
                    then generateAllInitialNativeMetaballs model
                    else Cmd.none
                ]
            )

        ChangeMode mode ->
            let
                newModel =
                    Model.init model.navKey mode (initialLayers mode) createLayer Gui.gui
            in
                ( newModel
                , Cmd.batch
                    [ Mode.encode newModel.mode |> modeChanged
                    , resizeToViewport
                    , Nav.pushUrlFrom newModel
                    ]
                )

        ApplyUrl url ->
            let
                (newModel, command) = Nav.applyUrl url model
                    |> batchUpdate model
            in
                ( newModel
                , Cmd.batch
                    [ command
                    , case model.size of
                        Dimensionless ->
                            resizeToViewport
                        _ ->
                            if hasFssLayers model
                                then rebuildAllFssLayersWith model
                                else Cmd.none
                    ]
                )

        GuiMessage guiMsg ->
            case model.gui of
                Just gui ->
                    let
                        ( ( newModel, commands ), newGui ) =
                            gui |> Gui.update update model guiMsg
                    in
                        (
                            { newModel
                            | gui = Just newGui
                            }
                        , commands
                        )
                Nothing -> ( model, Cmd.none )

        Animate dt ->
            (
                { model
                 | fps = floor (1000 / dt)
                 , theta = if not (model.autoRotate || model.paused)
                              then model.theta + (dt * model.omega) / 1000
                              else model.theta
                 , now = if not model.paused
                            then model.now + dt + model.timeShift
                            else model.now
                 }
            , Cmd.none
            )

        Pause ->
            ( { model | paused = True }
            , Cmd.none
            )

        Continue ->
            ( { model | paused = False }
            , Cmd.none
            )

        TriggerPause ->
            ( { model | paused = not model.paused }
            , Cmd.none
            )

        HideControls ->
            ( { model | controlsVisible = False }
            , Cmd.none
            )

        Import encodedModel ->
            case (encodedModel
                    |> IE.decodeModel model.navKey model.mode createLayer Gui.gui) of
                Ok decodedModel ->
                    ( decodedModel
                    , Cmd.batch
                        [ if hasFssLayers decodedModel
                            then rebuildAllFssLayersWith decodedModel
                            else Cmd.none
                        , if hasFluidLayers decodedModel
                            then
                                commandForAllFluidLayers
                                    (\layerIndex fluidModel ->
                                        buildFluidGradientTextures
                                            ( layerIndex
                                            , fluidModel
                                                |> FluidModel |> IE.encodeLayerModel decodedModel.product
                                            )
                                    )
                                    decodedModel
                            else Cmd.none
                        , Nav.pushUrlFrom decodedModel
                        ]
                    )
                Err importError ->
                    ( model |> addError importError
                    , Cmd.none
                    )

        Export ->
            ( model
            , model |> IE.encodeModel |> export_
            )

        ExportZip ->
            ( model
            , model |> IE.encodeModel |> exportZip_
            )

        TimeTravel timeShift ->
            (
                { model
                | timeShift = timeShift
                }
            , Cmd.none
            )

        BackToNow ->
            ( { model
              | timeShift = 0.0
              , now = model.now + model.timeShift
              , paused = True
              }
            , Cmd.none
            )

        Rotate omega ->
            ( { model | omega = omega  }
            , Cmd.none
            )

        Resize rule ->
            let
                -- _ = Debug.log "Resize: rule" rule
                ( width, height ) = getRuleSizeOrZeroes rule
                newModelWithSize =
                    { model
                    | size = rule
                    , origin = getOrigin ( width, height )
                    }
                    |> (\modelWithSize ->
                            if rule /= Dimensionless && hasFluidLayers modelWithSize
                            then remapAllFluidLayersToNewSize modelWithSize
                            else modelWithSize
                        )
            in
                ( newModelWithSize
                , Cmd.batch
                    [ newModelWithSize |> getSizeUpdate |> sizeChanged
                    , if hasFssLayers newModelWithSize
                        then rebuildAllFssLayersWith newModelWithSize
                        else Cmd.none
                    , requestWindowResize ( width, height )
                    , Nav.pushUrlFrom newModelWithSize
                    ]
                )

        RequestFitToWindow ->
            ( model, requestFitToWindow () )

        Locate (( x, y ) as pos) ->
            let
                modelWithMouse =
                    { model
                    | mouse = pos
                    }
                message = modelWithMouse.gui
                    |> Maybe.map
                        (\gui ->
                            Gui.moves gui { x = x, y = y } |> GuiMessage
                        )
                    |> Maybe.withDefault NoOp
            in
                update message modelWithMouse

        TurnOn index ->
            ( model |> updateLayerDef index
                (\def -> { def | on = True })
            , Cmd.none
            )

        TurnOff index ->
            ( model |> updateLayerDef index
                (\def -> { def | on = False })
            , Cmd.none
            )

        MirrorOn index ->
            ( model |> updateLayerDef index
                (\layerDef ->
                    case layerDef.layer of
                        WebGLLayer webglLayer blend ->
                            case webglLayer of
                                FssLayer maybeScene mesh ->
                                    { layerDef
                                    | layer =
                                        WebGLLayer
                                        (MirroredFssLayer maybeScene mesh)
                                        blend
                                    , kind = MirroredFss
                                    }
                                _ -> layerDef
                        _ -> layerDef
                )
            , Cmd.none
            )

        MirrorOff index ->
            ( model |> updateLayerDef index
                (\layerDef ->
                    case layerDef.layer of
                        WebGLLayer webglLayer blend ->
                            case webglLayer of
                                MirroredFssLayer maybeScene mesh ->
                                    { layerDef
                                    | layer =
                                        WebGLLayer
                                        (FssLayer maybeScene mesh)
                                        blend
                                    , kind = Fss
                                    }
                                _ -> layerDef
                        _ -> layerDef
                )
            , Cmd.none
            )

        ChangeProduct product ->
            let modelWithProduct = { model | product = product }
            in
                ( modelWithProduct
                , Cmd.batch
                    [ if hasFssLayers modelWithProduct
                        then rebuildAllFssLayersWith modelWithProduct
                        else Cmd.none
                    , if hasMetaballLayers modelWithProduct
                        then generateAllMetaballs modelWithProduct
                        else Cmd.none
                    , if hasFluidLayers model
                        then modelWithProduct
                            |> commandForAllFluidLayers
                                (\layerIndex fluidModel ->
                                    buildFluidGradientTextures
                                        ( layerIndex
                                        , fluidModel |> FluidModel |> IE.encodeLayerModel product
                                        )
                                )
                        else Cmd.none
                    -- , if hasNativeMetaballLayers modelWithProduct
                    --     then generateAllMetaballs modelWithProduct
                    --     else Cmd.none
                    , if hasNativeMetaballsLayers modelWithProduct
                        then updateAllNativeMetaballsWith modelWithProduct
                        else Cmd.none
                    , Nav.pushUrlFrom modelWithProduct
                    ]
                )

        Configure index _ ->
            ( model |> updateLayer index
                (\layer curLayerModel ->
                    case layer of
                        WebGLLayer webglLayer webglBlend ->
                            WebGLLayer
                            (case ( webglLayer, curLayerModel ) of
                                ( LorenzLayer _, LorenzModel lorenzModel ) ->
                                    LorenzLayer (lorenzModel |> Lorenz.build)
                                ( FractalLayer _, FractalModel fractalModel ) ->
                                    FractalLayer (fractalModel |> Fractal.build)
                                ( VoronoiLayer _, VoronoiModel voronoiModel ) ->
                                    VoronoiLayer (voronoiModel |> Voronoi.build)
                                ( FssLayer maybeScene _, FssModel fssModel ) ->
                                    let
                                        newMesh = maybeScene |> FSS.build fssModel
                                    in
                                        FssLayer maybeScene newMesh
                                ( MirroredFssLayer maybeScene _, FssModel fssModel ) ->
                                    let
                                        newMesh = maybeScene |> FSS.build fssModel
                                    in
                                        MirroredFssLayer maybeScene newMesh
                                ( TemplateLayer _, TemplateModel templateModel ) ->
                                    TemplateLayer (templateModel |> Template.build)
                                ( VignetteLayer, VignetteModel _ ) ->
                                    VignetteLayer
                                ( FluidLayer _, FluidModel fluidModel ) ->
                                    FluidLayer (fluidModel |> Fluid.build)
                                _ -> webglLayer)
                            webglBlend
                        _ -> layer)
            , Cmd.none
            )

        ChangeWGLBlend index newBlend ->
            ( model |> updateLayerBlend index
                (\_ -> Just newBlend)
                (\_ -> Nothing)
            , Cmd.none
            )

        AlterWGLBlend index changeF ->
            ( model |> updateLayerBlend index
                (\curBlend -> Just <| changeF curBlend)
                (\_ -> Nothing)
            , Cmd.none
            )

        ChangeHtmlBlend index newBlend ->
            ( model |> updateLayerBlend index
                (\_ -> Nothing)
                (\_ -> Just newBlend)
            , Cmd.none
            )

        ChangeFssRenderMode index renderMode ->
            -- ( model
            --     |> updateFss index
            --         (\fssModel -> { fssModel | renderMode = renderMode })
            -- , Cmd.none
            -- )
            model
                |> updateAndRebuildFssWith index
                    (\fssModel -> { fssModel | renderMode = renderMode })

        ChangeFaces index faces ->
            model
                |> updateAndRebuildFssWith index
                    (\fssModel -> { fssModel | faces = faces })

        AlterFaces index change ->
            model
                |> updateAndRebuildFssWith index
                    (\fss ->
                        let
                            current = fss.faces
                        in
                            { fss | faces =
                                FSS.Faces
                                    ( change.xChange |> Maybe.withDefault current.x )
                                    ( change.yChange |> Maybe.withDefault current.y )
                            }
                    )

        ChangeLightSpeed index lightSpeed ->
            model
                |> updateAndRebuildFssWith index
                    (\fssModel -> { fssModel | lightSpeed = lightSpeed })

        RebuildFss index serializedScene ->
            ( model |> updateLayer index
                (\layer layerModel ->
                    case layer of
                        WebGLLayer webglLayer webglBlend ->
                            case ( webglLayer, layerModel ) of
                                ( FssLayer _ mesh, FssModel fssModel ) ->
                                    let
                                        maybeScene = Just serializedScene
                                        newMesh = maybeScene |> FSS.build fssModel
                                    in
                                        WebGLLayer
                                        (FssLayer maybeScene newMesh)
                                        webglBlend
                                ( MirroredFssLayer _ mesh, FssModel fssModel ) ->
                                    let
                                        maybeScene = Just serializedScene
                                        newMesh = maybeScene |> FSS.build fssModel
                                    in
                                        WebGLLayer
                                        (MirroredFssLayer maybeScene newMesh)
                                        webglBlend
                                _ -> layer
                        _ -> layer
                )
            , Cmd.none
            )

        RebuildMetaballs index metaballsModel ->
            ( model |> updateMetaballsModel index metaballsModel
            , Cmd.none
            )

        UpdateNativeMetaballs index nativeMetaballsModel ->
            ( model |> updateNativeMetaballsModel index nativeMetaballsModel
            , updateNativeMetaballs
                { index = index
                , size = getRuleSizeOrZeroes model.size
                , palette = Product.getPalette model.product |> Product.encodePalette
                , layerModel = NativeMetaballsModel nativeMetaballsModel
                        |> IE.encodeLayerModel model.product
                }
            )

        RebuildFluid index fluidModel ->
            ( model |> updateFluidModel index fluidModel.groups
            , buildFluidGradientTextures
                ( index
                , IE.encodeLayerModel model.product <| FluidModel fluidModel
                )
            )

        RequestNewFluid index ->
            ( model
            , if hasFluidLayers model
                then generateAllFluid model -- FIXME: use actual index
                else Cmd.none
            )

        ChangeFluidVariety index value ->
            if hasFluidLayers model then
                let
                    newModel =
                        model |> updateLayerDef index
                            (\layerDef ->
                                case layerDef.model of
                                    FluidModel fModel ->
                                        { layerDef
                                        | model =
                                            FluidModel { fModel | variety = value }
                                        }
                                    _ -> layerDef
                            )
                in
                    ( newModel
                    , generateAllFluid newModel -- FIXME: use actual index
                    )
            else ( model, Cmd.none )

        ChangeFluidOrbit index value ->
            if hasFluidLayers model then
                let
                    newModel =
                        model |> updateLayerDef index
                            (\layerDef ->
                                case layerDef.model of
                                    FluidModel fModel ->
                                        { layerDef
                                        | model =
                                            FluidModel { fModel | orbit = value }
                                        }
                                    _ -> layerDef
                            )
                in
                    ( newModel
                    , generateAllFluid newModel -- FIXME: use actual index
                    )
            else ( model, Cmd.none )

        RequestNewFluidGrid index ->
            ( model
            , if hasFluidGridLayers model
                then generateAllFluidGrids model -- FIXME: use actual index
                else Cmd.none
            )

        RebuildFluidGrid index fluidGridModel ->
            ( model |> rebuildFluidGrid index fluidGridModel
            , Cmd.none
            )

        ChangeVignette index opacity ->
            ( model
                |> updateFss index
                    (\fssModel -> { fssModel | vignette = opacity })
            , Cmd.none
            )

        ChangeIris index iris ->
            ( model |> updateFss index
                (\fssModel -> { fssModel | iris = iris })
            , Cmd.none
            )

            -- model
            --     |> updateAndRebuildFssWith index
            --         (\fssModel -> { fssModel | vignette = vignette })

            -- ( model
            --     |> updateLayerWithItsModel
            --         index
            --         (\(layer, model) ->
            --             case ( layer, model ) of
            --                 ( WebGLLayer VignetteLayer _, VignetteModel vignetteModel ) ->
            --                     (layer, { vignetteModel | opacity = opacity } |> VignetteModel)
            --                 _ -> (layer, model)
            --         )
            -- , Cmd.none

        AlterAmplitude index change ->
            model
                |> updateAndRebuildFssWith index
                    (\fss ->
                        let
                            current = fss.amplitude
                        in
                            { fss | amplitude =
                                FSS.Amplitude
                                    ( change.xChange |> Maybe.withDefault current.amplitudeX )
                                    ( change.yChange |> Maybe.withDefault current.amplitudeY )
                                    ( change.zChange |> Maybe.withDefault current.amplitudeZ )
                            }
                    )


        ShiftColor index shift ->
            ( model |> updateFss index
                (\fss ->
                    let
                        current = fss.colorShift
                    in
                        { fss | colorShift =
                            FSS.ColorShift
                                ( shift.hueShift        |> Maybe.withDefault current.hue        )
                                ( shift.saturationShift |> Maybe.withDefault current.saturation )
                                ( shift.brightnessShift |> Maybe.withDefault current.brightness )
                        }
                )
            , Cmd.none
            )

        ChangeOpacity index newOpacity ->
            ( model |> updateFss index
                (\fssModel -> { fssModel | opacity = newOpacity })
            , Cmd.none
            )

        SavePng ->
            ( model
            , model |> getSizeUpdate |> triggerSavePng
            )

        Randomize ->
            ( model
            , model |> IE.encodePortModel |> requestRandomize
            -- TODO: updateUrl newModel?
            )

        ApplyRandomizer portModel ->
            case IE.decodePortModel model.navKey createLayer model.product portModel of
                Ok decodedPortModel ->
                    ( decodedPortModel
                    , if hasFssLayers decodedPortModel
                        then rebuildAllFssLayersWith decodedPortModel
                        else Cmd.none
                    )
                Err decodingErrors ->
                    ( model |> addErrors (IE.adaptModelDecodeErrors decodingErrors)
                    , Cmd.none
                    )

        AddError error ->
            ( model |> addError error
            , Cmd.none
            )

        AddErrors errors ->
            ( model |> addErrors errors
            , Cmd.none
            )

        LoadFluidGradientTextures layerIndex gradientUrls ->
            ( model
            , model
                -- FIXME: apply only to the layer by layer index
                |> getLayerModels (\kind -> if kind == Fluid then False else True)
                |> List.map (\layerModel ->
                    case layerModel of
                        FluidModel fluidModel ->
                            Fluid.loadTextures
                                gradientUrls
                                (getRuleSizeOrZeroes model.size)
                                fluidModel
                                (Fluid.packTextures >> ApplyFluidTextures layerIndex)
                                (\_ -> NoOp)
                        _ -> Cmd.none
                   )
                |> Cmd.batch
            )

        RegenerateFluidGradients layerIndex ->
            ( model
            , regenerateFluidGradients model
            )

        ApplyFluidTextures layerIndex textures ->
            ( model |> updateLayerWithItsModel
                0 -- FIXME: update to layerIndex when we will use it
                (\(layer, layerModel) ->
                    ( layer
                    , case layerModel of
                        FluidModel fluidModel ->
                            fluidModel
                                |> Fluid.injectTextures textures
                                |> FluidModel
                        _ -> layerModel
                    )
                )
            , Cmd.none
            )

        ChangeNativeMetaballsVariety index value ->
            if hasNativeMetaballsLayers model then
                let
                    newModel =
                        model |> updateLayerDef index
                            (\layerDef ->
                                case layerDef.model of
                                    NativeMetaballsModel nmModel ->
                                        { layerDef
                                        | model =
                                            NativeMetaballsModel { nmModel | variety = value }
                                        }
                                    _ -> layerDef
                            )
                in
                    ( newModel
                    , generateAllNativeMetaballs newModel -- FIXME: use actual index
                    )
            else ( model, Cmd.none )

        ChangeNativeMetaballsOrbit index value ->
            if hasNativeMetaballsLayers model then
                let
                    newModel =
                        model |> updateLayerDef index
                            (\layerDef ->
                                case layerDef.model of
                                    NativeMetaballsModel nmModel ->
                                        { layerDef
                                        | model =
                                            NativeMetaballsModel { nmModel | orbit = value }
                                        }
                                    _ -> layerDef
                            )
                in
                    ( newModel
                    , generateAllNativeMetaballs newModel -- FIXME: use actual index
                    )
            else ( model, Cmd.none )

        SwitchBackgroundStop layerIndex stopIndex value ->
            if hasBackgroundLayers model then
                let
                    newModel =
                        model |> updateLayerDef layerIndex
                            (\layerDef ->
                                case layerDef.model of
                                    BackgroundModel bgModel ->
                                        { layerDef
                                        | model =
                                            BackgroundModel
                                                { bgModel
                                                | mode = Background.switchStop
                                                            (Background.indexToStopId stopIndex)
                                                            (Background.boolToStopState value)
                                                            bgModel.mode
                                                }
                                        }
                                    _ -> layerDef
                            )
                in
                    ( newModel
                    , Cmd.none
                    )
            else ( model, Cmd.none )

        -- UpdateNativeMetaballs layerIndex ->
        --     ( model
        --     , NativeMetaballs.prepare model.product
        --         |> Tuple.pair layerIndex
        --         |> updateNativeMetaballs
        --     )

        NoOp -> ( model, Cmd.none )


batchUpdate : Model -> List Msg -> ( Model, Cmd Msg )
batchUpdate model messages =
    List.foldl
        (\msg ( prevModel, prevCmd ) ->
            update msg prevModel
                |> Tuple.mapSecond
                    (\newCmd -> Cmd.batch [ prevCmd, newCmd ])
        )
        ( model, Cmd.none )
        messages


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ bang (\_ -> Bang)
        , Browser.onAnimationFrameDelta Animate
        , Browser.onResize <| \w h -> Resize <| UseViewport <| ViewportSize w h
        -- , clicks (\pos ->
        --     fits model.size pos
        --         |> Maybe.map (\pos -> Pause)
        --         |> Maybe.withDefault NoOp
        --   )
        , Sub.map (\{ x, y } ->
                (x, y)
                    -- |> fits (getRuleSizeOrZeroes model.size)
                    |> ensurePositive
                    |> Maybe.map (\localPos -> Locate localPos)
                    |> Maybe.withDefault NoOp
            )
            <| Browser.onMouseMove
            <| decodeMousePosition
        --, downs <| Gui.downs >> GuiMessage
        , Sub.map (tellGui Gui.downs model)
            <| Browser.onMouseDown
            <| decodeMousePosition
        , Sub.map (tellGui Gui.ups model)
            <| Browser.onMouseUp
            <| decodeMousePosition
        , rotate Rotate
        , changeProduct
            (\productStr ->
                case Product.decode productStr of
                    Ok product -> ChangeProduct product
                    Err error -> AddError <| "Failed to decode product: " ++ error)
        , changeFssRenderMode (\{value, layer} ->
            FSS.decodeRenderMode value |> ChangeFssRenderMode layer)
        , changeFacesX (\{value, layer} ->
            case model |> getLayerModel layer of
                Just (FssModel { faces }) ->
                    ChangeFaces layer { x = value, y = faces.y }
                _ -> NoOp
          )
        , changeFacesY (\{value, layer} ->
            case model |> getLayerModel layer of
                Just (FssModel { faces }) ->
                    ChangeFaces layer { x = faces.x, y = value }
                _ -> NoOp
          )
        , changeLightSpeed (\{value, layer} -> ChangeLightSpeed layer value)
        , changeAmplitude (\{value, layer} -> AlterAmplitude layer value)
        , shiftColor (\{value, layer} -> ShiftColor layer value)
        , changeOpacity (\{value, layer} -> ChangeOpacity layer value)
        , changeVignette (\{value, layer} -> ChangeVignette layer value)
        , changeIris (\{value, layer} -> ChangeIris layer value)
        , changeMode
            (\modeStr ->
                case Mode.decode modeStr of
                    Ok mode -> ChangeMode mode
                    Err error -> AddError <| "Failed to decode mode: " ++ error
            )
        , resize
            (\{ presetCode, viewport } ->
                case viewport of
                    ( vw, vh ) ->
                        presetCode
                            |> Maybe.andThen decodePreset
                            |> Maybe.map (Resize << FromPreset)
                            |> Maybe.withDefault (Resize <| UseViewport <| ViewportSize vw vh )
            )
        , changeWGLBlend (\{ layer, value } ->
            ChangeWGLBlend layer value
          )
        , changeHtmlBlend (\{ layer, value } ->
            ChangeHtmlBlend layer <| HtmlBlend.decode value
          )
        , configureLorenz (\{ layer, value } ->
            Configure layer (LorenzModel value)
          )
        , configureFss (\{ layer, value } ->
            IE.fromFssPortModel value |> FssModel |> Configure layer
          )
        , rebuildFss (\{ layer, value } ->
            RebuildFss layer value
          )
        , loadFluidGradientTextures (\{ layer, value } ->
            value
                |> List.map Fluid.Base64Url
                |> LoadFluidGradientTextures layer
          )
        , requestRegenerateFluidGradients
            (\{ layer } -> RegenerateFluidGradients layer)
        -- , requestUpdateNativeMetaballs
        --     (\{ layer } -> UpdateNativeMetaballs layer)
        , refreshFluid
            (\{ layer } -> RequestNewFluid layer)
        , changeFluidVariety
            (\{ layer, value } ->
                ChangeFluidVariety layer (Gaussian.Variety value))
        , changeFluidOrbit
            (\{ layer, value } -> ChangeFluidOrbit layer (Fluid.Orbit value))
        , changeNativeMetaballsVariety
            (\{ layer, value } ->
                ChangeNativeMetaballsVariety layer (Gaussian.Variety value))
        , changeNativeMetaballsOrbit
            (\{ layer, value } -> ChangeNativeMetaballsOrbit layer (Fluid.Orbit value))
        , switchBackgroundStop
            (\{ layer, stopIndex, value } ->
                SwitchBackgroundStop layer stopIndex value)
        , applyRandomizer ApplyRandomizer
        , import_ Import
        , pause (\_ -> Pause)
        , continue (\_ -> Continue)
        , triggerPause (\_ -> TriggerPause)
        , hideControls (\_ -> HideControls)
        , turnOn TurnOn
        , turnOff TurnOff
        , mirrorOn MirrorOn
        , mirrorOff MirrorOff
        , savePng (\_ -> SavePng)
        ]


view : Model -> Html Msg
view model =
    let
        ( w, h ) =
                getRuleSize model.size |> Maybe.withDefault ( -1, -1 )
        visible = w > 0 && h > 0
        wrapHtml =
            div
                [ H.class "html-layers", H.class "layers"
                , Events.onClick TriggerPause
                ]
        wrapEntities =
            WebGL.toHtmlWith
                --[ WebGL.antialias
                [ WebGL.alpha True
                , WebGL.clearColor 0.0 0.0 0.0 1.0
                --, WebGL.depth 0.5
                ]
                [ H.class "webgl-layers", H.class "layers"
                , width w, height h
                --, style "transform" "scale(0.5)"
                , style "display" (if visible then "block" else "none")
                , Events.onClick TriggerPause
                ]
        renderQueue = model |> RQ.groupLayers layerToEntities layerToHtml
        isInPlayerMode =
            case model.mode of
                Player -> True
                _ -> False
    in div
        [ H.class <| "mode-" ++ Mode.encode model.mode ]
        [ if not isInPlayerMode then canvas [ H.id "js-save-buffer" ] [ ] else div [] []
        , if hasErrors model
            then
                div [ H.id "error-pane", H.class "has-errors" ]
                    ( case model.errors of
                        Errors errorsList ->
                            errorsList |> List.map (\err -> span [] [ text err ])
                    )
            else div [ H.id "error-pane" ] []
        , renderQueue |> RQ.apply wrapHtml wrapEntities
        , if model.controlsVisible && not isInPlayerMode
            then ( div
                ([ "overlay-panel", "import-export-panel", "hide-on-space" ] |> List.map H.class)
                [ div [ H.class "timeline-holder" ]
                    [ span [ H.class "label past"] [ text "past" ]
                    , input
                        [ type_ "range"
                        , class "timeline"
                        , H.min "0"
                        , H.max "100"
                        , extractTimeShift model.timeShift |> H.value
                        , Events.onInput (\v -> adaptTimeShift v |> TimeTravel)
                        , Events.onMouseUp BackToNow
                        ]
                        []
                    , span [ H.class "label", H.class "future" ] [ text "future" ]
                    ]
                -- , input [ type_ "button", id "import-button", value "Import" ] [ text "Import" ]
                -- , input [ type_ "button", onClick Export, value "Export" ] [ text "Export" ]
                , input
                    [ type_ "button", class "export_html5"
                    , Events.onClick ExportZip, value "warp in html5" ]
                    [ text "Export to html5.zip" ]
                , input
                    [ type_ "button", class "export_png"
                    , Events.onClick SavePng, value "blast to png" ]
                    [ text "Export to png" ]
                , div [ H.class "spacebar_info" ] [ text "spacebar to hide controls, click to pause" ]
                ]
            ) else div [] []
        , model.gui
            |> Maybe.map Gui.view
            |> Maybe.map (Html.map GuiMessage)
            |> Maybe.map (\guiLayer -> div [ H.class "hide-on-space" ] [ guiLayer ])
            |> Maybe.withDefault (div [] [])
        ]


document : Model -> Browser.Document Msg
document model =
    { title = "Elmsfeuer, Radiant"
    , body = [ view model ]
    }


getSizeUpdate : Model -> SizeUpdate
getSizeUpdate model =
    { size = getRuleSize model.size |> Maybe.withDefault ( -1, -1 )
    , sizeRule = SizeRule.encode model.size
    , product = Product.encode model.product
    -- , coverSize = Product.getCoverTextSize model.product
    , background = model.background
    , sizeConstant = -1
    -- , mode = encodeMode model.mode
    }


-- for the cases when navigation by URL contains no size data, but requests a resize
-- fallbackToViewport : Model -> Msg -> Msg
-- fallbackToViewport model msg =
--     case msg of
--         Resize Dimensionless ->
--             Resize (UseViewport model.size)
--         _ -> msg


tellGui : (Gui.Model Msg -> a -> Gui.Msg Msg) -> Model -> a -> Msg
tellGui f model =
    model.gui
        |> Maybe.map (\gui -> f gui >> GuiMessage)
        |> Maybe.withDefault (always NoOp)


decodeMousePosition : D.Decoder Position
decodeMousePosition =
    D.map2 Position
        (D.at [ "offsetX" ] D.int)
        (D.at [ "offsetY" ] D.int)


ensureFits : (Int, Int) -> Pos -> Maybe Pos
ensureFits (width, height) (x, y) =
    if (x <= width) && (y <= height)
    then Just (x, y) else Nothing


ensurePositive : Pos -> Maybe Pos
ensurePositive (x, y) =
    if (x > 0) && (y > 0)
    then Just (x, y) else Nothing


-- mapControls : Model -> Controls.Msg -> Msg
-- mapControls model controlsMsg =
--     case controlsMsg of
--         Controls.Configure cfg -> Configure 0 (LorenzModel cfg)
--         Controls.Rotate th -> Rotate th


layerToHtml : Model -> Viewport {} -> Int -> LayerDef -> Html Msg
layerToHtml model viewport index layerDef =
    case layerDef.layer of
        HtmlLayer htmlLayer htmlBlend ->
            case ( htmlLayer, layerDef.model ) of
                ( BackgroundLayer, BackgroundModel bgModel ) ->
                    Background.view viewport (Product.getPalette model.product) bgModel
                ( CoverLayer, _ ) ->
                    Cover.view
                        model.mode
                        model.product
                        (getRuleSizeOrZeroes model.size)
                        model.origin
                        htmlBlend
                ( MetaballsLayer, MetaballsModel metaballsModel ) ->
                    Metaballs.view viewport model.now model.timeShift model.mouse metaballsModel
                ( NativeMetaballsLayer, NativeMetaballsModel nativeMetaballsModel ) ->
                    NativeMetaballs.view htmlBlend nativeMetaballsModel
                ( FluidGridLayer, FluidGridModel fluidGridModel ) ->
                    FluidGrid.view fluidGridModel
                ( CanvasLayer, _ ) ->
                    Canvas.view
                _ -> div [] []
        _ -> div [] []


layerToEntities : Model -> Viewport {} -> Int -> LayerDef -> List WebGL.Entity
layerToEntities model viewport index layerDef =
    case layerDef.layer of
        WebGLLayer webglLayer blend ->
            case ( webglLayer, layerDef.model ) of
                ( LorenzLayer mesh, _ ) ->
                    [ Lorenz.makeEntity
                        viewport
                        [ DepthTest.default, WGLBlend.produce blend ]
                        mesh
                    ]
                ( FractalLayer mesh, _ ) ->
                    [ Fractal.makeEntity
                        viewport
                        [ DepthTest.default, WGLBlend.produce blend ]
                        mesh
                    ]
                ( TemplateLayer mesh, _ ) ->
                    [ Template.makeEntity
                        viewport
                        [ DepthTest.default, WGLBlend.produce blend ]
                        mesh
                    ]
                ( FluidLayer mesh, FluidModel fluidModel ) ->
                    Fluid.makeEntities
                        model.now
                        model.mouse
                        viewport
                        fluidModel
                        [ DepthTest.default, WGLBlend.produce blend ]
                        mesh
                ( VoronoiLayer mesh, _ ) ->
                    [ Voronoi.makeEntity
                        viewport
                        [ DepthTest.default, WGLBlend.produce blend ]
                        mesh
                    ]
                ( FssLayer serialized mesh, FssModel fssModel ) ->
                    let
                        ( maybeBorrowedSerialized, maybeBorrowedMesh ) =
                            ( serialized, mesh )
                            -- if (fssModel.shareMesh) then
                            --     model |>
                            --         findTopAndGet
                            --             (\layer otherIndex ->
                            --                 if otherIndex < index then
                            --                     case layer of
                            --                         FssLayer _ otherSerialized otherMesh ->
                            --                             Just ( otherSerialized, otherMesh )
                            --                         MirroredFssLayer _ otherSerialized otherMesh ->
                            --                             Just ( otherSerialized, otherMesh )
                            --                         _ -> Nothing
                            --                 else Nothing
                            --             )
                            --             ( serialized, mesh )
                            -- else ( serialized, mesh )
                    in
                        [ FSS.makeEntity
                            model.now
                            (case model.mouse of ( x, y ) -> { x = x, y = y })
                            (model.product |> Product.getId)
                            index
                            viewport
                            fssModel
                            maybeBorrowedSerialized
                            [ DepthTest.default, WGLBlend.produce blend, sampleAlphaToCoverage ]
                            maybeBorrowedMesh -- seems this mesh is already built with "Triangles", so there's no sense in reusing it
                        ]
                ( MirroredFssLayer serialized mesh, FssModel fssModel ) ->
                    let
                        -- TODO: store clip position in the layer
                        fssModel1 =
                            { fssModel
                            | clip = Just { x = 0.0, y = FSS.defaultMirror }
                            , mirror = True
                            }
                        fssModel2 =
                            { fssModel
                            | clip = Just { x = FSS.defaultMirror, y = 1.0 }
                            }
                    in
                        layerToEntities model viewport index
                            { layerDef
                            | layer = WebGLLayer (FssLayer serialized mesh) blend
                            , model = FssModel fssModel1
                            } ++
                        layerToEntities model viewport index
                            { layerDef
                            | layer = WebGLLayer (FssLayer serialized mesh) blend
                            , model = FssModel fssModel2
                            }
                ( VignetteLayer, _ ) ->
                    [ Vignette.makeEntity
                        viewport
                        Vignette.init
                        [ DepthTest.default, WGLBlend.produce blend, sampleAlphaToCoverage ]
                    ]
                _ -> []
        _ -> []


resizeToViewport =
    Task.perform
        (\{ viewport } ->
            Resize
                <| UseViewport
                <| ViewportSize (floor viewport.width) (floor viewport.height))
        Browser.getViewport

-- TODO: The functions below belong to the specific layers. move them to `Model/Layer?`


hasMetaballLayers : Model -> Bool
hasMetaballLayers model
    = True -- FIXME: implement, think that on Bang (where it is called) initialLayers could not exist yet


hasNativeMetaballsLayers : Model -> Bool
hasNativeMetaballsLayers model
    = True -- FIXME: implement, think that on Bang (where it is called) initialLayers could not exist yet


hasFluidLayers : Model -> Bool
hasFluidLayers model
    = True -- FIXME: implement, think that on Bang (where it is called) initialLayers could not exist yet


hasFssLayers : Model -> Bool
hasFssLayers model
    = True -- FIXME: implement, think that on Bang (where it is called) initialLayers could not exist yet


hasFluidGridLayers : Model -> Bool
hasFluidGridLayers model
    = True -- FIXME: implement, think that on Bang (where it is called) initialLayers could not exist yet


hasBackgroundLayers : Model -> Bool
hasBackgroundLayers model
    = True -- FIXME: implement, think that on Bang (where it is called) initialLayers could not exist yet


isFluidLayer : LayerDef -> Maybe Fluid.Model
isFluidLayer layerDef =
    case layerDef.model of
        FluidModel fluidModel -> Just fluidModel
        _ -> Nothing


isLayerFss : LayerDef -> Maybe FSS.Model
isLayerFss layerDef =
    case layerDef.model of
        FssModel fssModel -> Just fssModel
        _ -> Nothing


isFluidGridLayer : LayerDef -> Maybe FluidGrid.Model
isFluidGridLayer layerDef =
    case layerDef.model of
        FluidGridModel fluidGridModel -> Just fluidGridModel
        _ -> Nothing


isMetaballLayer : LayerDef -> Maybe Metaballs.Model
isMetaballLayer layerDef =
    case layerDef.model of
        MetaballsModel metaballsModel -> Just metaballsModel
        _ -> Nothing


isNativeMetaballsLayer : LayerDef -> Maybe NativeMetaballs.Model
isNativeMetaballsLayer layerDef =
    case layerDef.model of
        NativeMetaballsModel nativeMetaballsModel -> Just nativeMetaballsModel
        _ -> Nothing


updateMetaballsModel : LayerIndex -> Metaballs.Model -> Model -> Model
updateMetaballsModel index newMetaballsModel model =
    model |> updateLayerWithItsModel
        index
        (\(layer, layerModel) ->
            -- FIXME: why update model in two places??
            ( layer
            , case layerModel of
                MetaballsModel _ -> MetaballsModel newMetaballsModel
                _ -> layerModel
            )
        )


updateNativeMetaballsModel : LayerIndex -> NativeMetaballs.Model -> Model -> Model
updateNativeMetaballsModel index newNativeMetaballsModel model =
    model |> updateLayerWithItsModel
        index
        (\(layer, layerModel) ->
            -- FIXME: why update model in two places??
            ( layer
            , case layerModel of
                NativeMetaballsModel _ -> NativeMetaballsModel newNativeMetaballsModel
                _ -> layerModel
            )
        )


updateFluidModel : LayerIndex -> List Fluid.BallGroup -> Model -> Model
updateFluidModel index newGroups model =
    model |> updateLayerWithItsModel
        index
        (\(layer, layerModel) ->
            -- FIXME: why update model in two places??
            ( layer
            , case layerModel of
                FluidModel currentFluidModel ->
                    FluidModel
                        { currentFluidModel
                        | groups = newGroups
                        }
                _ -> layerModel
            )
        )


rebuildFluidGrid : LayerIndex -> FluidGrid.Model -> Model -> Model
rebuildFluidGrid index newFluidGridModel model =
    model |> updateLayerWithItsModel
        index
        (\(layer, layerModel) ->
            -- FIXME: why update model in two places??
            ( layer
            , case layerModel of
                FluidGridModel _ -> FluidGridModel newFluidGridModel
                _ -> layerModel
            )
        )


updateFss : LayerIndex -> (FSS.Model -> FSS.Model) -> Model -> Model
updateFss index f model =
    model |> updateLayerWithItsModel
        index
        (\(layer, layerModel) ->
            ( layer
            , case layerModel of
                FssModel fssModel ->
                    f fssModel |> FssModel
                _ -> layerModel
            )
        )

updateAndRebuildFssWith
    : LayerIndex
    -> (FSS.Model -> FSS.Model)
    -> Model
    -> ( Model, Cmd Msg )
updateAndRebuildFssWith index f curModel =
    let
        newModel = updateFss index f curModel
    in
        ( newModel
        , case (newModel |> getLayerModel index) of
            Just (FssModel fssModel) ->
                requestFssRebuild
                    { layer = index
                    , model = IE.encodePortModel newModel
                    , value = IE.encodeFss fssModel newModel.product
                    }
            _ -> Cmd.none
        )


forAllLayersOf : (LayerDef -> Maybe x) -> (Int -> x -> Cmd Msg) -> Model -> Cmd Msg
forAllLayersOf isRequiredLayer performWhat model =
    model.layers
        |> List.indexedMap Tuple.pair
        |> List.filterMap
            (\(index, layer) ->
                isRequiredLayer layer |> Maybe.map (Tuple.pair index)
            )
        |> List.map (\(index, layerModel) -> performWhat index layerModel)
        |> Cmd.batch


rebuildAllFssLayersWith : Model -> Cmd Msg
rebuildAllFssLayersWith model =
    let
        rebuildPotentialFss index fssModel =
            requestFssRebuild
                { layer = index
                , model = IE.encodePortModel model
                , value = IE.encodeFss fssModel model.product
                }
    in model |> forAllLayersOf isLayerFss rebuildPotentialFss


forAllFluidLayers : (Int -> Fluid.Model -> a) -> a -> (List (LayerIndex, a) -> b) -> Model -> b
forAllFluidLayers modifyFluidModel default composeList model =
    model
        -- FIXME: apply only to the layer by layer index
        |> getLayerModels (\kind -> if kind == Fluid then False else True)
        |> List.indexedMap (\layerIndex layerModel ->
            ( layerIndex
            , case layerModel of
                FluidModel fluidModel ->
                    modifyFluidModel layerIndex fluidModel
                _ -> default
            )
        )
        |> composeList


commandForAllFluidLayers : (Int -> Fluid.Model -> Cmd Msg) -> Model -> Cmd Msg
commandForAllFluidLayers modifyFluidModel model =
    forAllFluidLayers
        modifyFluidModel
        Cmd.none
        (List.map Tuple.second >> Cmd.batch)
        model


-- extractFssBuildOptions : Model -> FssBuildOptions
-- extractFssBuildOptions = prepareGuiConfig


generateAllMetaballs : Model -> Cmd Msg
generateAllMetaballs model =
    let
        palette = model.product |> Product.getPalette
    in model
        |> forAllLayersOf
                isMetaballLayer
                (\index _ -> generateMetaballs palette model.size index)


generateMetaballs : Product.Palette -> SizeRule -> LayerIndex -> Cmd Msg
generateMetaballs palette size layerIdx =
    Metaballs.generate
        (RebuildMetaballs layerIdx)
            (Metaballs.generator palette <| getRuleSizeOrZeroes size)


-- TODO: combine with generateAllMetaballs
generateAllFluid : Model -> Cmd Msg
generateAllFluid model =
    let
        -- _ = Debug.log "product for palette" model.product
        productPalette = Product.getPalette model.product
    in model
        |> forAllLayersOf
                isFluidLayer
                (\index fModel ->
                    generateFluid
                        model.size
                        fModel.variety
                        fModel.orbit
                        productPalette
                        index)


generateAllFluidGrids : Model -> Cmd Msg
generateAllFluidGrids model =
    model
        |> forAllLayersOf
                isFluidGridLayer
                (\index fModel ->
                    generateFluidGrid
                        model.size
                        index)


generateAllNativeMetaballs : Model -> Cmd Msg
generateAllNativeMetaballs model =
    let
        palette = model.product |> Product.getPalette
    in model
        |> forAllLayersOf
                isNativeMetaballsLayer
                (\index nmModel ->
                    generateNativeMetaballs
                        model.size
                        nmModel.variety
                        nmModel.orbit
                        palette
                        index)



generateAllInitialNativeMetaballs : Model -> Cmd Msg
generateAllInitialNativeMetaballs model =
    let
        palette = model.product |> Product.getPalette
    in model
        |> forAllLayersOf
                isNativeMetaballsLayer
                (\index nmModel ->
                    generateInitialNativeMetaballs
                        model.size
                        palette
                        index)


updateAllNativeMetaballsWith : Model -> Cmd Msg
updateAllNativeMetaballsWith model =
    let
        palette = model.product |> Product.getPalette
    in model
        |> forAllLayersOf
                isNativeMetaballsLayer
                (\index nmModel ->
                    generateNativeMetaballsDynamics
                        model.size
                        nmModel.variety
                        nmModel.orbit
                        palette
                        (Fluid.extractStatics nmModel)
                        index
                )


generateNativeMetaballs
    :  SizeRule
    -> Gaussian.Variety
    -> Fluid.Orbit
    -> Product.Palette
    -> LayerIndex
    -> Cmd Msg
generateNativeMetaballs size variety orbit palette layerIdx =
    NativeMetaballs.generate
        (UpdateNativeMetaballs layerIdx)
            (NativeMetaballs.generator
                (getRuleSizeOrZeroes size)
                <| Fluid.RandomizeAll NativeMetaballs.defaultRange palette variety orbit
            )


generateInitialNativeMetaballs
    :  SizeRule
    -> Product.Palette
    -> LayerIndex
    -> Cmd Msg
generateInitialNativeMetaballs size palette layerIdx =
    NativeMetaballs.generate
        (UpdateNativeMetaballs layerIdx)
            (NativeMetaballs.generator
                (getRuleSizeOrZeroes size)
                <| Fluid.RandomizeInitial palette NativeMetaballs.initial
            )


generateNativeMetaballsDynamics
    :  SizeRule
    -> Gaussian.Variety
    -> Fluid.Orbit
    -> Product.Palette
    -> Fluid.StaticModel
    -> LayerIndex
    -> Cmd Msg
generateNativeMetaballsDynamics size variety orbit palette curModel layerIdx =
    NativeMetaballs.generate
        (UpdateNativeMetaballs layerIdx)
            (NativeMetaballs.generator
                (getRuleSizeOrZeroes size)
                <| Fluid.RandomizeDynamics NativeMetaballs.defaultRange palette variety orbit curModel
            )


-- TODO: combine with generateAllMetaballs and generateAllFluid
regenerateFluidGradients : Model -> Cmd Msg
regenerateFluidGradients model =
    let
        -- _ = Debug.log "product for palette" model.product
        productPalette = Product.getPalette model.product
    in model
        |> forAllLayersOf
                isFluidLayer
                (\index fModel ->
                    generateFluidGradients productPalette index fModel)


generateFluid
    :  SizeRule
    -> Gaussian.Variety
    -> Fluid.Orbit
    -> Product.Palette
    -> LayerIndex
    -> Cmd Msg
generateFluid size variety orbit palette layerIdx =
    Fluid.generate
        (RebuildFluid layerIdx)
            (Fluid.generator
                (getRuleSizeOrZeroes size)
                <| Fluid.RandomizeAll Fluid.defaultRange palette variety orbit
            )


generateFluidGradients : Product.Palette -> LayerIndex -> Fluid.Model -> Cmd Msg
generateFluidGradients palette layerIdx currentModel =
    currentModel |>
        Fluid.generateGradientsFor
            (RebuildFluid layerIdx)
            palette


remapAllFluidLayersToNewSize : Model -> Model
remapAllFluidLayersToNewSize model =
    let newSize = getRuleSizeOrZeroes model.size
    in
        model
            |> updateAllLayerModels
                (\layerIndex layerKind layerModel ->
                    case layerModel of
                        FluidModel fluidModel -> FluidModel <| Fluid.remapTo newSize <| fluidModel
                        _ -> layerModel
                )

generateFluidGrid : SizeRule -> LayerIndex -> Cmd Msg
generateFluidGrid size layerIdx =
    FluidGrid.generate
        (RebuildFluidGrid layerIdx)
            (FluidGrid.generator <| SizeRule.toRecord size)


-- INCOMING PORTS

port bang : (() -> msg) -> Sub msg

port changeMode : (String -> msg) -> Sub msg

port pause : (() -> msg) -> Sub msg

port continue : (() -> msg) -> Sub msg

port triggerPause : (() -> msg) -> Sub msg

port hideControls : (() -> msg) -> Sub msg

port rotate : (Float -> msg) -> Sub msg

port initLayers : (Array String -> msg) -> Sub msg

port configureLorenz : ({ value: Lorenz.Model, layer: LayerIndex } -> msg) -> Sub msg

port configureFss : ({ value: FSS.PortModel, layer: LayerIndex } -> msg) -> Sub msg

port configureMirroredFss : ({ value: FSS.PortModel, layer: LayerIndex } -> msg) -> Sub msg

port changeProduct : (String -> msg) -> Sub msg

port rebuildFss : ({ value: FSS.SerializedScene, layer: LayerIndex } -> msg) -> Sub msg

port loadFluidGradientTextures : ({ value: List String, layer: LayerIndex } -> msg) -> Sub msg

port requestRegenerateFluidGradients : ({ layer: LayerIndex } -> msg) -> Sub msg

-- port requestUpdateNativeMetaballs : ({ layer: LayerIndex } -> msg) -> Sub msg

port turnOn : (LayerIndex -> msg) -> Sub msg

port turnOff : (LayerIndex -> msg) -> Sub msg

port mirrorOn : (LayerIndex -> msg) -> Sub msg

port mirrorOff : (LayerIndex -> msg) -> Sub msg

port import_ : (String -> msg) -> Sub msg

port changeFssRenderMode : ({ value: String, layer: LayerIndex } -> msg) -> Sub msg

port changeFacesX : ({ value: Int, layer: LayerIndex } -> msg) -> Sub msg

port changeFacesY : ({ value: Int, layer: LayerIndex } -> msg) -> Sub msg

port changeLightSpeed : ({ value: Int, layer: LayerIndex } -> msg) -> Sub msg

port changeVignette : ({ value: FSS.Vignette, layer: LayerIndex } -> msg) -> Sub msg

port changeIris : ({ value: FSS.Iris, layer: LayerIndex } -> msg) -> Sub msg

port changeAmplitude : ({ value: FSS.AmplitudeChange, layer: LayerIndex } -> msg) -> Sub msg

port shiftColor : ({ value: FSS.ColorShiftPatch, layer: LayerIndex } -> msg) -> Sub msg

port changeOpacity : ({ value: FSS.Opacity, layer: LayerIndex } -> msg) -> Sub msg

port resize :
    ({ presetCode: Maybe SizePresetCode
     , viewport: (Int, Int)
     } -> msg)
    -> Sub msg

port applyRandomizer : (PortModel -> msg) -> Sub msg

port savePng : (() -> msg) -> Sub msg

port changeWGLBlend :
    ( { layer : LayerIndex
      , value : WGLBlend.Blend
      }
    -> msg) -> Sub msg

port changeHtmlBlend :
    ( { layer : LayerIndex
      , value : String
      }
    -> msg) -> Sub msg

port refreshFluid :
    ( { layer : LayerIndex }
    -> msg) -> Sub msg

port changeFluidVariety :
    ( { layer : LayerIndex
      , value : Float
      }
    -> msg) -> Sub msg

port changeFluidOrbit :
    ( { layer : LayerIndex
      , value : Float
      }
    -> msg) -> Sub msg

port changeNativeMetaballsVariety :
    ( { layer : LayerIndex
      , value : Float
      }
    -> msg) -> Sub msg

port changeNativeMetaballsOrbit :
    ( { layer : LayerIndex
      , value : Float
      }
    -> msg) -> Sub msg

port switchBackgroundStop :
    ( { layer : LayerIndex
      , stopIndex : Int
      , value : Bool
      }
    -> msg) -> Sub msg

-- OUTGOING PORTS

type alias SizeUpdate =
    { size: ( Int, Int )
    , sizeRule : String
    , product: String
    -- , coverSize: Size
    , background: String
    , sizeConstant: Int
    -- , mode: String
    }

port startGui : ( PortModel, Constants ) -> Cmd msg

port requestFssRebuild :
    { layer: LayerIndex
    , model: PortModel
    , value: FSS.PortModel
    } -> Cmd msg

port buildFluidGradientTextures : ( Int, E.Value ) -> Cmd msg

port sizeChanged : SizeUpdate -> Cmd msg

port modeChanged : String -> Cmd msg

-- port nextBatchStep : { size, coverSize, product, background }

port export_ : String -> Cmd msg

port exportZip_ : String -> Cmd msg

port triggerSavePng : SizeUpdate -> Cmd msg -- FIXME: Remove, use Browser.DOM task instead

port requestRandomize : PortModel -> Cmd msg

port requestFitToWindow : () -> Cmd msg

port requestWindowResize : ( Int, Int ) -> Cmd msg

-- port rebuildOnClient : (FSS.SerializedScene, Int) -> Cmd msg

port updateNativeMetaballs :
    { index: LayerIndex
    , size: (Int, Int)
    , layerModel : E.Value
    , palette: List String
    } -> Cmd msg
