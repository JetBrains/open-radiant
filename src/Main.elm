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

import Random

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
import Model.SizeRule exposing (..)
import Model.SizeRule as SizeRule exposing (decode, encode, toRecord)
import Model.Error exposing (..)
import Model.Export as IE -- IE for import/export

import Model.Layer.Layer exposing (Layer, Blend(..))
import Model.Layer.Layer as Layer
import Model.Layer.Broadcast as B
import Model.Layer.Def as Layer exposing (Index, indexToString, Opacity(..))
import Model.Layer.Export as Layer exposing (encodeKind)
import Model.Layer.Layers as Layers
import Model.Layer.Blend.Html as HtmlBlend
import Model.Layer.Blend.WebGL as WGLBlend

import Layer.Background.Background as Background
import Layer.Cover.Cover as Cover
import Layer.NativeMetaballs.NativeMetaballs as NativeMetaballs

import Gui.Gui as Gui
import Gui.Mouse exposing (Position)
import TronGui as Gui
import Viewport exposing (Viewport)
import RenderQueue as RQ

-- import Controls
import Navigation as Nav
import Gradient as Gradient

{-
import Layer.Background.Background as Background
import Layer.Background.Background exposing (StopState(..), StopStates(..))
import Layer.Lorenz.Lorenz as Lorenz
import Layer.Fractal.Fractal as Fractal
import Layer.Voronoi.Voronoi as Voronoi
import Layer.FSS.FSS as FSS
import Layer.Template.Template as Template
import Layer.Cover.Cover as Cover
import Layer.Canvas.Canvas as Canvas
import Layer.Vignette.Vignette as Vignette
import Layer.Metaballs.Metaballs as Metaballs
import Layer.NativeMetaballs.NativeMetaballs as NativeMetaballs
import Layer.Fluid.Fluid as Fluid
import Layer.FluidGrid.FluidGrid as FluidGrid
-}


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
        ( layers, commands ) =
            initialLayers initialModel.mode
                        |> Layers.init ToLayer (getContext initialModel)
        initialModelWithLayers =
            { initialModel
            | layers = layers
            }
        initialModelWithLayersAndGui =
            initialModelWithLayers
            -- , gui = case model.mode of
            --     TronUi innerAppMode ->
            --         Just <| createGui { modelWithLayers | mode = innerAppMode }
            --     _ -> Nothing
        ( model, command ) =
            Nav.applyUrl url initialModelWithLayersAndGui
                |> batchUpdate initialModelWithLayersAndGui
    in
        ( model
        , Cmd.batch
            [ command
            , case model.size of
                Dimensionless ->
                    resizeToViewport
                _ -> Cmd.none
                -- _ ->
                --     if hasFssLayers model
                --         then rebuildAllFssLayersWith model
                --         else Cmd.none
            ]
        )


initialLayers : AppMode -> Layers.Initial
initialLayers mode =
    let
        layers =
            [ { fromDef = Cover.id
              , visibility = if mode == Ads then Layer.Hidden else Layer.Visible
              , blend = Layer.ForHtml HtmlBlend.default
              }
            , { fromDef = NativeMetaballs.id
              , visibility = Layer.Hidden
              -- , blend = Layer.ForWebGL WGLBlend.default
              , blend = Layer.ForHtml HtmlBlend.default
              }
            , { fromDef = NativeMetaballs.id
              , visibility = Layer.Visible
              -- , blend = Layer.ForWebGL WGLBlend.default
              , blend = Layer.ForHtml HtmlBlend.default
              }
            , { fromDef = Background.id
              , visibility = Layer.Locked
              , blend = Layer.ForHtml HtmlBlend.default
              }
            ]
    in
        layers
            |> List.filter (\{ fromDef } ->
                case ( fromDef, mode ) of
                    ( "cover", Ads ) -> False
                    _ -> True
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of

        Bang ->
            let
                ( layers, commands ) =
                    initialLayers model.mode
                        |> Layers.init ToLayer (getContext model)
            in
                ( { model
                  | layers = layers
                  {-
                  , gui = case model.mode of
                        TronUi innerAppMode ->
                            Just <| createGui { modelWithLayers | mode = innerAppMode }
                        _ -> Nothing
                  -}
                  }
                , Cmd.batch
                    [ startGui
                        ( model |> IE.encodeForPort
                        , makeConstants
                        )
                    , commands
                    ]
                )

        ChangeMode mode ->
            let
                ( newModel, commands ) =
                    update Bang model
            in
                ( newModel
                , Cmd.batch
                    [ commands
                    , Mode.encode newModel.mode |> modeChanged
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
                        _ -> Cmd.none
                        {-
                        _ ->
                            if hasFssLayers model
                                then rebuildAllFssLayersWith model
                                else Cmd.none
                        -}
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
            ( { model
              | paused = True }
            , Cmd.none
            ) |> broadcastAll B.Pause

        Continue ->
            ( { model | paused = False }
            , Cmd.none
            ) |> broadcastAll B.Continue

        TriggerPause ->
            ( { model | paused = not model.paused }
            , Cmd.none
            ) |> broadcastAll ( if model.paused then B.Continue else B.Pause )

        HideControls ->
            ( { model | controlsVisible = False }
            , Cmd.none
            )

        Import encodedModel ->
            case (encodedModel
                    |> IE.decodeFromString model.navKey (getContext model) Gui.gui) of
                Ok decodedModel ->
                    ( decodedModel
                    , Cmd.batch
                        [ Nav.pushUrlFrom decodedModel ]
                        {-
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
                        -}
                    )
                Err importError ->
                    ( model |> addError importError
                    , Cmd.none
                    )

        Export ->
            ( model
            , model |> IE.encodeToString |> export_
            )

        ExportZip ->
            ( model
            , model |> IE.encodeToString |> exportZip_
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
                ( width, height ) = getRuleSizeOrZeroes rule
                newModelWithSize =
                    { model
                    | size = rule
                    , origin = getOrigin ( width, height )
                    }
            in
                ( newModelWithSize
                , Cmd.batch
                    [ requestWindowResize ( width, height )
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
            (
                { model
                | layers = model.layers |> Layers.modify Layer.show index
                }
            , Cmd.none
            ) |> broadcast B.TurnOn index


        TurnOff index ->
            (
                { model
                | layers = model.layers |> Layers.modify Layer.hide index
                }
            , Cmd.none
            ) |> broadcast B.TurnOff index


        ChangeProduct product ->
            let
                modelWithProduct =
                    { model
                    | product = product
                    }
            in
                ( modelWithProduct
                , Nav.pushUrlFrom modelWithProduct
                ) |> broadcastAll (B.ChangeProduct product)


        TriggerFeelLucky ->
            ( model
            , model.layers
                |> List.filter (not << Layer.isDef Background.id)
                |> Layers.randomizeStats
                |> List.map
                        (\(index, generator) ->
                            Random.generate (ApplyStats index) generator
                        )
                |> Cmd.batch
                -- [ Nav.pushUrlFrom model ]
            ) |> broadcastAll B.IFeelLucky


        ApplyStats index ( blend, Opacity opacity ) ->
            (
                { model
                | layers =
                        model.layers
                            |> Layers.modify
                                (\layer ->
                                    layer
                                        |> Layer.changeBlend blend
                                        |> Layer.changeOpacity opacity
                                ) index
                }
            , Layer.updateLayerStats
                { blend = Layer.encodePortBlend blend
                , layer = Layer.indexToJs index
                , opacity = opacity
                }
            )

        ChangeWGLBlend index newBlend ->
            let
                newModel =
                    { model
                    | layers =
                        model.layers
                            |> Layers.modify
                                    (Layer.changeBlend <| Layer.ForWebGL newBlend)
                                    index
                    }
            in
                ( newModel
                , Cmd.none
                )

        AlterWGLBlend index changeF ->
            let
                newModel =
                    { model
                    | layers =
                        model.layers
                            |> Layers.modify (Layer.alterWebGlBlend changeF) index
                    }
            in
                ( newModel
                , Cmd.none
                )

        ChangeHtmlBlend index newBlend ->
            let
                newModel =
                    { model
                    | layers =
                        model.layers
                            |> Layers.modify
                                (Layer.changeBlend <| ForHtml newBlend)
                                index
                    }
            in
                ( newModel
                , Cmd.none
                )

        ChangeOpacity index value ->
            let
                newModel =
                    { model
                    | layers =
                        model.layers |> Layers.modify (Layer.changeOpacity value) index

                    }
            in
                ( newModel
                , Cmd.none
                )


        ToLayer index layerMsg ->
            case model.layers
                |> Layers.update ToLayer (getContext model) index layerMsg of
                ( newLayers, cmds ) ->
                    ( { model
                      | layers = newLayers
                      }
                    , cmds
                    )

        SavePng ->
            ( model
            , triggerSavePng
                { size = getRuleSize model.size |> Maybe.withDefault ( -1, -1 )
                , product = Product.encode model.product
                , background = model.background
                , layers =
                    Layers.collectStats model.layers
                        |> List.map
                            (\{ index, def, kind, visibility } ->
                                { index = index
                                , def = def
                                , kind = Layer.encodeKind kind
                                , visibility = Layer.encodeVisibility visibility
                                }
                            )
                }
            )

        Randomize ->
            ( model
            , model |> IE.encodeForPort |> requestRandomize
            -- TODO: updateUrl newModel?
            )

        ApplyRandomizer portModel ->
            case IE.decodeFromPort model.navKey (getContext model) portModel of
                Ok decodedPortModel ->
                    ( decodedPortModel
                    , Cmd.none
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
        , Layers.subscribe ToLayer (getContext model) model.layers
        , changeWGLBlend (\{ layer, value } ->
            ChangeWGLBlend (Layer.makeIndex layer) value
          )
        , changeHtmlBlend (\{ layer, value } ->
            ChangeHtmlBlend (Layer.makeIndex layer) <| HtmlBlend.decode value
          )
        , changeOpacity (\{ layer, value } ->
            ChangeOpacity (Layer.makeIndex layer) value
          )
        , iFeelLucky
            (\_ -> TriggerFeelLucky)
        , applyRandomizer ApplyRandomizer
        , import_ Import
        , pause (\_ -> Pause)
        , continue (\_ -> Continue)
        , triggerPause (\_ -> TriggerPause)
        , hideControls (\_ -> HideControls)
        , turnOn (Layer.makeIndex >> TurnOn)
        , turnOff (Layer.makeIndex >> TurnOff)
        -- , mirrorOn MirrorOn
        -- , mirrorOff MirrorOff
        , savePng (\_ -> SavePng)
        ]


view : Model -> Html Msg
view model =
    let
        ( w, h ) =
                getRuleSize model.size |> Maybe.withDefault ( -1, -1 )
        visible = w > 0 && h > 0
        wrapHtml htmls =
            div
                [ H.class "html-layers", H.class "layers"
                , Events.onClick TriggerPause
                ]
                <| List.map
                    (\( index, _, html ) ->
                        div
                            [ H.id <| "layer-" ++ Layer.indexToString index ]
                            [ Html.map (ToLayer index) html ]
                    )
                    htmls
        wrapEntities entities =
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
                <| List.map (\(_, _, e) -> e) entities
        renderedLayers =
            model.layers
                |> Layers.render (getContext model)
                |> RQ.make
                |> RQ.apply
                        wrapHtml
                        wrapEntities
        isInPlayerMode =
            case model.mode of
                Player -> True
                _ -> False
    in div
        [ H.class <| "mode-" ++ Mode.encode model.mode ]
        [ renderedLayers --|> RQ.apply wrapHtml wrapEntities
        , if model.controlsVisible && not isInPlayerMode
            then ( div
                ([ "overlay-panel", "import-export-panel", "hide-on-space" ] |> List.map H.class)
                [ div [ H.class "timeline-holder" ]
                    [
                    --     span [ H.class "label past"] [ text "past" ]
                    --    , input
                    --     [ type_ "range"
                    --     , class "timeline"
                    --     , H.min "0"
                    --     , H.max "100"
                    --     , extractTimeShift model.timeShift |> H.value
                    --     , Events.onInput (\v -> adaptTimeShift v |> TimeTravel)
                    --     , Events.onMouseUp BackToNow
                    --     ]
                    --     []
                    -- , span [ H.class "label", H.class "future" ] [ text "future" ]
                    ]
                -- , input [ type_ "button", id "import-button", value "Import" ] [ text "Import" ]
                -- , input [ type_ "button", onClick Export, value "Export" ] [ text "Export" ]
                , input
                    [ type_ "button", class "export_html5"
                    , Events.onClick ExportZip, value "|> HTML5" ]
                    [ text "Export to html5.zip" ]
                , input
                    [ type_ "button", class "export_png"
                    , Events.onClick SavePng, value "|> PNG" ]
                    [ text "Export to png" ]
                , div [ H.class "spacebar_info" ] [ text "spacebar to hide controls, click to pause" ]
                ]
            ) else div [] []
        , model.gui
            |> Maybe.map Gui.view
            |> Maybe.map (Html.map GuiMessage)
            |> Maybe.map (\guiLayer -> div [ H.class "hide-on-space" ] [ guiLayer ])
            |> Maybe.withDefault (div [] [])
        , if not isInPlayerMode then canvas [ H.id "js-save-buffer" ] [ ] else div [] []
        , if hasErrors model
            then
                div [ H.id "error-pane", H.class "has-errors" ]
                    ( case model.errors of
                        Errors errorsList ->
                            errorsList |> List.map (\err -> span [] [ text err ])
                    )
            else div [ H.id "error-pane" ] []
        ]


document : Model -> Browser.Document Msg
document model =
    { title = "Elmsfeuer, Radiant"
    , body = [ view model ]
    }


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


resizeToViewport =
    Task.perform
        (\{ viewport } ->
            Resize
                <| UseViewport
                <| ViewportSize (floor viewport.width) (floor viewport.height))
        Browser.getViewport


broadcast : B.Msg -> Layer.Index -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
broadcast bmsg index ( model, commands ) =
    let
        ( newLayers, layersCommands ) =
            model.layers
                |> Layers.broadcast ToLayer (getContext model) index bmsg
    in
        (
            { model
            | layers = newLayers
            }
        , Cmd.batch [ layersCommands, commands ]
        )



broadcastAll : B.Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
broadcastAll bmsg ( model, commands ) =
    let
        ( newLayers, layersCommands ) =
            model.layers
                |> Layers.broadcastAll ToLayer (getContext model) bmsg
    in
        (
            { model
            | layers = newLayers
            }
        , Cmd.batch [ layersCommands, commands ]
        )


-- TODO: The functions below belong to the specific layers. move them to `Model/Layer?`

-- INCOMING PORTS

port bang : (() -> msg) -> Sub msg

port changeMode : (String -> msg) -> Sub msg

port pause : (() -> msg) -> Sub msg

port continue : (() -> msg) -> Sub msg

port triggerPause : (() -> msg) -> Sub msg

port hideControls : (() -> msg) -> Sub msg

port rotate : (Float -> msg) -> Sub msg

port initLayers : (Array String -> msg) -> Sub msg

port changeProduct : (String -> msg) -> Sub msg

port turnOn : (Layer.JsIndex -> msg) -> Sub msg

port turnOff : (Layer.JsIndex -> msg) -> Sub msg

port mirrorOn : (Layer.JsIndex -> msg) -> Sub msg

port mirrorOff : (Layer.JsIndex -> msg) -> Sub msg

port import_ : (String -> msg) -> Sub msg

port iFeelLucky : (() -> msg) -> Sub msg

port resize :
    ({ presetCode: Maybe SizePresetCode
     , viewport: (Int, Int)
     } -> msg)
    -> Sub msg

port applyRandomizer : (PortModel -> msg) -> Sub msg

port savePng : (() -> msg) -> Sub msg

port changeWGLBlend :
    ( { layer : Layer.JsIndex
      , value : WGLBlend.Blend
      }
    -> msg) -> Sub msg

port changeHtmlBlend :
    ( { layer : Layer.JsIndex
      , value : String
      }
    -> msg) -> Sub msg

port changeOpacity :
    ( { layer : Layer.JsIndex
      , value : Float
      }
    -> msg) -> Sub msg

-- OUTGOING PORTS

port startGui : ( PortModel, Constants ) -> Cmd msg

port modeChanged : String -> Cmd msg

-- port nextBatchStep : { size, coverSize, product, background }

port export_ : String -> Cmd msg

port exportZip_ : String -> Cmd msg

port triggerSavePng :
    { size : ( Int, Int )
    , product : String
    , background : String
    , layers :
        List
            { index : Layer.JsIndex
            , def : String
            , kind : String
            , visibility : String
            }
    } -> Cmd msg
    -- FIXME: Remove, use Browser.DOM task instead

port requestRandomize : PortModel -> Cmd msg

port requestFitToWindow : () -> Cmd msg

port requestWindowResize : ( Int, Int ) -> Cmd msg

-- port rebuildOnClient : (FSS.SerializedScene, Int) -> Cmd msg
