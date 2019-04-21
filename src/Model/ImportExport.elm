module Model.ImportExport exposing
    ( encodeModel
    , decodeModel
    , encodePortModel
    , encodePortLayer
    , decodePortModel
    , encodeFss
    , encodeLayerModel
    , fromFssPortModel
    , adaptModelDecodeErrors
    )

import Array

import Tuple
import Time

import Math.Vector2 as Vec2

import Json.Decode as D exposing (bool, int, string, float, Decoder, Value)
import Json.Decode.Pipeline as D exposing (required, optional, hardcoded)
import Json.Decode.Extra as D exposing (andMap)
import Json.Encode as E exposing (encode, Value, string, int, float, bool, list, object)

import Model.WebGL.Blend as WGLBlend
import Model.Html.Blend as HtmlBlend

import Layer.FSS as FSS
import Layer.Lorenz as Lorenz
import Layer.Fluid as Fluid

import Model.Core as M
import Model.AppMode as M
import Model.Layer as M
import Model.SizeRule as M
import Model.Error as M
import Model.Product as Product exposing (Product)
import Model.Product exposing (..)
import Model.Range exposing (..)

import TronGui as GUI


-- FIXME: Move all the decoding/encoding to the corresponding layers


type ModelDecodeError
    = LayerDecodeErrors (List LayerDecodeError)
    | SizeRuleDecodeError String
    -- | ModeDecodeError String
    | ProductDecodeError String


type LayerDecodeError
    = KindDecodeFailed String
    | BlendDecodeFailed String
    | LayerCreationFailed String
    | LayerModelDecodeFailed D.Error


encodeIntPair : ( Int, Int ) -> E.Value
encodeIntPair ( v1, v2 ) =
    E.object
        [ ( "v1", E.int v1 )
        , ( "v2", E.int v2 )
        ]


-- encodePairAsArray : (a -> E.Value) -> ( a, a ) -> E.Value
-- encodePairAsArray f ( v1, v2 ) =
--     E.list f [ v1, v2 ]


encodeXY : (a -> E.Value) -> { x: a, y: a } -> E.Value
encodeXY f { x, y } =
    E.object
        [ ( "x", f x )
        , ( "y", f y )
        ]


encodeColor : { r: Float, g: Float, b: Float } -> E.Value
encodeColor { r, g, b } =
    E.list
        E.float
        [ r
        , g
        , b
        ]


encodeColorShift : FSS.ColorShift -> E.Value
encodeColorShift { hue, saturation, brightness } =
    E.list
        E.float
        [ hue
        , saturation
        , brightness
        ]


encodeAmplitude : FSS.Amplitude -> E.Value
encodeAmplitude { amplitudeX, amplitudeY, amplitudeZ } =
    E.list
        E.float
        [ amplitudeX
        , amplitudeY
        , amplitudeZ
        ]


-- encodeTripleAsArray : (a -> E.Value) -> Array a -> E.Value
-- encodeTripleAsArray f [ v1, v2, v3 ] =
--    [ v1, v2, v3 ]
--         |> List.map f
--         |> Array.fromList
--         |> E.array


encodeKind : M.LayerKind -> E.Value
encodeKind = E.string << M.encodeKind


webglOrHtml : M.LayerDef -> String
webglOrHtml layerDef =
     case layerDef.layer of
        M.WebGLLayer _ _ -> "webgl"
        M.HtmlLayer _ _ -> "html"


encodeLayerDef : M.LayerDef -> E.Value
encodeLayerDef layerDef =
    E.object
        [ ( "kind", encodeKind layerDef.kind )
        , ( "blend",
            case layerDef.layer of
                M.WebGLLayer _ webglBlend ->
                    WGLBlend.encodeOne webglBlend |> E.string
                M.HtmlLayer _ htmlBlend ->
                    HtmlBlend.encode htmlBlend |> E.string
          )
        , ( "blendDesc",
            case layerDef.layer of
                M.WebGLLayer _ webglBlend ->
                    webglBlend
                    |> WGLBlend.encodeHumanOne { delim = "; ", space = "> " }
                    |> E.string
                M.HtmlLayer _ htmlBlend ->
                    HtmlBlend.encode htmlBlend |> E.string
          )
        , ( "isOn", layerDef.on |> E.bool )
        , ( "model", encodeLayerModel layerDef.model )
        , ( "name", layerDef.name |> E.string )
        , ( "webglOrHtml", webglOrHtml layerDef |> E.string )
        -- , ( "mesh", E.string "" )
        ]


encodeLayerModel : M.LayerModel -> E.Value
encodeLayerModel layerModel =
    E.object <|
        case layerModel of
            M.FssModel fssModel ->
                [ ( "renderMode", FSS.encodeRenderMode fssModel.renderMode |> E.string )
                , ( "faces", encodeXY E.int fssModel.faces )
                , ( "lightSpeed", E.int fssModel.lightSpeed )
                , ( "amplitude", encodeAmplitude fssModel.amplitude )
                , ( "colorShift", encodeColorShift fssModel.colorShift )
                , ( "opacity", E.float fssModel.opacity )
                , ( "mirror", E.bool fssModel.mirror )
                , ( "clip",
                        Maybe.withDefault FSS.noClip fssModel.clip
                            |> encodeXY E.float
                  )
                , ( "shareMesh", E.bool fssModel.shareMesh )
                , ( "vignette", E.float fssModel.vignette )
                , ( "iris", E.float fssModel.iris )
                ]
            M.VignetteModel vignetteModel ->
                [ ( "opacity", E.float vignetteModel.opacity )
                , ( "color", encodeColor vignetteModel.color )
                ]
            M.FluidModel fluidModel ->
                let
                    encodeBall ball =
                        E.object
                            [ ( "x", E.float <| Vec2.getX ball.origin )
                            , ( "y", E.float <| Vec2.getY ball.origin )
                            , ( "r", E.float ball.radius )
                            , ( "speed", E.float ball.speed )
                            , ( "t", E.float ball.t )
                            , ( "ax", E.float <| Vec2.getX ball.arcMult )
                            , ( "ay", E.float <| Vec2.getY ball.arcMult )
                            ]
                    encodeStop ( stopPos, stopColor )  =
                        E.object
                            [ ( "pos", E.float stopPos )
                            , ( "color", E.string stopColor )
                            ]
                    encodeGradient { stops, orientation } =
                        E.object
                            [ ( "stops", E.list encodeStop stops )
                            , ( "orientation",
                                case orientation of
                                    Fluid.Horizontal -> E.string "horizontal"
                                    Fluid.Vertical -> E.string "vertical"
                                )
                            ]
                    encodeGroup group =
                        E.object
                            [ ( "balls", E.list encodeBall group.balls )
                            , ( "gradient"
                              , group.gradient
                                    |> Maybe.map encodeGradient
                                    |> Maybe.withDefault E.null
                              )
                            ]

                in
                    [ ( "groups", E.list encodeGroup fluidModel.groups )
                    ]
            _ -> [] -- FIXME: fail for unknown layer kinds, but don't fail if layer just has empty model


encodeModel_ : M.Model -> E.Value
encodeModel_ model =
    E.object
        [ ( "background", E.string model.background )
        , ( "mode", E.string <| M.encodeMode model.mode )
        , ( "theta", E.float model.theta )
        , ( "omega", E.float model.omega )
        , ( "layers", E.list encodeLayerDef model.layers )
        -- , ( "layers", E.list (List.filterMap
        --         (\layer -> Maybe.map encodeLayer layer) model.layers) )
        -- for b/w compatibility, we also encode size as numbers, but sizeRule is what should matter
        -- when it is defined/known on import
        , ( "size", encodeIntPair <| M.getRuleSizeOrZeroes model.size )
        , ( "sizeRule", E.string <| M.encodeSizeRule model.size )
        , ( "origin", encodeIntPair model.origin )
        , ( "mouse", encodeIntPair model.mouse )
        , ( "now", E.float model.now )
        , ( "palette",
            model.product
                |> getPalette
                |> E.list E.string )
        , ( "product", model.product |> Product.encode |> E.string )
        ]


encodeModel : M.Model -> String
encodeModel model = model |> encodeModel_ |> E.encode 2


encodePortModel : M.Model -> M.PortModel
encodePortModel model =
    { background = model.background
    , mode = M.encodeMode model.mode
    , now = model.now
    , theta = model.theta
    , omega = model.omega
    , layers = List.map encodePortLayer model.layers
    , size = M.getRuleSize model.size |> Maybe.withDefault ( -1, -1 )
    , sizeRule = M.encodeSizeRule model.size |> Just
    , origin = model.origin
    , mouse = model.mouse
    , palette = model.product |> getPalette
    , product = model.product |> Product.encode
    }


decodePortModel : M.CreateLayer -> M.PortModel -> Result (List ModelDecodeError) M.Model
decodePortModel createLayer portModel =
    let
        couldBeDecodedLayers =
            List.map (decodePortLayer createLayer) portModel.layers
        extractLayerDecodeErrors res =
            case res of
                Ok layer -> Nothing
                Err errors -> Just <| LayerDecodeErrors errors
        layerDecodeErrors =
            couldBeDecodedLayers
                |> List.filterMap extractLayerDecodeErrors
        tryToDecodeSize maybeSizeRule =
            case maybeSizeRule of
                Just sizeRuleStr ->
                    M.decodeSizeRule sizeRuleStr
                        |> Result.mapError (List.singleton << SizeRuleDecodeError)
                Nothing -> case portModel.size of
                    ( w, h ) -> Ok <| M.Custom w h
        applyDecoded decodedLayers decodedSize decodedProduct =
            let
                modeResult = M.decodeMode portModel.mode
                mode =
                    modeResult
                        |> Result.withDefault M.Production
                initialModel = M.initEmpty mode
                decodedModel =
                    { initialModel
                    | background = portModel.background
                    , mode = mode
                    , now = portModel.now
                    , theta = portModel.theta
                    , omega = portModel.omega
                    , layers = decodedLayers
                    , size = decodedSize
                    , origin = portModel.origin
                    , mouse = portModel.mouse
                    , product = decodedProduct
                    }
            in
                { decodedModel
                | gui = case mode of
                    M.TronUi _ -> Just <| GUI.gui decodedModel
                    _ -> Nothing
                }
    in
        Result.map3 -- TODO: join in a list of all failures
            applyDecoded
            (if List.isEmpty layerDecodeErrors
                then couldBeDecodedLayers
                    |> List.filterMap Result.toMaybe
                    |> Ok
                else Err <| layerDecodeErrors)
            (tryToDecodeSize portModel.sizeRule)
            (portModel.product
                |> Product.decode
                |> Result.mapError (List.singleton << ProductDecodeError))



encodePortLayer : M.LayerDef -> M.PortLayerDef
encodePortLayer layerDef =
    { kind = M.encodeKind layerDef.kind
    , isOn = layerDef.on
    , webglOrHtml = webglOrHtml layerDef
    , blend =
        case layerDef.layer of
            M.WebGLLayer _ webglBlend ->
                ( Just webglBlend, Nothing )
            M.HtmlLayer _ htmlBlend ->
                ( Nothing, HtmlBlend.encode htmlBlend |> Just )
    , name = layerDef.name
    , model = layerDef.model
        |> encodeLayerModel
        |> E.encode 2
    }


decodePortLayer : M.CreateLayer -> M.PortLayerDef -> Result (List LayerDecodeError) M.LayerDef
decodePortLayer createLayer portLayerDef  =
    M.decodeKind portLayerDef.kind
        |> Result.mapError KindDecodeFailed
        |> Result.andThen
            (\kind ->
                portLayerDef.model
                    |> D.decodeString (layerModelDecoder kind)
                    |> Result.map (\layerModel -> ( kind, layerModel ))
                    |> Result.mapError LayerModelDecodeFailed
            )
        |> Result.andThen
            (\( kind, layerModel ) ->
                createLayer kind layerModel
                    |> Result.fromMaybe
                        (LayerCreationFailed <| "kind: " ++ M.encodeKind kind)
                    |> Result.map (\layer -> ( kind, layerModel, layer ))
            )
        |> Result.map
            (\( kind, layerModel, layerWithoutBlend ) ->
                ( case layerWithoutBlend of
                    M.WebGLLayer webglLayer _ ->
                        portLayerDef.blend
                            |> Tuple.first
                            |> Maybe.withDefault WGLBlend.default
                            -- TODO: produce BlendDecodeError?
                            |> M.WebGLLayer webglLayer
                    M.HtmlLayer htmlLayer _ ->
                        portLayerDef.blend
                            |> Tuple.second
                            |> Maybe.map HtmlBlend.decode
                            |> Maybe.withDefault HtmlBlend.default
                            -- TODO: produce BlendDecodeError?
                            |> M.HtmlLayer htmlLayer
                , layerModel
                , kind
                )
            )
        |> Result.mapError List.singleton
        |> Result.map
            (\( layer, layerModel, kind ) ->
                -- TODO: try to avoid using records in this mapping
                { kind = kind
                , on = portLayerDef.isOn
                , layer = layer
                , model = layerModel
                , name = portLayerDef.name
                }
            )


intPairDecoder : D.Decoder (Int, Int)
intPairDecoder =
    D.map2 Tuple.pair
        (D.field "v1" D.int)
        (D.field "v2" D.int)


resultToDecoder : Result String a -> D.Decoder a
resultToDecoder result =
    case result of
        Ok res -> D.succeed res
        Err err -> D.fail err


resultToDecoder_ : (x -> String) -> Result x a -> D.Decoder a
resultToDecoder_ errToStr result =
    case result of
        Ok res -> D.succeed res
        Err err -> D.fail <| errToStr err


maybeToDecoder : String -> Maybe a -> D.Decoder a
maybeToDecoder failureReason maybe =
    case maybe of
        Just v -> D.succeed v
        Nothing -> D.fail failureReason


layerDefDecoder : M.CreateLayer -> D.Decoder M.LayerDef
layerDefDecoder createLayer =
    let
        createLayerDef kindStr layerModelStr name isOn blendStr =
            M.decodeKind kindStr
                |> resultToDecoder
                |> D.andThen
                    (\kind ->
                        layerModelStr
                            |> D.decodeString (layerModelDecoder kind)
                            |> resultToDecoder_ D.errorToString
                            |> D.map (\layerModel -> ( kind, layerModel ))
                    )
                |> D.andThen
                    (\( kind, layerModel ) ->
                        createLayer kind layerModel
                            |> maybeToDecoder
                                ("Failed to create layer with kind: " ++ M.encodeKind kind)
                            |> D.map (\layer -> ( kind, layerModel, layer ))
                    )
                |> D.map
                    (\( kind, layerModel, layerWithoutBlend ) ->
                        let
                            layer =
                                case layerWithoutBlend of
                                    M.WebGLLayer webglLayer _ ->
                                        WGLBlend.decodeOne blendStr
                                            |> Maybe.withDefault WGLBlend.default
                                            |> M.WebGLLayer webglLayer
                                    M.HtmlLayer htmlLayer _ ->
                                        HtmlBlend.decode blendStr |>
                                            M.HtmlLayer htmlLayer
                        in
                            { kind = kind
                            , on = isOn
                            , layer = layer
                            , model = layerModel
                            , name = name
                            }
                    )
    in
        D.map5 createLayerDef
            (D.field "kind" D.string)
            (D.field "model" D.string)
            (D.field "name" D.string)
            (D.field "isOn" D.bool)
            (D.field "blend" D.string)
            |> D.andThen identity


layerModelDecoder : M.LayerKind -> D.Decoder M.LayerModel
layerModelDecoder kind =
    case kind of
        M.Fss ->
            let
                createFssModel
                    renderModeStr
                    faces
                    amplitude
                    colorShift
                    opacity
                    mirror
                    clip
                    lightSpeed
                    shareMesh
                    vignette
                    iris =
                    case [ faces, amplitude, colorShift, clip ] of
                        [ [facesX, facesY], [amplitudeX, amplitudeY, amplitudeZ], [hue, saturation, brightness], [clipX, clipY] ] ->
                            M.FssModel
                                { renderMode = FSS.decodeRenderMode renderModeStr
                                , faces = { x = floor facesX, y = floor facesY }
                                , amplitude =
                                    { amplitudeX = amplitudeX
                                    , amplitudeY = amplitudeY
                                    , amplitudeZ = amplitudeZ
                                    }
                                , colorShift =
                                    { hue = hue
                                    , saturation = saturation
                                    , brightness = brightness
                                    }
                                , opacity = opacity
                                , mirror = mirror
                                , clip = Just { x = clipX, y = clipY }
                                , lightSpeed = lightSpeed
                                , shareMesh = shareMesh
                                , vignette = vignette
                                , iris = iris
                                }
                            |> D.succeed
                        _ -> D.fail "failed to parse model"
            in
                D.succeed createFssModel
                    |> D.andMap (D.field "renderMode" D.string)
                    |> D.andMap (D.field "faces" <| D.list D.float)
                    |> D.andMap (D.field "amplitude" <| D.list D.float)
                    |> D.andMap (D.field "colorShift" <| D.list D.float)
                    |> D.andMap (D.field "opacity" D.float)
                    |> D.andMap (D.field "mirror" D.bool)
                    |> D.andMap (D.field "clip" <| D.list D.float)
                    |> D.andMap (D.field "lightSpeed" D.int)
                    |> D.andMap (D.field "shareMesh" D.bool)
                    |> D.andMap (D.field "vignette" D.float)
                    |> D.andMap (D.field "iris" D.float)
                    |> D.andThen identity
        M.MirroredFss ->
            layerModelDecoder M.Fss
        M.Fluid ->
            let
                makeBall =
                    D.map7
                        (\x y r speed t ax ay ->
                            { origin = Vec2.vec2 x y
                            , radius = r
                            , speed = speed
                            , t = t
                            , arcMult = Vec2.vec2 ax ay
                            }
                        )
                        (D.field "x" D.float)
                        (D.field "y" D.float)
                        (D.field "r" D.float)
                        (D.field "speed" D.float |> D.withDefault (getFloatMin Fluid.speedRange))
                        (D.field "t" D.float |> D.withDefault 0)
                        (D.field "ax" D.float |> D.withDefault (getFloatMin Fluid.multArcX))
                        (D.field "ay" D.float |> D.withDefault (getFloatMin Fluid.multArcY))
                makeGradientStop =
                    D.map2
                        Tuple.pair
                        (D.field "pos" D.float)
                        (D.field "color" D.string)
                makeGradient =
                    D.map2
                        (\stops orientationStr ->
                            { stops = stops
                            , orientation =
                                case orientationStr of
                                    "horizontal" -> Fluid.Horizontal
                                    "vertical" -> Fluid.Vertical
                                    _ -> Fluid.Vertical
                            }
                        )
                        (D.field "stops" <| D.list makeGradientStop)
                        (D.field "orientation" <| D.string)
                makeGroup =
                    D.map2
                        (\balls gradient ->
                            { balls = balls
                            , textures = Nothing
                            , gradient = gradient
                            }
                        )
                        (D.field "balls" <| D.list makeBall)
                        (D.field "gradient" <| D.maybe <| makeGradient)
            in
                D.list makeGroup
                    |> D.field "groups"
                    |> D.map (\groups -> { groups = groups })
                    |> D.map M.FluidModel
        -- TODO: add parsing other models here
        _ -> D.succeed <| M.initLayerModel kind -- FIXME: Fail to decode if layer is unknown, but don't fail if it just has empty model
        -- _ -> D.fail "unknown kind"


modelDecoder : M.AppMode -> M.CreateLayer -> M.CreateGui -> D.Decoder M.Model
modelDecoder currentMode createLayer createGui =
    let
        createModel
            background
            theta
            omega
            layers
            maybeSize
            maybeSizeRule
            origin
            mouse
            now
            productStr =
            let
                initialModel = M.init currentMode [] createLayer createGui
                sizeResult =
                    case maybeSizeRule of
                        Just sizeRuleStr -> M.decodeSizeRule sizeRuleStr
                        Nothing -> case maybeSize of
                            Just (w, h) -> Ok <| M.Custom w h
                            Nothing -> Err "Unknown Size"
            in
                sizeResult
                    |> resultToDecoder
                    |> D.map2
                        (\product size ->
                            { initialModel
                            | background = background
                            , theta = theta
                            , omega = omega
                            , layers = layers
                            , size = size
                            , origin = origin
                            , mouse = mouse
                            , now = now
                            , product = product
                            --, palette = Product.getPalette product
                            }
                        )
                        (Product.decode productStr |> resultToDecoder)

    in
        -- case maybeSizeRule of
        --     Just sizeRuleStr -> M.decodeSizeRule sizeRuleStr
        --     Nothing -> case maybeSize of
        --         Just (w, h) -> M.Custom w h
        --         Nothing -> M.Dimensionless
        D.succeed createModel
            |> D.andMap (D.field "background" D.string)
            |> D.andMap (D.field "theta" D.float)
            |> D.andMap (D.field "omega" D.float)
            |> D.andMap (D.field "layers" (layerDefDecoder createLayer |> D.list))
            |> D.andMap (D.maybe (D.field "size" intPairDecoder))
            |> D.andMap (D.maybe (D.field "sizeRule" D.string))
            |> D.andMap (D.field "origin" intPairDecoder)
            |> D.andMap (D.field "mouse" intPairDecoder)
            |> D.andMap (D.field "now" D.float)
            |> D.andMap (D.field "product" D.string)
            |> D.andThen identity


decodeModel : M.AppMode -> M.CreateLayer -> M.CreateGui -> String -> Result String M.Model
decodeModel currentMode createLayer createGui modelStr =
    D.decodeString (modelDecoder currentMode createLayer createGui) modelStr
        |> Result.mapError D.errorToString


encodeFss : FSS.Model -> Product -> FSS.PortModel
encodeFss m product =
    { amplitude = m.amplitude
    , colorShift = m.colorShift
    , opacity = m.opacity
    , faces = m.faces
    , lightSpeed = m.lightSpeed
    , renderMode = FSS.encodeRenderMode m.renderMode
    , clip = m.clip
    , shareMesh = m.shareMesh
    , vignette = m.vignette
    , iris = m.iris
    , mirror = m.mirror
    --, palette = product |> getPalette
    }


fromFssPortModel : FSS.PortModel -> FSS.Model
fromFssPortModel pm =
    { amplitude = pm.amplitude
    , colorShift = pm.colorShift
    , opacity = pm.opacity
    , faces = pm.faces
    , lightSpeed = pm.lightSpeed
    , renderMode = FSS.decodeRenderMode pm.renderMode
    , clip = pm.clip
    , shareMesh = pm.shareMesh
    , vignette = pm.vignette
    , iris = pm.iris
    , mirror = pm.mirror
    --, palette = product |> getPalette
    }


adaptModelDecodeErrors : List ModelDecodeError -> M.Errors
adaptModelDecodeErrors modelDecodeErrors =
    let
        layerDecodeErrorToString index layerDecodeError =
            "(" ++ String.fromInt index ++ ") " ++
                case layerDecodeError of
                    KindDecodeFailed whyKindDecodeFailed ->
                        "Failed to decode kind: " ++ whyKindDecodeFailed
                    BlendDecodeFailed whyBlendDecodeFailed ->
                        "Failed to decode blend: " ++ whyBlendDecodeFailed
                    LayerCreationFailed whyLayerCreationFailed ->
                        "Failed to create layer: " ++ whyLayerCreationFailed
                    LayerModelDecodeFailed whyLayerModelDecodeFailed ->
                        "Failed to decode layer model: "
                            ++ D.errorToString whyLayerModelDecodeFailed
        modelDecodeErrorToString index modelDecodeError =
            "(" ++ String.fromInt index ++ ") " ++
                case modelDecodeError of
                    LayerDecodeErrors layerDecodeErrors ->
                        "Layers failed to decode: " ++
                            (layerDecodeErrors
                                |> List.indexedMap layerDecodeErrorToString
                                |> String.join "; ")
                    SizeRuleDecodeError whySizeRuleDecodeFailed ->
                        "Failed to decode sizeRule: " ++ whySizeRuleDecodeFailed
                    ProductDecodeError whyProductDecodeFailed ->
                        "Failed to decode product: " ++ whyProductDecodeFailed
    in
        modelDecodeErrors
            |> List.indexedMap modelDecodeErrorToString
            |> M.Errors
