module Model.Export exposing
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
import Browser.Navigation as Nav

import Math.Vector2 as Vec2

import Json.Decode as D exposing (bool, int, string, float, Decoder, Value)
import Json.Decode.Pipeline as D exposing (required, optional, hardcoded)
import Json.Decode.Extra as D exposing (andMap)
import Json.Encode as E exposing (encode, Value, string, int, float, bool, list, object)

import Algorithm.Gaussian as Gaussian

import Model.WebGL.Blend as WGLBlend
import Model.Html.Blend as HtmlBlend

import Gradient

import Model.Core as M
import Model.AppMode as Mode
import Model.Layer.Layer as Layer
import Model.SizeRule as SizeRule
import Model.Error as M
import Model.Product as Product exposing (Product, encode, decode, encodeGradient, decodeGradient)
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



encodeColor : { r: Float, g: Float, b: Float } -> E.Value
encodeColor { r, g, b } =
    E.list
        E.float
        [ r
        , g
        , b
        ]


encodeLayerDef : Product -> M.LayerDef -> E.Value
encodeLayerDef product layerDef =
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
        , ( "model", encodeLayerModel product layerDef.model )
        , ( "name", layerDef.name |> E.string )
        , ( "webglOrHtml", webglOrHtml layerDef |> E.string )
        -- , ( "mesh", E.string "" )
        ]


encodeModel_ : M.Model -> E.Value
encodeModel_ model =
    E.object
        [ ( "background", E.string model.background )
        , ( "mode", E.string <| Mode.encode model.mode )
        , ( "theta", E.float model.theta )
        , ( "omega", E.float model.omega )
        , ( "layers", E.list (encodeLayerDef model.product) model.layers )
        -- , ( "layers", E.list (List.filterMap
        --         (\layer -> Maybe.map encodeLayer layer) model.layers) )
        -- for b/w compatibility, we also encode size as numbers, but sizeRule is what should matter
        -- when it is defined/known on import
        , ( "size", encodeIntPair <| SizeRule.getRuleSizeOrZeroes model.size )
        , ( "sizeRule", E.string <| SizeRule.encode model.size )
        , ( "origin", encodeIntPair model.origin )
        , ( "mouse", encodeIntPair model.mouse )
        , ( "now", E.float model.now )
        , ( "palette",
            model.product
                |> Product.getPalette
                |> Product.encodePalette
                |> E.list E.string )
        , ( "product", model.product |> Product.encode |> E.string )
        ]


encodeModel : M.Model -> String
encodeModel model = model |> encodeModel_ |> E.encode 2


encodePortModel : M.Model -> M.PortModel
encodePortModel model =
    { background = model.background
    , mode = Mode.encode model.mode
    , now = model.now
    , theta = model.theta
    , omega = model.omega
    , layers = List.map (encodePortLayer model.product) model.layers
    , size = SizeRule.getRuleSize model.size |> Maybe.withDefault ( -1, -1 )
    , sizeRule = SizeRule.encode model.size |> Just
    , origin = model.origin
    , mouse = model.mouse
    , palette = model.product |> Product.getPalette |> Product.encodePalette
    , product = model.product |> Product.encode
    }


decodePortModel
    :  Nav.Key
    -> M.CreateLayer
    -> Product
    -> M.PortModel
    -> Result (List ModelDecodeError) M.Model
decodePortModel navKey createLayer product portModel =
    let
        couldBeDecodedLayers =
            List.map (decodePortLayer createLayer product) portModel.layers
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
                    SizeRule.decode sizeRuleStr
                        |> Result.mapError (List.singleton << SizeRuleDecodeError)
                Nothing -> case portModel.size of
                    ( w, h ) -> Ok <| SizeRule.Custom w h
        applyDecoded decodedLayers decodedSize decodedProduct =
            let
                modeResult = Mode.decode portModel.mode
                mode =
                    modeResult
                        |> Result.withDefault Mode.Production
                initialModel = M.initEmpty navKey mode
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
                    , product = decodedProduct -- Debug.log "decoded product" decodedProduct
                    }
            in
                { decodedModel
                | gui = case mode of
                    Mode.TronUi _ -> Just <| GUI.gui decodedModel
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



encodePortLayer : Product -> M.LayerDef -> M.PortLayerDef
encodePortLayer product layerDef =
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
        |> encodeLayerModel product
        |> E.encode 2
    }


decodePortLayer
    :  M.CreateLayer
    -> Product
    -> M.PortLayerDef
    -> Result (List LayerDecodeError) M.LayerDef
decodePortLayer createLayer product portLayerDef  =
    M.decodeKind portLayerDef.kind
        |> Result.mapError KindDecodeFailed
        |> Result.andThen
            (\kind ->
                portLayerDef.model
                    |> D.decodeString (layerModelDecoder kind product)
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


layerDefDecoder : M.CreateLayer -> Product -> D.Decoder M.LayerDef
layerDefDecoder createLayer product =
    let
        createLayerDef kindStr layerModelStr name isOn blendStr =
            M.decodeKind kindStr
                |> resultToDecoder
                |> D.andThen
                    (\kind ->
                        layerModelStr
                            |> D.decodeString (layerModelDecoder kind product)
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


modelDecoder : Nav.Key -> Mode.AppMode -> M.CreateLayer -> M.CreateGui -> D.Decoder M.Model
modelDecoder navKey currentMode createLayer createGui =
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
            product =
            let
                initialModel = M.init navKey currentMode [] createLayer createGui
                sizeResult =
                    case maybeSizeRule of
                        Just sizeRuleStr -> SizeRule.decode sizeRuleStr
                        Nothing -> case maybeSize of
                            Just (w, h) -> Ok <| SizeRule.Custom w h
                            Nothing -> Err "Unknown Size"
            in
                sizeResult
                    |> resultToDecoder
                    |> D.map
                        (\size ->
                            { initialModel
                            | background = background
                            , theta = theta
                            , omega = omega
                            , layers = layers
                            , size = size
                            , origin = origin
                            , mouse = mouse
                            , now = now
                            , product = product -- Debug.log "product decoded" product
                            --, palette = Product.getPalette product
                            }
                        )

    in
        -- case maybeSizeRule of
        --     Just sizeRuleStr -> M.decodeSizeRule sizeRuleStr
        --     Nothing -> case maybeSize of
        --         Just (w, h) -> M.Custom w h
        --         Nothing -> SizeRule.default
        D.field "product" D.string
            |> D.map (Product.decode >> resultToDecoder)
            |> D.andThen identity
            |> D.andThen
            (\product ->
                D.succeed createModel
                    |> D.andMap (D.field "background" D.string)
                    |> D.andMap (D.field "theta" D.float)
                    |> D.andMap (D.field "omega" D.float)
                    |> D.andMap (D.field "layers" (layerDefDecoder createLayer product |> D.list))
                    |> D.andMap (D.maybe (D.field "size" intPairDecoder))
                    |> D.andMap (D.maybe (D.field "sizeRule" D.string))
                    |> D.andMap (D.field "origin" intPairDecoder)
                    |> D.andMap (D.field "mouse" intPairDecoder)
                    |> D.andMap (D.field "now" D.float)
                    |> D.andMap (D.succeed product)
                    |> D.andThen identity
            )


decodeModel
     : Nav.Key
    -> Mode.AppMode
    -> M.CreateLayer
    -> M.CreateGui
    -> String
    -> Result String M.Model
decodeModel navKey currentMode createLayer createGui modelStr =
    D.decodeString (modelDecoder navKey currentMode createLayer createGui) modelStr
        |> Result.mapError D.errorToString



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
