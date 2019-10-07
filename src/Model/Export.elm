module Model.Export exposing
    ( encode
    , decode
    , encodeToString
    , encodeForPort
    , decodeFromPort
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

import Model.Layer.Blend.Html as HtmlBlend
import Model.Layer.Blend.WebGL as WGLBlend
import Model.Util exposing (resultToDecoder, intPairDecoder)

import Gradient

import Model.Core as M
import Model.AppMode as Mode
import Model.Layer.Layer as Layer
import Model.Layer.Export as Layer
import Model.Layer.Context exposing (Context)
import Model.SizeRule as SizeRule
import Model.Error as M
import Model.Product as Product exposing (Product, encode, decode, encodeGradient, decodeGradient)
import Model.Range exposing (..)

import TronGui as GUI


-- FIXME: Move all the decoding/encoding to the corresponding layers


type ModelDecodeError
    = LayerDecodeErrors (List Layer.DecodeError)
    | SizeRuleDecodeError String
    -- | ModeDecodeError String
    | ProductDecodeError String


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


encode : M.Model -> E.Value
encode model =
    E.object
        [ ( "background", E.string model.background )
        , ( "mode", E.string <| Mode.encode model.mode )
        , ( "theta", E.float model.theta )
        , ( "omega", E.float model.omega )
        , ( "layers", E.list (Layer.encode <| M.getContext model) model.layers )
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


encodeToString : M.Model -> String
encodeToString model = model |> encode |> E.encode 2


encodeForPort : M.Model -> M.PortModel
encodeForPort model =
    { background = model.background
    , mode = Mode.encode model.mode
    , now = model.now
    , theta = model.theta
    , omega = model.omega
    , layers = List.map (Layer.encodeForPort <| M.getContext model) model.layers
    , size = SizeRule.getRuleSize model.size |> Maybe.withDefault ( -1, -1 )
    , sizeRule = SizeRule.encode model.size |> Just
    , origin = model.origin
    , mouse = model.mouse
    , palette = model.product |> Product.getPalette |> Product.encodePalette
    , product = model.product |> Product.encode
    }


decodeFromPort
    :  Nav.Key
    -> Context
    -> M.PortModel
    -> Result (List ModelDecodeError) M.Model
decodeFromPort navKey ctx portModel =
    let
        couldBeDecodedLayers =
            List.map (Layer.decodeFromPort ctx) portModel.layers
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
                initialModel = M.init navKey mode
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



decode : Nav.Key -> Context -> M.CreateGui -> D.Decoder M.Model
decode navKey ctx createGui =
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
                initialModel =
                    M.init navKey ctx.mode
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
                    |> D.map
                        (\newModel ->
                            { newModel
                            | gui = case ctx.mode of
                                Mode.TronUi innerAppMode ->
                                    Just <| createGui { newModel | mode = innerAppMode }
                                _ -> Nothing
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
                    |> D.andMap (D.field "layers" (Layer.decode ctx |> D.list))
                    |> D.andMap (D.maybe (D.field "size" intPairDecoder))
                    |> D.andMap (D.maybe (D.field "sizeRule" D.string))
                    |> D.andMap (D.field "origin" intPairDecoder)
                    |> D.andMap (D.field "mouse" intPairDecoder)
                    |> D.andMap (D.field "now" D.float)
                    |> D.andMap (D.succeed product)
                    |> D.andThen identity
            )


decodeToResult
     : Nav.Key
    -> Context
    -> M.CreateGui
    -> String
    -> Result String M.Model
decodeToResult navKey ctx createGui modelStr =
    D.decodeString (decode navKey ctx createGui) modelStr
        |> Result.mapError D.errorToString



adaptModelDecodeErrors : List ModelDecodeError -> M.Errors
adaptModelDecodeErrors modelDecodeErrors =
    let
        layerDecodeErrorToString index layerDecodeError =
            "(" ++ String.fromInt index ++ ") " ++
                case layerDecodeError of
                    Layer.UnknownDefId whyKindDecodeFailed ->
                        "Failed to decode kind: " ++ whyKindDecodeFailed
                    Layer.UnknownBlend whyBlendDecodeFailed ->
                        "Failed to decode blend: " ++ whyBlendDecodeFailed
                    -- LayerCreationFailed whyLayerCreationFailed ->
                    --     "Failed to create layer: " ++ whyLayerCreationFailed
                    Layer.LayerModelDecodeFailed whyLayerModelDecodeFailed ->
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
