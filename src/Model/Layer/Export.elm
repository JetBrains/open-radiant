module Model.Layer.Export exposing (..)


import Json.Encode as E
import Json.Decode as D

import Model.Util exposing (..)
import Model.Layer.Def exposing (..)
import Model.Layer.Layer exposing (..)
import Model.Layer.Layer as Layer exposing (Model)
import Model.Layer.Context exposing (Context)

import Model.Layer.Blend.Html as HtmlBlend
import Model.Layer.Blend.WebGL as WGLBlend
import Model.Product exposing (Product)


type DecodeError
    = UnknownDefId String
    | UnknownBlend String
    | LayerModelDecodeFailed D.Error


encodeKind : Kind -> String
encodeKind kind =
    case kind of
        WebGL -> "webgl"
        Canvas -> "canvas"
        JS -> "js"
        Html -> "html"


decodeKind : String -> Maybe Kind
decodeKind str =
    case str of
        "webgl" -> Just WebGL
        "canvas" -> Just Canvas
        "js" -> Just JS
        "html" -> Just Html
        _ -> Nothing


encodeVisibility : Visibility -> String
encodeVisibility visibility =
    case visibility of
        Visible -> "visible"
        Hidden -> "hidden"
        Locked -> "locked"


decodeVisibility : String -> Maybe Visibility
decodeVisibility str =
    case str of
        "visible" -> Just Visible
        "hidden" -> Just Hidden
        "locked" -> Just Locked
        _ -> Nothing


encodeModel : Context -> Model -> Maybe E.Value
encodeModel ctx model =
    registry.byModel model
        |> Maybe.map (\def -> def.encode ctx model)


unknown = "<unknown>"


encodeForPort : Context -> Layer -> PortLayer
encodeForPort ctx (Layer props model as layer) =
    let
        unknownDef =
            { def = unknown
            , kind =  unknown
            , isOn = isOn layer
            , visible = encodeVisibility props.visibility
            , blend = encodePortBlend props.blend
            , zOrder = props.zOrder
            , index = props.index
            , opacity = props.opacity
            , model = model
                |> encodeModel ctx
                |> Maybe.withDefault (E.null)
            }
    in
        case registry.byModel model of
            Just def ->
                { unknownDef
                | def = def.id
                , kind =  encodeKind def.kind
                }
            Nothing ->
                unknownDef


decodeFromPort
    :  Context
    -> PortLayer
    -> Result (List DecodeError) Layer
decodeFromPort ctx portDef  =
    case registry.byId portDef.def of
        Just def ->
            portDef.model
                |> D.decodeValue (def.decode ctx)
                |> Result.mapError LayerModelDecodeFailed
                |> Result.map
                    (\model ->
                        Layer
                            { blend =
                                case decodeKind portDef.kind
                                    |> Maybe.withDefault Html of
                                    WebGL ->
                                        portDef.blend
                                            |> Tuple.first
                                            |> Maybe.withDefault WGLBlend.default
                                            -- TODO: produce BlendDecodeError?
                                            |> ForWebGL
                                    _ ->
                                        portDef.blend
                                            |> Tuple.second
                                            |> Maybe.map HtmlBlend.decode
                                            |> Maybe.withDefault HtmlBlend.default
                                            -- TODO: produce BlendDecodeError?
                                            |> ForHtml
                            , visibility =
                                decodeVisibility portDef.visible
                                    |> Maybe.withDefault Visible
                            , opacity = portDef.opacity
                            , zOrder = portDef.zOrder
                            , index = portDef.index
                            }
                            model
                    )
                |> Result.mapError List.singleton
        Nothing ->
            UnknownDefId portDef.def
                |> List.singleton
                |> Result.Err


encode : Context -> Layer -> E.Value
encode ctx (Layer props model as layer) =
    -- FIXME: store props inside the separate object
    [ ( "blend", encodeBlend props.blend |> E.string)
    , ( "blendDesc", encodeBlendDesc props.blend |> E.string )
    , ( "visible", encodeVisibility props.visibility |> E.string )
    , ( "opacity", props.opacity |> E.float )
    , ( "zOrder", props.zOrder |> E.int )
    , ( "index", props.index |> E.int )
    , ( "isOn", isOn layer |> E.bool )
    , ( "model", encodeModel ctx model
                |> Maybe.withDefault (E.string unknown) )
    -- , ( "mesh", E.string "" )
    ]
    ++ (
        case registry.byModel model of
            Just def ->
                [ ( "def", def.id |> E.string )
                , ( "kind", encodeKind def.kind |> E.string)
                ]
            Nothing ->
                [ ( "def", unknown |> E.string )
                , ( "kind", unknown |> E.string)
                ]
        )
    |> E.object


decode : Context -> D.Decoder Layer
decode ctx =
    let
        createLayer
            def
            kindStr
            index
            blendStr
            visibilityStr
            opacity
            zOrder
            layerModel =
                Layer
                    { blend =
                        case decodeKind kindStr
                            |> Maybe.withDefault Html of
                        WebGL ->
                            WGLBlend.decodeOne blendStr
                                |> Maybe.withDefault WGLBlend.default
                                -- TODO: produce BlendDecodeError?
                                |> ForWebGL
                        _ ->
                            HtmlBlend.decode blendStr
                                -- TODO: produce BlendDecodeError?
                                |> ForHtml
                    , visibility =
                        decodeVisibility visibilityStr
                            |> Maybe.withDefault Visible
                    , opacity = opacity
                    , zOrder = zOrder
                    , index = index
                    }
                    layerModel
    in
        D.field "def" D.string
            |> D.andThen
                (\defId ->
                    case registry.byId defId of
                        Just def ->
                            D.map7 (createLayer def)
                                (D.field "kind" D.string)
                                (D.field "index" D.int)
                                (D.field "blend" D.string)
                                (D.field "visible" D.string)
                                (D.field "opacity" D.float)
                                (D.field "zOrder" D.int)
                                (D.field "model" <| def.decode ctx)
                        Nothing ->
                            -- ( Hidden
                            -- , NoBlend
                            -- , Unknown
                            -- ) |> D.succeed
                            D.fail <| "unknown Def ID " ++ defId
            )
