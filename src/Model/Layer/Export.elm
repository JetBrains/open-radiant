module Model.Layer.Export exposing (..)


import Model.Layer.Blend.Html as HtmlBlend
import Model.Layer.Blend.WebGL as WGLBlend

import Json.Encode as E
import Json.Decode as D

import Model.Layer.Def exposing (..)
import Model.Layer.Layer exposing (..)


encodeKind : Kind -> String
encodeKind kind =
     case kind of
        WebGL -> "webgl"
        Canvas -> "canvas"
        JS -> "js"
        Html -> "html"


encodeBlend : Kind -> Blend -> E.Value
encodeBlend kind blend =
    case blend of
        ForWebGL webglBlend ->
            WGLBlend.encodeOne webglBlend |> E.string
        ForHtml htmlBlend ->
            HtmlBlend.encode htmlBlend |> E.string


encode : Registry -> Layer -> E.Value
encode registry ( visibility, blend, model )  =
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
    -> Layer.PortLayerDef
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


layerDefDecoder : Product -> D.Decoder M.LayerDef
layerDefDecoder product =
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
