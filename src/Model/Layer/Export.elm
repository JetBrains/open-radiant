module Model.Layer.Export exposing (..)


import Model.Layer.Blend.Html as HtmlBlend
import Model.Layer.Blend.WebGL as WGLBlend

import Json.Encode as E
import Json.Decode as D

import Model.Util exposing (..)
import Model.Layer.Def exposing (..)
import Model.Layer.Layer exposing (..)
import Model.Layer.Layer as Layer exposing (Model)


type DecodeError
    = UnknownDefId String
    | UnknownBlend String
    | LayerModelDecodeFailed D.Error


encode : Registry -> Layer -> E.Value
encode registry (( visibility, blend, model ) as layer) =
    case registry.byModel model of
        Just def ->
            E.object
                [ ( "def", def.id |> E.string )
                -- , ( "name", layerDef.name |> E.string )
                , ( "kind", encodeKind def.kind |> E.string)
                , ( "blend", encodeBlend blend |> E.string)
                , ( "blendDesc", encodeBlendDesc blend |> E.string )
                , ( "visible", encodeVisibility visibility |> E.string )
                , ( "isOn", isOn layer |> E.bool )
                , ( "model", encodeModel model |> Maybe.withDefault (E.string unknown) )
                -- , ( "mesh", E.string "" )
                ]
        Nothing ->
            E.object
                [ ( "def", unknown |> E.string )
                , ( "kind", unknown |> E.string )
                , ( "blend", unknown |> E.string )
                , ( "blendDesc", unknown |> E.string )
                , ( "isOn", False |> E.bool )
                , ( "visible", encodeVisibility Hidden |> E.string )
                , ( "model", unknown |> E.string )
                ]


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


encodeBlend : Blend -> String
encodeBlend blend =
    case blend of
        ForWebGL webglBlend ->
            WGLBlend.encodeOne webglBlend
        ForHtml htmlBlend ->
            HtmlBlend.encode htmlBlend
        _ -> unknown


encodePortBlend : Blend -> PortBlend
encodePortBlend blend =
    case blend of
        ForWebGL webglBlend ->
            ( Just webglBlend, Nothing )
        ForHtml htmlBlend ->
            ( Nothing, HtmlBlend.encode htmlBlend |> Just )
        _ ->
            ( Nothing, Nothing )


encodeBlendDesc : Blend -> String
encodeBlendDesc blend =
    case blend of
        ForWebGL webglBlend ->
            webglBlend
                |> WGLBlend.encodeHumanOne { delim = "; ", space = "> " }
        ForHtml htmlBlend ->
            HtmlBlend.encode htmlBlend
        _ -> unknown


encodeModel : Model -> Maybe E.Value
encodeModel model =
    registry.byModel model
        |> Maybe.map (\def -> def.encode model)


unknown = "<unknown>"


encodeForPort : Registry -> Layer -> PortDef
encodeForPort product (( visibility, blend, model ) as layer) =
    case registry.byModel model of
        Just def ->
            { def = def.id
            , kind =  encodeKind def.kind
            , isOn = isOn layer
            , visible = encodeVisibility visibility
            , blend = encodePortBlend blend
            , model = model
                |> encodeModel
                |> Maybe.withDefault (E.string unknown)
                |> E.encode 2
            }
        Nothing ->
            { def = unknown
            , kind =  unknown
            , isOn = isOn layer
            , visible = encodeVisibility visibility
            , blend = encodePortBlend blend
            , model = model
                |> encodeModel
                |> Maybe.withDefault (E.string unknown)
                |> E.encode 2
            }


decodeFromPort
    :  PortDef
    -> Result (List DecodeError) Layer
decodeFromPort portDef  =
    case registry.byId portDef.def of
        Just def ->
            portDef.model
                |> D.decodeString def.decode
                |> Result.mapError LayerModelDecodeFailed
                |> Result.map
                    (\model ->
                        ( decodeVisibility portDef.visible
                            |> Maybe.withDefault Hidden
                        , case decodeKind portDef.kind
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
                        , model
                        )
                    )
                |> Result.mapError List.singleton
        Nothing ->
            UnknownDefId portDef.def
                |> List.singleton
                |> Result.Err


decode : D.Decoder Layer
decode =
    let
        createLayer defId kindStr blendStr visibilityStr layerModelStr =
            case registry.byId defId of
                Just def ->
                    layerModelStr
                        |> D.decodeString def.decode
                        |> resultToDecoder_ D.errorToString
                        |> D.map
                            (\model ->
                                ( decodeVisibility visibilityStr
                                    |> Maybe.withDefault Hidden
                                , case decodeKind kindStr
                                        |> Maybe.withDefault Html of
                                    WebGL ->
                                        WGLBlend.decodeOne blendStr
                                            |> Maybe.withDefault WGLBlend.default
                                            -- TODO: produce BlendDecodeError?
                                            |> ForWebGL
                                    _ ->
                                        HtmlBlend.decode blendStr
                                            |> ForHtml
                                , model
                                )
                            )
                Nothing ->
                    ( Hidden
                    , NoBlend
                    , Unknown
                    ) |> D.succeed
    in
        D.map5 createLayer
            (D.field "def" D.string)
            (D.field "kind" D.string)
            (D.field "blend" D.string)
            (D.field "visible" D.string)
            (D.field "model" D.string)
            |> D.andThen identity
