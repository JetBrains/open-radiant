module Layer.FSS.Export exposing (..)

import Layer.FSS.Model as FSS exposing (..)

import Json.Encode as E
import Json.Decode as D
import Json.Decode.Extra as D exposing (andMap)


encodeXY : (a -> E.Value) -> { x: a, y: a } -> E.Value
encodeXY f { x, y } =
    E.object
        [ ( "x", f x )
        , ( "y", f y )
        ]


encodeRenderMode : FSS.RenderMode -> String
encodeRenderMode mode =
    case mode of
        Triangles -> "triangles"
        Lines -> "lines"
        PartialLines -> "partial-lines"
        Points -> "points"


decodeRenderMode : String -> FSS.RenderMode
decodeRenderMode str =
    case str of
        "triangles" -> Triangles
        "lines" -> Lines
        "partial-lines" -> PartialLines
        "points" -> Points
        _ -> Triangles


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


encode : FSS.Model -> E.Value
encode fssModel =
        [ ( "renderMode", encodeRenderMode fssModel.renderMode |> E.string )
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
        ] |> E.object


decode : D.Decoder Model
decode =
    let
        make
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
                    { renderMode = decodeRenderMode renderModeStr
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
        D.succeed make
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
