module Layer.FSS.Gui exposing (..)

import Layer.FSS.Model exposing (..)

import Gui.Def exposing (..)
import Gui.Nest exposing (..)

gui : Model -> {- WGLBlend.Blend -> -} Nest Msg
gui fssModel {-currentBlend-} =
    oneLine [ Ghost "fss" ]

    -- FIXME: implement

    {-
    let
        { lightSpeed, faces, amplitude, vignette, iris, colorShift } = fssModel
        { amplitudeX, amplitudeY, amplitudeZ } = amplitude
        changeFacesX val = AlterFaces layerIndex
                                    { xChange = Just <| round val, yChange = Nothing }
        changeFacesY val = AlterFaces layerIndex
                                    { xChange = Nothing, yChange = Just <| round val }
        changeAmplutudeX val = AlterAmplitude layerIndex
                                    <| FSS.AmplitudeChange (Just val) Nothing Nothing
        changeAmplutudeY val = AlterAmplitude layerIndex
                                    <| FSS.AmplitudeChange Nothing (Just val) Nothing
        changeAmplutudeZ val = AlterAmplitude layerIndex
                                    <| FSS.AmplitudeChange Nothing Nothing (Just val)
        changeHue val = ShiftColor layerIndex
                                    <| FSS.ColorShiftPatch (Just val) Nothing Nothing
        changeSaturation val = ShiftColor layerIndex
                                    <| FSS.ColorShiftPatch Nothing (Just val) Nothing
        changeBrightness val = ShiftColor layerIndex
                                    <| FSS.ColorShiftPatch Nothing Nothing (Just val)
        toggleMirror state =
            layerIndex |> if (state == TurnedOn) then MirrorOn else MirrorOff
        toggleVisibility state =
            layerIndex |> if (state == TurnedOn) then TurnOn else TurnOff
        chooseMesh _ label =
            FSS.decodeRenderMode label |> ChangeFssRenderMode layerIndex
        defaultKnobSetup defaultVal =
            { min = 0.0, max = 1.0, step = 0.05, roundBy = 100, default = defaultVal }
        lightSpeedSetup =
            { min = 0.0, max = 2000.0, step = 1.0, roundBy = 1
            , default = toFloat lightSpeed }
        facesKnobSetup defaultVal =
            { min = 0.0, max = 100.0, step = 1.0, roundBy = 1, default = defaultVal }
        colorShiftKnobSetup defaultVal =
            { min = -1.0, max = 1.0, step = 0.05, roundBy = 100, default = defaultVal }
    in
        oneLine
            [ Toggle "visible" TurnedOn toggleVisibility
            , Toggle "mirror" TurnedOff toggleMirror
            -- , Knob "opacity" TurnedOff <| toggleMirror layerIndex
            , Knob "lights" lightSpeedSetup (toFloat lightSpeed)
                <| round >> ChangeLightSpeed layerIndex
            , Knob "col"
                (facesKnobSetup <| toFloat faces.x)
                (toFloat faces.y)
                changeFacesX
            , Knob "row"
                (facesKnobSetup <| toFloat faces.y)
                (toFloat faces.y)
                changeFacesY
            , Nested "fog" Collapsed <|
                nestWithin ( 2, 1 )
                    [ Knob "shine"
                        (defaultKnobSetup vignette)
                        vignette <| ChangeVignette layerIndex
                    , Knob "density"
                        (defaultKnobSetup iris)
                        iris <| ChangeIris layerIndex
                    ]
            , Choice "mesh" Collapsed 0 chooseMesh <|
                nestWithin ( 2, 1 )
                    [ ChoiceItem "triangles"
                    , ChoiceItem "lines"
                    ]
            , Nested "ranges" Collapsed <|
                    nestWithin ( 3, 1 )
                        [ Knob "horizontal"
                            (defaultKnobSetup amplitudeX)
                            amplitudeX changeAmplutudeX
                        , Knob "vertical"
                            (defaultKnobSetup amplitudeY)
                            amplitudeY changeAmplutudeY
                        , Knob "depth"
                            (defaultKnobSetup amplitudeZ)
                            amplitudeZ changeAmplutudeZ
                        ]
            , Nested "hsb" Collapsed <|
                nestWithin ( 3, 1 )
                    [ Knob "hue"
                        (colorShiftKnobSetup colorShift.hue)
                        colorShift.hue changeHue
                    , Knob "saturation"
                        (colorShiftKnobSetup colorShift.saturation)
                        colorShift.saturation changeSaturation
                    , Knob "brightness"
                        (colorShiftKnobSetup colorShift.brightness)
                        colorShift.brightness changeBrightness
                    ]
            , Nested "blend" Collapsed
                <| webglBlendGrid mode currentBlend layerIndex
            ]
    -}
