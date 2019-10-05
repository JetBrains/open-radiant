module Layer.FSS exposing (..)

import Model.Layer.Def as Layer

import Layer.FSS.Model exposing (..)
import Layer.FSS.Model as Model exposing (init)
import Layer.FSS.Export as Export
import Layer.FSS.Render as Render

import WebGL

import Model.WebGL.Blend exposing (..)


type Msg
    = RebuildFss SerializedScene
    --| RebuildOnClient Layer.Index FSS.SerializedScene
    | ChangeRenderMode RenderMode
    | ChangeFaces Faces
    | AlterFaces FacesChange
    | ChangeLightSpeed Int
    | ChangeVignette Vignette
    | ChangeIris Iris
    | AlterAmplitude AmplitudeChange
    | ShiftColor ColorShiftPatch
    | ChangeOpacity Opacity


def : Layer.Def Model WebGL.Entity Msg Blend
def =
    { id = "fss"
    , kind = Layer.WebGL
    , init = Model.init
    , encode = Export.encode
    , decode = Export.decode
    , update = \msg model -> ( model, Cmd.none )-- FIXME
    , subscribe = always Sub.none -- FIXME
    , view = Render.view
    , gui = Nothing -- FIXME
    }
