module Layer.FSS.FSS exposing (..)

import Model.Layer.Def as Layer

import Layer.FSS.Model exposing (..)
import Layer.FSS.Model as Model exposing (init)
import Layer.FSS.Export as Export
import Layer.FSS.Render as Render
import Layer.FSS.Gui as Gui

import WebGL

import Model.WebGL.Blend exposing (..)


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
    , gui = Just Gui.gui
    }
