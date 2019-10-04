module Model.Layer.Def exposing (..)

import Gui.Def exposing (Nest)

import Json.Decode as D
import Json.Encode as E


type Kind
    = Html
    | WebGL
    | Canvas
    | JS


type alias Def model view msg blend =
    { id : String
    , kind : Kind
    , init : model
    , encode : model -> E.Value
    , decode : D.Decoder model
    , update : msg -> model -> ( model, Cmd msg )
    , view : model -> Maybe blend -> view
    , subscribe : model -> Sub msg
    , gui : Maybe (Nest msg)
    }
