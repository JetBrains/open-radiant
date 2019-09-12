module LayerDef exposing (..)

import Json.Decode as D
import Json.Encode as E

type Kind
    = Html
    | WebGL
    | Canvas
    | JS


type alias LayerDef model view msg =
    { id : String
    , kind : Kind
    , encode : model -> E.Value
    , decode : D.Decoder model
    , update : msg -> model -> model
    , view : model -> view
    }
