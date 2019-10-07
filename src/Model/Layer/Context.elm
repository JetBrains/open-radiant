module Model.Layer.Context exposing (..)

import Model.Product exposing (Product)
import Model.AppMode exposing (AppMode)

type alias Context =
    { viewport : ()
    , mode : AppMode
    , product : Product
    }
