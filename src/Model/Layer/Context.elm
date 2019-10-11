module Model.Layer.Context exposing (..)

import Model.Product exposing (Product, Palette)
import Model.AppMode exposing (AppMode)

type alias Context =
    { viewport : ()
    , mode : AppMode
    , product : Product
    , size : ( Int, Int )
    , palette : Palette
    , origin : ( Int, Int )
    }
