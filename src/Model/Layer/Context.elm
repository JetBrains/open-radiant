module Model.Layer.Context exposing (..)

import Model.Product exposing (Product)

type alias Context =
    { viewport : {}
    , product : Product
    }
