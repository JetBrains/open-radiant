module Layer.NativeMetaballs exposing
    ( Model
    , PortModel
    , init
    , prepare
    )


import Model.Product as Product


type alias Model =
    { palette : Maybe Product.Palette
    }


type alias PortModel = ( String, String, String )


init : Model
init = { palette = Nothing }


prepare : Product.Product -> PortModel
prepare product = ( "aaa", "bbb", "ccc" )
