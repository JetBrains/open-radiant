module Layer.NativeMetaballs exposing
    ( Model
    , PortModel
    , init
    , prepare
    , view
    )


import Model.Product as Product

import Html as H
import Html.Attributes as H


type alias Model =
    {
    }


type alias PortModel = { }


init : Model
init = { }


prepare : Product.Product -> PortModel
prepare product = { }


view : Model -> H.Html a
view _ =
    H.canvas [ H.id "native-metaballs-0" ] [] -- FIXME: use actual layer index
