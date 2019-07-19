module Layer.NativeMetaballs exposing
    ( Model
    , PortModel
    , init
    , with
    , export
    , view
    )


import Model.Product as Product

import Html as H
import Html.Attributes as H


defaultColors = [ "#f38038", "#ed3d7d", "#341f49" ]


type alias Model =
    { colors : Product.Palette
    }


type alias PortModel = Product.Palette


init : Model
init =
    { colors = defaultColors }


export : Model -> PortModel
export = .colors


with : Product.Palette -> Model
with palette =
    { colors = palette }


view : Model -> H.Html a
view _ =
    H.canvas [ H.id "native-metaballs-0" ] [] -- FIXME: use actual layer index
