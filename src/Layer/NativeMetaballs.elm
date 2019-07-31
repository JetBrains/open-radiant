module Layer.NativeMetaballs exposing
    ( Model
    -- , PortModel
    , init
    -- , export
    , view
    , generate
    , generator
    , Variety
    , Orbit
    )


import Html as H
import Html.Attributes as H

import Model.Product as Product
import Layer.Fluid as Fluid exposing (Model, generate, generator, Variety(..), Orbit(..))

import Random


defaultColors = [ "#f38038", "#ed3d7d", "#341f49" ]


type alias Model = Fluid.Model


type alias Variety = Fluid.Variety
type alias Orbit = Fluid.Orbit


-- type alias PortModel =
--     { palette: Product.Palette
--     , temp : String
--     }


init : Model
init = Fluid.init


-- export : Model -> PortModel
-- export model =
--     { palette = model.colors
--     , temp = ""
--     }


view : Model -> H.Html a
view _ =
    H.canvas [ H.id "native-metaballs-0"] [] -- FIXME: use actual layer index


generate : (Model -> msg) -> Random.Generator Model -> Cmd msg
generate =
    Fluid.generate


generator
    :  ( Int, Int )
    -> Fluid.Variety
    -> Fluid.Orbit
    -> Product.Palette
    -> Random.Generator Model
generator =
    Fluid.generator
