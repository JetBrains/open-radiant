module Layer.Metaballs exposing
    ( Model
    , init
    , view
    )


import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)


v = 0.5
handleLenRate = 2.4
fill = "black"


type alias Model =
    {
    }


init : Model
init = {}


type alias Ball =
    { center : ( Int, Int )
    , radius: Int
    }


type alias Path = String

type Metaball = Metaball Path


smallCircles =
    [ (255, 129)
    , (610, 73)
    , (486, 363)
    , (117, 459)
    , (484, 726)
    , (843, 306)
    , (789, 615)
    , (1049, 82)
    , (1292, 428)
    , (1117, 733)
    , (1352, 86)
    , (92, 798)
    ]



metaball : Ball -> Ball -> Float -> Metaball
metaball ball1 ball2 maxDistance = Metaball ""


view : ( Int, Int ) -> Html a
view mouse =
    svg [ width "1000", height "1000" ]
        [ rect [ x "10", y "10", width "100", height "100", rx "15", ry "15" ] [] ]
