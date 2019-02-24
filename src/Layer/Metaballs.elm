module Layer.Metaballs exposing
    ( Model
    , init
    , view
    )


import Html exposing (Html)
import Svg exposing (..)
import Svg as S exposing (path)
import Svg.Attributes exposing (..)


v = 0.5
handleLenRate = 2.4
ballsFill = "black"
maxDistance = 300


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



metaball : Ball -> Ball -> Metaball
metaball ball1 ball2 = Metaball ""


scene :  ( Int, Int ) -> ( List Ball, List Path )
scene mousePos =
    let
        balls =
            Ball mousePos 100 :: List.map (\center -> Ball center 50) smallCircles
        indexedBalls =
            balls |> List.indexedMap Tuple.pair
    in
        ( balls, [] )



view : ( Int, Int ) -> Html a
view mousePos =
    let
        ( balls, paths ) = scene mousePos
        drawBall { center, radius }
            = case center of
                ( ballX, ballY ) ->
                    circle
                        [ cx <| String.fromInt ballX
                        , cy <| String.fromInt ballY
                        , r <| String.fromInt radius
                        ]
                        []
        drawPath pathStr =
            S.path [ d pathStr, fill ballsFill ] []
    in
        svg [ width "1000", height "1000" ]
            ([ rect [ x "0", y "0", width "1000", height "1000", fill "white" ] [ ] ] ++
            List.map drawBall balls ++
            List.map drawPath paths)
