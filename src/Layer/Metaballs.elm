module Layer.Metaballs exposing
    ( Model
    , init
    , view
    )


import Math.Vector2 exposing (..)
import Array
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
    { center : Vec2
    , radius: Float
    }


type alias Metaball =
    { p1: Vec2, p2: Vec2, p3: Vec2, p4: Vec2
    , h1: Vec2, h2: Vec2, h3: Vec2, h4: Vec2
    , escaped: Bool
    , radius: Float
    }


type alias Path = String
type alias Segment =
    { point : Vec2
    , handleIn : Maybe Vec2
    , handleOut : Maybe Vec2
    }


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


buildPath : Metaball -> Path
buildPath { p1, p2, p3, p4, h1, h2, h3, h4, escaped, radius } =
    let
        vecstr vec = String.fromFloat (getX vec) ++ " " ++ String.fromFloat (getY vec)
    in
        String.join " "
            [ "M", vecstr p1
            , "C", vecstr h1, vecstr h3, vecstr p3
            , "A", String.fromFloat radius, String.fromFloat radius
                 , "0", if escaped then "1" else "0", "0", vecstr p4
            , "C", vecstr h4, vecstr h3, vecstr p4
            ]


metaball : Ball -> Ball -> Maybe Path
metaball ball1 ball2 =
    let
        vecAt rads len =
            case fromPolar ( len, degrees rads ) of
                ( x, y ) -> vec2 x y
        center1 = ball1.center
        center2 = ball2.center
        radius1 = ball1.radius
        radius2 = ball2.radius
        pi2 = pi / 2
        d = distance center1 center2
    in
        if (radius1 <= 0 || radius2 <= 0) then
            Nothing
        else if (d > maxDistance || d <= abs (radius1 - radius2)) then
            Nothing
        else
            let
                ballsOverlap = d < radius1 + radius2
                u1 =
                    if ballsOverlap then
                        acos <| (radius1 * radius1 + d * d - radius2 * radius2) / (2 * radius1 * d)
                    else 0
                u2 =
                    if ballsOverlap then
                        acos <| (radius2 * radius2 + d * d - radius1 * radius1) / (2 * radius2 * d)
                    else 0
                angle1 = let vc = sub center2 center1 in atan2 (getX vc) (getY vc)
                angle2 = acos <| (radius1 - radius2) / d
                angle1a = angle1 + u1 + (angle2 - u1) * v
                angle1b = angle1 - u1 - (angle2 - u1) * v
                angle2a = angle1 + pi - u2 - (pi - u2 - angle2) * v
                angle2b = angle1 - pi + u2 + (pi - u2 - angle2) * v
                p1a = add center1 <| vecAt angle1a radius1
                p1b = add center1 <| vecAt angle1b radius1
                p2a = add center2 <| vecAt angle2a radius2
                p2b = add center2 <| vecAt angle2b radius2

                -- define handle length by the distance between
                -- both ends of the curve to draw
                totalRadius = radius1 + radius2
                d2 = Basics.min (v * handleLenRate) <| length (sub p1a p2a) / totalRadius

                scaledRadius1 = radius1 * d2
                scaledRadius2 = radius2 * d2

                theMetaball =
                    { p1 = p1a, p2 = p1b, p3 = p2a, p4 = p2b
                    , h1 = vecAt (angle1a - pi2) radius1, h2 = vecAt (angle2a + pi2) radius2
                    , h3 = vecAt (angle2b - pi2) radius2, h4 = vecAt (angle1b + pi2) radius1
                    , escaped = d > radius1, radius = radius2
                    }
            in
                Just <| buildPath theMetaball


scene :  ( Int, Int ) -> ( List Ball, List Path )
scene ( mouseX, mouseY ) =
    let
        ballAtCursor = Ball (vec2 (toFloat mouseX) (toFloat mouseY)) 100
        smallCircleToBall (cx, cy) = Ball (vec2 cx cy) 50
        balls =
            ballAtCursor :: List.map smallCircleToBall smallCircles
        indexedBalls =
            balls |> List.indexedMap Tuple.pair
        connections =
            List.foldr (\(i, ball1) allConnections ->
                allConnections ++
                    List.foldr (\(j, ball2) ballConnections ->
                        if (j < i) then
                            metaball ball1 ball2 :: ballConnections
                        else ballConnections
                    ) [] indexedBalls
            ) [] indexedBalls
    in
        ( balls, List.filterMap identity connections )



view : ( Int, Int ) -> Html a
view mousePos =
    let
        ( balls, connections ) = scene mousePos
        drawBall { center, radius }
            = circle
                [ cx <| String.fromFloat <| getX center
                , cy <| String.fromFloat <| getY center
                , r  <| String.fromFloat radius
                ]
                [ ]
        drawConnection pathStr =
            S.path [ d pathStr, fill ballsFill ] []
    in
        svg [ width "1000", height "1000" ]
            ([ rect
                [ x "0", y "0", width "1000", height "1000", fill "white" ] [ ]
             ] ++
            List.map drawBall balls ++
            List.map drawConnection connections
            )
