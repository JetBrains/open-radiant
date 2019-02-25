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
-- maxDistance = 300


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
        vecstr vec = String.fromFloat (getX vec) ++ "," ++ String.fromFloat (getY vec)
    in
        String.join " "
            [ "M", vecstr p1
            , "C", vecstr h1, vecstr h3, vecstr p3
            , "A", String.fromFloat radius, String.fromFloat radius
                 , "0", if escaped then "1" else "0", "0", vecstr p4
            --, "C", vecstr h4, vecstr h3, vecstr p4
            , "C", vecstr h4, vecstr h2, vecstr p2
            ]


metaball : Ball -> Ball -> Maybe Path
metaball ball1 ball2 =
    let
        vecAt center a r =
            let ( cx, cy ) = ( getX center, getY center )
            in
                vec2
                    (cx + r * cos a)
                    (cy + r * sin a)
        angleBetween vec1 vec2 =
            atan2 (getY vec1 - getY vec2) (getX vec1 - getX vec2)
        center1 = ball1.center
        center2 = ball2.center
        radius1 = ball1.radius
        radius2 = ball2.radius
        maxDistance = radius1 + radius2 * 2.5
        halfPi = pi / 2
        d = distance center1 center2
    in
        -- No blob if a radius is 0
        -- or if distance between the balls is larger than max-dist
        -- or if ball2 is completely inside ball1
        if (radius1 <= 0 || radius2 <= 0) then
            Nothing
        else if (d > maxDistance || d <= abs (radius1 - radius2)) then
            Nothing
        else
            let
                ballsOverlap = d < radius1 + radius2

                -- Calculate u1 and u2 if the balls are overlapping
                u1 =
                    if ballsOverlap then
                        acos <| (radius1 * radius1 + d * d - radius2 * radius2) / (2 * radius1 * d)
                    else 0
                u2 =
                    if ballsOverlap then
                        acos <| (radius2 * radius2 + d * d - radius1 * radius1) / (2 * radius2 * d)
                    else 0

                -- Calculate the max spread
                angleBetweenCenters = angleBetween center2 center1
                maxSpread = acos <| (radius1 - radius2) / d

                -- Angles for the points
                angle1 = angleBetweenCenters + u1 + (maxSpread - u1) * v
                angle2 = angleBetweenCenters - u1 - (maxSpread - u1) * v
                angle3 = angleBetweenCenters + pi - u2 - (pi - u2 - maxSpread) * v
                angle4 = angleBetweenCenters - pi + u2 + (pi - u2 - maxSpread) * v

                -- Point locations
                p1 = vecAt center1 angle1 radius1
                p2 = vecAt center1 angle2 radius1
                p3 = vecAt center2 angle3 radius2
                p4 = vecAt center2 angle4 radius2

                -- Define handle length by the distance between
                -- both ends of the curve
                totalRadius = radius1 + radius2
                d2Base = Basics.min (v * handleLenRate) (distance p1 p2 / totalRadius)
                -- Take into account when circles are overlapping
                d2 = d2Base * (Basics.min 1 (d * 2 / (radius1 + radius2)))

                -- Length of the handles
                sRadius1 = radius1 * d2
                sRadius2 = radius2 * d2

                -- Create the metaball
                theMetaball =
                    { p1 = p1, p2 = p2, p3 = p3, p4 = p4
                    , h1 = vecAt p1 (angle1 - halfPi) sRadius1
                    , h2 = vecAt p2 (angle2 + halfPi) sRadius1
                    , h3 = vecAt p3 (angle3 + halfPi) sRadius2
                    , h4 = vecAt p4 (angle4 - halfPi) sRadius2
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
