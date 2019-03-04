module Layer.Metaballs exposing
    ( Model
    , init
    , view
    )

import Viewport exposing (Viewport)

import Math.Vector2 exposing (..)
import Array
import Html exposing (Html)
import Svg as S exposing (..)
import Svg.Attributes as SA exposing (..)


v = 0.5
handleLenRate = 2.5
--distanceFactor = 1
globalMaxDistance = 2000
circlesFill = "black"
loop = 4000.0


type Tween =
    Translate
        { from: Vec2
        , to: Vec2
        , start: Float
        , end: Float }


type alias Model =
    {
    }


init : Model
init = { }


type alias Transform = Vec2


type alias Circle =
    { origin : Vec2
    , radius: Float
    , tweens: List Tween
    , transform: Transform
    }


type alias Metaball =
    { p1: Vec2, p2: Vec2, p3: Vec2, p4: Vec2
    , h1: Vec2, h2: Vec2, h3: Vec2, h4: Vec2
    , escaped: Bool
    , radius: Float
    , builtFrom: ( Circle, Circle )
    }


type alias Path = String
type alias Segment =
    { point : Vec2
    , handleIn : Maybe Vec2
    , handleOut : Maybe Vec2
    }


circle : ( Float, Float ) -> Float -> List Tween -> Circle
circle ( x, y ) r tweens = Circle (vec2 x y) r tweens (vec2 0 0)


translate : ( Float, Float ) -> ( Float, Float ) -> Float -> Float -> Tween
translate ( x0, y0 ) ( x1, y1 ) start end =
    Translate { from = (vec2 x0 y0), to = (vec2 x1 y1), start = start, end = end }


initialCircles : ( Float, Float ) -> List Circle
initialCircles ( w, h ) =
    [ circle ( w / 2, h / 2 ) 70
        [ translate (0, 0) (w / 2, 0) 0 0.5
        , translate (0, 0) (-w / 4, 0) 0.5 1
        ]
    -- , circle ( w / 4, 1.7 * h / 3 ) 60
    --     [ translate (0, 0) (100, 0) 0 0.4
    --     , translate (0, 0) (-100, 0) 0.4 1.0
    --     ]
    , circle ( 1.3 * w / 2, h / 2 ) 100
        [ translate (0, 0) (-w / 4, 0) 0 0.5
        , translate (0, 0) (w / 4, 0) 0.5 1
        ]
    -- , circle ( 3 * w / 4, h / 2 ) 16
    --     [ translate (0, 0) (200, -50) 0 0.3
    --     , translate (0, 0) (-100, 25) 0.3 0.7
    --     , translate (0, 0) (-100, 25) 0.7 1.0
    --     ]
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


metaball : Circle -> Circle -> Maybe Path
metaball circle1 circle2 =
    let
        vecAt center a r =
            let ( cx, cy ) = ( getX center, getY center )
            in
                vec2
                    (cx + r * cos a)
                    (cy + r * sin a)
        angleBetween vec1 vec2 =
            atan2 (getY vec1 - getY vec2) (getX vec1 - getX vec2)
        center1 = add circle1.origin circle1.transform
        center2 = add circle2.origin circle2.transform
        radius1 = circle1.radius
        radius2 = circle2.radius
        --maxDistance = Basics.min (radius1 + radius2 * distanceFactor) globalMaxDistance
        maxDistance = radius1 + radius2
        halfPi = pi / 2
        d = distance center1 center2
        --v = d / 400
    in
        -- No blob if a radius is 0
        -- or if distance between the circles is larger than max-dist
        -- or if circle2 is completely inside circle1
        if (radius1 <= 0 || radius2 <= 0) then
            Nothing
        else if (d > maxDistance || d <= abs (radius1 - radius2)) then
            Nothing
        else
            let
                circlesOverlap = d < radius1 + radius2

                -- Calculate u1 and u2 if the circles are overlapping
                u1 =
                    if circlesOverlap then
                        acos <| (radius1 * radius1 + d * d - radius2 * radius2) / (2 * radius1 * d)
                    else 0
                u2 =
                    if circlesOverlap then
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
                    , builtFrom = ( circle1, circle2 )
                    }
            in
                Just <| buildPath theMetaball


scene : Float -> ( Float, Float )  -> ( Int, Int ) -> ( List Circle, List Path )
scene t ( w, h ) ( mouseX, mouseY ) =
    let
        circleAtCursor = Circle (vec2 (toFloat mouseX) (toFloat mouseY)) 100 [] (vec2 0 0)
        animatedInitialCircles = List.map (applyTweens t) <| initialCircles ( w, h )
        circles =
            circleAtCursor :: animatedInitialCircles
            --animatedInitialCircles
        indexedCircles =
            circles |> List.indexedMap Tuple.pair
        connections =
            List.foldr (\(i, circle1) allConnections ->
                allConnections ++
                    List.foldr (\(j, circle2) circleConnections ->
                        if (j < i) then
                            metaball circle1 circle2 :: circleConnections
                        else circleConnections
                    ) [] indexedCircles
            ) [] indexedCircles
    in
        ( circles
        , List.filterMap identity connections
        )


getLocT : Float -> Float -> Float -> Float
getLocT start end globt =
    let
        -- _ = Debug.log "globt" globt
        loct = (globt - (floor (globt / loop) |> toFloat) * loop) / loop
        clamped = clamp start end loct
    in
        clamped / (end - start)


applyTweens : Float -> Circle -> Circle
applyTweens t toCircle =
    let
        applyPos t_ tween ( curX, curY ) =
            case tween of
                Translate { from, to, start, end } ->
                    let tloc = getLocT start end t_
                    in
                        case ( ( getX from, getY from ), ( getX to, getY to ) ) of
                            ( ( fromX, fromY ), ( toX, toY ) ) ->
                                ( curX + fromX + ((toX - fromX) * tloc)
                                , curY + fromY + ((toY - fromY) * tloc)
                                )

        translateTo =
            List.foldl (applyPos t) (0, 0) toCircle.tweens
    in case translateTo of
        ( x, y ) ->
            { toCircle
            | transform = vec2 x y
            }


extractTransform : Transform -> String
extractTransform transform =
    case ( getX transform, getY transform ) of
        ( tx, ty ) ->
            "translate(" ++ String.fromFloat tx ++ "," ++ String.fromFloat ty ++ ")"


view : Viewport {} -> Float -> Float -> ( Int, Int ) -> Html a
view vp t dt mousePos =
    let
        -- _ = Debug.log "t" t
        ( w, h ) = ( getX vp.size, getY vp.size )
        ( circles, metaballs ) = scene t ( w, h ) mousePos
        drawCircle ({ origin, radius, transform })
            = S.circle
                [ SA.cx <| String.fromFloat <| getX origin
                , SA.cy <| String.fromFloat <| getY origin
                , SA.r  <| String.fromFloat radius
                , SA.transform <| extractTransform transform
                ]
                [ ]
        drawMetaball pathStr =
            S.path [ d pathStr, fill circlesFill ] []
    in
        S.svg [ SA.width <| String.fromFloat w, height <| String.fromFloat h ]
            (
            List.map drawCircle circles ++
            List.map drawMetaball metaballs
            )
