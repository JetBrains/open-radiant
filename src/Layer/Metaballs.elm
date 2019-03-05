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


-- v = 0.5
handleLenRate = 2.5
distanceFactor = 1.5
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


type Color = Color String


type alias Transform = Vec2


type alias Circle =
    { origin : Vec2
    , radius: Float
    , tweens: List Tween
    , transform: Transform
    }


type alias Connection =
    { p1: Vec2, p2: Vec2, p3: Vec2, p4: Vec2
    , h1: Vec2, h2: Vec2, h3: Vec2, h4: Vec2
    , escaped: Bool
    , radius: Float
    , builtFrom: ( Circle, Circle )
    }


type alias Path = String

type ConnectionView
    = NoConnection
    | ConnectionView ( List ( Path, Color ) )


type Scene = Scene (List Group)


type Group = Group { circles : List Circle, connections: List ( Path, Color ) }


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


startFrom : ( Float, Float ) -> List (List Circle)
startFrom ( w, h ) =
    let
        group0 =
            [ circle ( 646.44, 251.24 ) 48
                [ translate (0, 0) (40, 0) 0 0.5
                , translate (0, 0) (-40, 0) 0.5 1
                ]
            , circle ( 545.65, 100.6 ) 72.5
                [ translate (0, 0) (-60, 0) 0 0.5
                , translate (0, 0) (60, 0) 0.5 1
                ]
            , circle ( 440.42, 250.07 ) 100
                [ translate (0, 0) (-10, 0) 0 0.5
                , translate (0, 0) (10, 0) 0.5 1
                ]
            , circle ( 107.42, 249.06 ) 56.42
                [ translate (0, 0) (-80, 0) 0 0.5
                , translate (0, 0) (80, 0) 0.5 1
                ]
            , circle ( 349.33, 225.06 ) 167.33
                [ translate (0, 0) (-5, 0) 0 0.5
                , translate (0, 0) (5, 0) 0.5 1
                ]
            ]
    in [ group0 ]


buildPath : Connection -> Path
buildPath { p1, p2, p3, p4, h1, h2, h3, h4, escaped, radius } =
    let
        vecstr vec = String.fromFloat (getX vec) ++ "," ++ String.fromFloat (getY vec)
    in
        String.join " "
            [ "M", vecstr p1
            , "C", vecstr h1, vecstr h3, vecstr p3
            , "A", String.fromFloat radius, String.fromFloat radius
                 , "0", if escaped then "1" else "0", "0", vecstr p4
            , "C", vecstr h4, vecstr h2, vecstr p2
            ]


connect : Circle -> Circle -> ConnectionView
connect circle1 circle2 =
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
        maxDistance = Basics.min (radius1 + radius2 * distanceFactor) globalMaxDistance
        -- maxDistance = radius1 + radius2
        halfPi = pi / 2
        d = distance center1 center2
        v = 0.5
    in
        -- No blob if a radius is 0
        -- or if distance between the circles is larger than max-dist
        -- or if circle2 is completely inside circle1
        if (radius1 <= 0 || radius2 <= 0) then
            NoConnection
        else if (d > maxDistance || d <= abs (radius1 - radius2)) then
            NoConnection
        else
            let
                circlesOverlap = d < (radius1 + radius2)

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
                d2Base = Basics.min (v * handleLenRate) (distance p1 p3 / totalRadius)
                -- Take into account when circles are overlapping
                d2 = d2Base * (Basics.min 1 (d * 2 / (radius1 + radius2)))

                -- Length of the handles
                handle1Length = radius1 * d2
                handle2Length = radius2 * d2

                -- Create the metaball
                theConnection =
                    { p1 = p1, p2 = p2, p3 = p3, p4 = p4
                    , h1 = vecAt p1 (angle1 - halfPi) handle1Length
                    , h2 = vecAt p2 (angle2 + halfPi) handle1Length
                    , h3 = vecAt p3 (angle3 + halfPi) handle2Length
                    , h4 = vecAt p4 (angle4 - halfPi) handle2Length
                    , escaped = d > radius1, radius = radius2
                    , builtFrom = ( circle1, circle2 )
                    }

                vecToSquarePath vec =
                    let locVec = sub vec center1
                    in case ( getX vec, getY vec ) of
                        ( x, y ) ->
                            [ "M", String.fromFloat x, String.fromFloat y
                            , "L", String.fromFloat <| x + 5, String.fromFloat y
                            , "L", String.fromFloat <| x + 5, String.fromFloat <| y + 5
                            , "L", String.fromFloat x, String.fromFloat <| y + 5
                            , "L", String.fromFloat x, String.fromFloat y
                            ] |> String.join " "
            in
                ConnectionView
                    [ ( buildPath theConnection, Color "url(#gradient)" )
                    , ( vecToSquarePath theConnection.h1, Color "blue" )
                    , ( vecToSquarePath theConnection.h2, Color "blue" )
                    , ( vecToSquarePath theConnection.h3, Color "blue" )
                    , ( vecToSquarePath theConnection.h4, Color "blue" )
                    , ( vecToSquarePath theConnection.p1, Color "green" )
                    , ( vecToSquarePath theConnection.p2, Color "green" )
                    , ( vecToSquarePath theConnection.p3, Color "green" )
                    , ( vecToSquarePath theConnection.p4, Color "green" )
                    ]


group : Float -> List Circle -> Group
group t circles =
    let
        animatedCircles = List.map (applyTweens t) circles
        indexedCircles =
            animatedCircles |> List.indexedMap Tuple.pair
        connections =
            List.foldr (\(i, circle1) allConnections ->
                allConnections ++
                    List.foldr (\(j, circle2) circleConnections ->
                        if (j < i) then
                            case connect circle1 circle2 of
                                NoConnection -> circleConnections
                                ConnectionView connection ->
                                    connection :: circleConnections
                        else circleConnections
                    ) [] indexedCircles
            ) [] indexedCircles
    in
        Group
            { circles = animatedCircles
            , connections = List.concat <| List.filter (not << List.isEmpty) connections
            }


scene : Float -> ( Float, Float )  -> ( Int, Int ) -> Scene
scene t ( w, h ) ( mouseX, mouseY ) =
    let
        circleAtCursor = Circle (vec2 (toFloat mouseX) (toFloat mouseY)) 100 [] (vec2 0 0)
    in
        Scene <| List.map (group t) <| startFrom ( w, h )


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
        (Scene groups) = scene t ( w, h ) mousePos
        drawCircle ({ origin, radius, transform })
            = S.circle
                [ SA.cx <| String.fromFloat <| getX origin
                , SA.cy <| String.fromFloat <| getY origin
                , SA.r  <| String.fromFloat radius
                , SA.transform <| extractTransform transform
                , SA.fill "url(#gradient)"
                ]
                [ ]
        drawPath ( pathStr, Color fillColor ) =
            S.path [ d pathStr, fill fillColor ] []
        -- gradient =
        --     S.linearGradient
        --         [ SA.id "gradient", SA.x1 "0", SA.x2 "0", SA.y1 "100%", SA.y2 "0"
        --         , SA.gradientUnits "userSpaceOnUse" ]
        --         [ S.stop [ SA.offset "0%", SA.stopColor "red" ] []
        --         , S.stop [ SA.offset "50%", SA.stopColor "black" ] []
        --         , S.stop [ SA.offset "100%", SA.stopColor "blue" ] []
        --         ]
        drawGroup (Group { circles, connections }) =
            S.g [ ]
                (
                List.map drawCircle circles ++
                List.map drawPath connections
                )
        gradient =
            S.radialGradient
                [ SA.id "gradient", SA.cx "593", SA.cy "402", SA.r "527.5685"
                , SA.gradientUnits "userSpaceOnUse" ]
                [ S.stop [ SA.offset "0.125", SA.stopColor "#E14729" ] []
                , S.stop [ SA.offset "0.2913", SA.stopColor "#D33450" ] []
                , S.stop [ SA.offset "0.4824", SA.stopColor "#9F1E59" ] []
                , S.stop [ SA.offset "0.6266", SA.stopColor "#89225D" ] []
                , S.stop [ SA.offset "0.9311", SA.stopColor "#4F2050" ] []
                ]
        defs = S.defs [ ] [ gradient ]
    in
        S.svg
            [ SA.width <| String.fromFloat w, height <| String.fromFloat h ]
            (defs :: List.map drawGroup groups)
