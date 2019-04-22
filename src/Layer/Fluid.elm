module Layer.Fluid exposing
    ( Model
    , Mesh
    , BallGroup
    , Base64Url(..)
    , TextureAndSize
    , GradientStops, GradientOrientation(..)
    , makeEntities
    , build
    , init
    , loadTextures, injectTextures, packTextures
    , generate, generator
    , numberOfGroups, numberOfBalls
    , radiusRange, speedRange
    , multArcX, multArcY
    , originOffset
    )

import Array exposing (Array)
import Random
import Task

import Animation exposing (..)
import Ease exposing (..)

import Math.Vector3 as Vec3 exposing (..)
import Math.Vector2 as Vec2 exposing (..)

import Algorithm.Base64.BMP exposing (encode24With)
import Algorithm.Base64.Image exposing (..)

import WebGL
import WebGL.Settings exposing (Setting)
import WebGL.Texture as Texture
import WebGL.Texture exposing (Texture)

import Viewport exposing (Viewport)

import Model.Product as Product
import Model.Range exposing (..)


type alias Ball =
    { origin : Vec2
    , radius : Float
    , speed : Float
    , t : Float
    , arcMult : Vec2
    }


type alias BallGroup =
    { balls: List Ball
    , textures:
        Maybe
            { gradient : TextureAndSize
            , data : TextureAndSize
            }
    , gradient: Maybe
        { stops: GradientStops
        , orientation: GradientOrientation
        }
    }


type alias Model =
    { groups: List BallGroup
    }


type alias Mesh = WebGL.Mesh Vertex
type alias Time = Float


type alias TextureAndSize = ( Texture, Vec2 )


type Base64Url = Base64Url String


-- type alias GradientsToLoad =
--     List Base64Url


type alias ColorStop =
    ( Float, Product.Color )


type alias GradientStops = List ColorStop


type GradientOrientation
    = Horizontal
    | Vertical


init : Model
init =
    { groups = [ ]
    }


numberOfGroups = iRange 2 5
numberOfBalls  = iRange 5 30
radiusRange    = fRange 5 50
speedRange     = fRange 0.2 2.0
multArcX       = fRange -0.25 0.75
multArcY       = fRange -0.25 0.25
originOffset   = vec2 0.65 0.45


speedTextureMultiplier = 500
multArcTextureMultiplier = 500
tTextureMultiplier = 4


generator : ( Int, Int ) -> Product.Palette -> Random.Generator Model
generator ( w, h ) palette =
    let
        paletteLen = List.length palette
        loopedPalette = palette ++ (palette |> List.drop 1 |> List.reverse |> List.drop 1)
        loopedPaletteLen = List.length loopedPalette
        loopedPaletteArray = Array.fromList loopedPalette
        addColors shift stops =
            stops |>
                List.indexedMap
                    (\index stop ->
                        let
                            loopedPaletteIndex = modBy loopedPaletteLen <| index + shift
                        in
                            ( stop
                            , Array.get loopedPaletteIndex loopedPaletteArray
                                |> Maybe.withDefault  ""
                            )
                    )
        generatePosition =
            Random.map2 vec2
                (Random.float 0 <| toFloat w)
                (Random.float 0 <| toFloat h)
        generateRadius = randomFloatInRange radiusRange
        generateSpeed = randomFloatInRange speedRange
        generateT = Random.float 0 200
        generateMultArc =
            Random.map2 vec2
                (randomFloatInRange multArcX)
                (randomFloatInRange multArcY)
        generateStopPosition = Random.float 0 1
        generateStopsWithColors =
            Random.int 2 4
                |> Random.andThen
                    (\numStops ->
                        generateStopPosition |> Random.list numStops
                    )
                |> Random.andThen
                    (\stops ->
                        Random.int 0 paletteLen
                            |> Random.map (\shift -> addColors shift stops)
                    )
        generateGroup =
            randomIntInRange numberOfBalls
                |> Random.andThen
                    (\numCircles ->
                        Random.map5
                            Ball
                            generatePosition
                            generateRadius
                            generateSpeed
                            generateT
                            generateMultArc
                            |> Random.list numCircles
                    )
                |> Random.andThen
                    (\balls ->
                        generateStopsWithColors
                            |> Random.map
                                (\stopsWithColors ->
                                    ( balls, stopsWithColors )
                                )
                    )
                |> Random.andThen
                    (\(balls, stops) ->
                        Random.float 0 1
                            |> Random.map
                                (\v -> if v >= 0.5 then Horizontal else Vertical)
                            |> Random.map
                                (\orientation -> (balls, stops, orientation))
                    )
                |> Random.map
                    (\(balls, stops, orientation) ->
                        { balls = balls
                        , textures = Nothing
                        , gradient = Just { stops = stops, orientation = orientation }
                        }
                    )
        makeBall { center, radius } = Ball center radius
    in
        randomIntInRange numberOfGroups
            |> Random.andThen
                (\numGroups ->
                    Random.list numGroups generateGroup
                )
            |> Random.map (\groups -> { groups = groups })


generate : (Model -> msg) -> Random.Generator Model -> Cmd msg
generate = Random.generate


makeDataTexture : List Ball -> ( Base64Url, Vec2 )
makeDataTexture balls =
    let addBallData ball prevData =
            prevData ++
                [ floor <| Vec2.getX ball.origin -- 0.
                , floor <| Vec2.getY ball.origin -- 1.
                , floor ball.radius              -- 2.
                , 0                              -- 3.
                , floor <| speedTextureMultiplier * ball.speed               -- 4.
                , floor <| tTextureMultiplier * ball.t                       -- 5.
                , floor <| multArcTextureMultiplier * Vec2.getX ball.arcMult -- 6.
                , floor <| multArcTextureMultiplier * Vec2.getY ball.arcMult -- 7.
                ]
        data = balls |> List.foldl addBallData []
        dataLen =  List.length data
        width = 4
        -- FIXME: could be not enough height for all the data
        maxNumberOfBalls = getIntMax numberOfBalls
        height = (maxNumberOfBalls + modBy 4 maxNumberOfBalls) * 2
    in
        ( Base64Url <| encode24With  width height data  {defaultOptions | order = RightUp}
        , vec2 (toFloat width) (toFloat height)
        )


packTextures : List TextureAndSize -> List { gradient : TextureAndSize, data : TextureAndSize }
packTextures textures =
    let
        packTexture items =
            case items of
                a::b::xs ->
                    { gradient = a
                    , data = b
                    } :: packTexture xs
                _ -> []
    in packTexture textures


injectTextures : List { gradient : TextureAndSize, data : TextureAndSize } -> Model -> Model
injectTextures textures model =
    let
        addTexture group texturePair =
            { group | textures = Just texturePair }
    in
        { model | groups = List.map2 addTexture model.groups textures }


makeEntity
    :  Time
    -> Viewport {}
    -> List Setting
    -> Mesh
    -> List Ball
    -> { gradient : ( Texture, Vec2 ) , data : ( Texture, Vec2 ) }
    -> WebGL.Entity
makeEntity now viewport settings mesh balls textures  =
    WebGL.entityWith
        settings
        vertexShader
        fragmentShader
        mesh
        (uniforms now balls textures.gradient textures.data viewport)


-- TODO: add mouse
makeEntities : Time -> Viewport {} -> Model -> List Setting -> Mesh -> List WebGL.Entity
makeEntities now viewport model settings mesh =
    let
        makeGroupEntity group =
            group.textures
                |> Maybe.map
                    (\textures ->
                        makeEntity now viewport settings mesh group.balls textures
                    )
    in
        model.groups |> List.filterMap makeGroupEntity


-- Mesh


type alias Vertex =
    { position : Vec3
    }


build : Model -> Mesh
build model =
    let
        topLeft =
            Vertex (vec3 -1 1 0)

        topRight =
            Vertex (vec3 1 1 0)

        bottomLeft =
            Vertex (vec3 -1 -1 0)

        bottomRight =
            Vertex (vec3 1 -1 0)
    in
        WebGL.triangles
            [ ( topLeft, topRight, bottomLeft )
            , ( bottomLeft, topRight, bottomRight )
            ]


loadTextures
    :  List Base64Url
    -> (Int, Int)
    -> Model
    -> (List TextureAndSize -> msg)
    -> (Texture.Error -> msg)
    -> Cmd msg
loadTextures gradientsToLoad (w, h) model success fail =
    let
        gradientSize = vec2 (toFloat w) (toFloat h)
    in gradientsToLoad
        |> List.map2 (\group url -> ( group.balls, url )) model.groups
        |> List.foldl
            (\( balls, Base64Url gradientUrl ) texturesToLoad ->
                let ( Base64Url dataUrl, dataTextureSize ) = makeDataTexture balls
                in texturesToLoad ++
                    [ Texture.load gradientUrl |> Task.map (\t -> (t, gradientSize))
                    , Texture.load dataUrl |> Task.map (\t -> (t, dataTextureSize))
                    ]
            ) []
        |> Task.sequence
        |> Task.attempt
            (\result ->
                case result of
                    Err error -> fail error
                    Ok textures -> success textures
            )

-- Shaders


ballToVec3 : Ball -> Vec3
ballToVec3 { radius, origin } =
    let ( x, y ) = ( Vec2.getX origin, Vec2.getY origin )
    in vec3 x y radius


type alias Uniforms =
   { gradientTexture : Texture
   , dataTexture : Texture
   , resolution : Vec2
   , time : Time
   , ballsQuantity : Int
   , dataTextureSize : Vec2
   , translate : Vec2
   }


animateGroupPosition : List Ball -> Float -> Vec2
animateGroupPosition balls =
    let
        translateX =
            animation 0
                |> from 100
                |> to 300
                |> duration 3000
                |> delay 0
                |> ease inOutBack
        translateY =
            animation 300
                |> from 20
                |> to 100
                |> duration 5000
                |> delay 0
                |> ease inOutBack
    in
        \now -> vec2 (animate now translateX) (animate now translateY)


uniforms : Time -> List Ball -> TextureAndSize -> TextureAndSize -> Viewport {} -> Uniforms
uniforms now balls ( groupTexture, _ ) ( dataTexture, dataTextureSize) v =
    let
        width = Vec2.getX v.size
        height = Vec2.getY v.size
    in
        { gradientTexture = groupTexture
        , dataTexture = dataTexture
        , resolution = vec2 width height
        , time = now
        , ballsQuantity = List.length balls
        , dataTextureSize = dataTextureSize
        , translate = animateGroupPosition balls now
        }


vertexShader : WebGL.Shader Vertex Uniforms {}
vertexShader =
    [glsl|
        attribute vec3 position;

        void main () {
            gl_Position = vec4(position, 1.0);
        }
    |]


fragmentShader : WebGL.Shader {} Uniforms {}
fragmentShader =
    [glsl|
        //-// #ifdef GL_OES_standard_derivatives
        //-// #extension GL_OES_standard_derivatives : enable
        //-// #endif

        precision mediump float;
        uniform sampler2D gradientTexture;
        uniform sampler2D dataTexture;
        uniform vec2 resolution;
        uniform float time;
        uniform int ballsQuantity;
        uniform vec2 dataTextureSize;
        uniform vec2 translate;

        float v = 0.0;
        float scale = .65;
        float positionMultiplier = 1.0;
        float radiusMultiplier = 1.0;

        vec2 originOffset = vec2(.65, .45);
        vec2 screenCenter = vec2(500., 300.); // FIXME: use width & height

        vec2 animate(float time, vec2 curPos, float radius, vec4 animation) {
            float speed = animation.s / 500.; // FIXME: why speed needs to be even slower?
            float t = animation.t;
            vec2 arcMult = animation.pq;

            vec2 origin = screenCenter * originOffset;

            float targX = origin.x + (curPos.x * scale + (sin((t + time) * speed) * radius * arcMult.x) + (sin((t + time) * speed) * radius * arcMult.x)) * positionMultiplier;
            float targY = origin.y + (curPos.y * scale + (sin((t + time) * speed) * radius * arcMult.y) + (sin((t + time) * speed) * radius * arcMult.y)) * positionMultiplier;

            return vec2(targX, targY);

            // return curPos;

            // targX = centerX + (mb.center.x * scale + (Math.sin((mb.t + time) * mb.speed) * radius * mb.arcMult.x) + (Math.sin((mb.t + time) * mb.speed) * radius * mb.arcMult.x)) * animationProperties.positionMultiplier;
            // targY = centerY + (mb.center.y * scale + (Math.cos((mb.t + time) * mb.speed) * radius * mb.arcMult.y) + (Math.cos((mb.t + time) * mb.speed) * radius * mb.arcMult.y)) * animationProperties.positionMultiplier;

            // t = Math.atan2(mb.x - mousePosition.x, mb.y - mousePosition.y);
            // d = 500 / Math.sqrt(Math.pow(mousePosition.x - mb.x, 2) + Math.pow(mousePosition.y - mb.y, 2));



            // mb.x += d * Math.sin(t) + (targX - mb.x) * 0.1;
            // mb.y += d * Math.cos(t) + (targY - mb.y) * 0.1;
        }

        float noise(vec2 seed, float time) {
              float x = (seed.x / 3.14159 + 4.0) * (seed.y / 13.0 + 4.0) * ((fract(time) + 1.0) * 10.0);
              return mod((mod(x, 13.0) + 1.0) * (mod(x, 123.0) + 1.0), 0.01) - 0.005;
        }

        float brightness(vec3 color) {
              return (0.2126 * color.r + 0.7152 * color.g + 0.0722 * color.b);
        }

        float color2float(vec4 color) {
            return color.z * 255.0
            + color.y * 256.0 * 255.0
            + color.x * 256.0 * 256.0 * 255.0;
        }

        vec3 findMetaball(int t) {
            vec2 coordinateForX = (vec2(0., t * 2)  * 2. + 1.) / (dataTextureSize * 2.);
            float xValue = color2float( texture2D(dataTexture, coordinateForX));
            vec2 coordinateForY = (vec2(1., t * 2)  * 2. + 1.) / (dataTextureSize * 2.);
            float yValue = color2float( texture2D(dataTexture, coordinateForY));
            vec2 coordinateForR = (vec2(2., t * 2)  * 2. + 1.) / (dataTextureSize * 2.);
            float rValue = color2float( texture2D(dataTexture, coordinateForR));
            return vec3(xValue, yValue, rValue);
        }

        vec4 findAnimation(int t) {
            vec2 coordinateForSpeed    = (vec2(0., t * 2 + 1)  * 2. + 1.) / (dataTextureSize * 2.);
            float speedValue = color2float( texture2D(dataTexture, coordinateForSpeed));
            vec2 coordinateForT        = (vec2(1., t * 2 + 1)  * 2. + 1.) / (dataTextureSize * 2.);
            float tValue = color2float( texture2D(dataTexture, coordinateForT));
            vec2 coordinateForArcMultX = (vec2(2., t * 2 + 1)  * 2. + 1.) / (dataTextureSize * 2.);
            float arcMultXValue = color2float( texture2D(dataTexture, coordinateForArcMultX));
            vec2 coordinateForArcMultY = (vec2(3., t * 2 + 1)  * 2. + 1.) / (dataTextureSize * 2.);
            float arcMultYValue = color2float( texture2D(dataTexture, coordinateForArcMultY));
            return vec4(speedValue / 500., tValue / 4., arcMultXValue / 500., arcMultYValue / 500.);
        }

        void main () {
            vec2 curPosition = gl_FragCoord.xy - translate.xy;
            vec3 metaball;
            vec4 animation;
            float r;
            vec2 deltaPos, animatedPos;

            for (int i = 0; i < 50; i++) {
                if (i < ballsQuantity){
                    metaball = findMetaball(i);

                    animation = findAnimation(i);

                    r = metaball.z;
                    animatedPos = animate(time, metaball.xy, r, animation);
                    deltaPos = animatedPos - curPosition;
                    v += r*r/dot( deltaPos, deltaPos );
                }
            }


            float delta = 0.0;
            float alpha = 1.0;
            vec4 color;
            vec4 textureColor = texture2D(gradientTexture, gl_FragCoord.xy / resolution);

            //-// #ifndef GL_OES_standard_derivatives
            if (v > 1.0) {
                float l = length(textureColor);
                if (l > 1.05) {
                    color = textureColor * 0.7;
                } else {
                    color = textureColor * 0.5;
                };
            } else { discard; }
            //-// #endif

            //-// #ifdef GL_OES_standard_derivatives
            //-// color = textureColor;
            //-// delta = fwidth(v);
            //-// if (v > delta) {
            //-//   alpha = smoothstep( 1.0 - delta, 1.0 + delta, v );
            //-// }
            //-// #endif

            vec2 st = gl_FragCoord.xy / resolution;
            color.rgb = mix(color.rgb, vec3(noise(st * 1000.0, 1.0) * 100.0), 0.03 / pow(brightness(color.rgb), 0.3));
            gl_FragColor = color * alpha * 0.8;
        }
    |]
