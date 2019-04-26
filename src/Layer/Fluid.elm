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
    , amplitudeX, amplitudeY
    )

import Array exposing (Array)
import Random exposing (..)
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
    , amplitude : Vec2
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


numberOfGroups = iRange 3 5
numberOfBalls  = iRange 5 30
radiusRange    = fRange 10 100
speedRange     = fRange 0.2 2.0
amplitudeX       = fRange -1.0 1.0
amplitudeY       = fRange -0.25 1.0


speedTextureMultiplier = 500
amplitudeTextureMultiplier = 500
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
        generateAmplitude =
            Random.map2 vec2
                (randomFloatInRange amplitudeX)
                (randomFloatInRange amplitudeY)
        generateStopsWithColors =
                   let
                    count = 4
                    step = 1 / count
                    generateNext prevValues index min max =
                        if index < count then
                            Random.float min max
                                |> Random.andThen
                                    (\val ->
                                        generateNext
                                            (prevValues ++ [ val ])
                                            (index + 1)
                                            (min + step)
                                            (max + step)
                                    )

                        else
                            Random.constant prevValues
                    in
                        generateNext [] 0 0 step

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
                            generateAmplitude
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
                , floor <| amplitudeTextureMultiplier * Vec2.getX ball.amplitude -- 6.
                , floor <| amplitudeTextureMultiplier * Vec2.getY ball.amplitude -- 7.
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
    -> ( Int, Int )
    -> Viewport {}
    -> List Setting
    -> Mesh
    -> List Ball
    -> { gradient : ( Texture, Vec2 ) , data : ( Texture, Vec2 ) }
    -> WebGL.Entity
makeEntity now mousePos viewport settings mesh balls textures  =
    WebGL.entityWith
        settings
        vertexShader
        fragmentShader
        mesh
        (uniforms now mousePos balls textures.gradient textures.data viewport)


-- TODO: add mouse
makeEntities
     : Time
    -> ( Int, Int )
    -> Viewport {}
    -> Model
    -> List Setting
    -> Mesh
    -> List WebGL.Entity
makeEntities now mousePos viewport model settings mesh =
    let
        makeGroupEntity group =
            group.textures
                |> Maybe.map
                    (\textures ->
                        makeEntity now mousePos viewport settings mesh group.balls textures
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
   , mousePosition : Vec2
   }


uniforms
    :  Time
    -> ( Int, Int )
    -> List Ball
    -> TextureAndSize
    -> TextureAndSize
    -> Viewport {}
    -> Uniforms
uniforms now ( mouseX, mouseY ) balls ( groupTexture, _ ) ( dataTexture, dataTextureSize) v =
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
        , mousePosition = vec2 (toFloat mouseX) (toFloat mouseY)
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
        uniform vec2 mousePosition;

        float v = 0.0;
        float scale = .65;
        float positionMultiplier = 1.0;
        float radiusMultiplier = 1.0;
        float speedMultiplier = 0.0002;

        vec2 originOffset = vec2(.65, .45);

        float tm, dm;
        float speed, t, targX, targY;
        vec2 amplitude, origin, toReturn;

        float atan2(float y, float x) {
            bool s = (abs(x) > abs(y));
            return mix(3.14/2.0 - atan(x,y), atan(y,x), s ? 1.0 : 0.0);
        }

        vec2 animate(float time, vec2 curPos, float radius, vec4 animation) {
            speed = animation.s * speedMultiplier; // FIXME: why speed needs to be even slower?
            t = animation.t;
            amplitude = animation.pq;

            origin = (resolution / 2.) - (resolution / 2. * originOffset);

            targX = origin.x + (curPos.x * scale + (sin((t + time) * speed) * radius * amplitude.x) + (sin((t + time) * speed) * radius * amplitude.x)) * positionMultiplier;
            targY = origin.y + (curPos.y * scale + (sin((t + time) * speed) * radius * amplitude.y) + (sin((t + time) * speed) * radius * amplitude.y)) * positionMultiplier;

            toReturn = vec2(curPos.x, curPos.y);

            tm = atan2(curPos.x - mousePosition.x, curPos.y - mousePosition.y);
            dm = 500. / sqrt(pow(mousePosition.x - curPos.x, 2.0) + pow(mousePosition.y - curPos.y, 2.0));

            toReturn.x += dm * sin(tm) + (targX - curPos.x) * 0.1;
            toReturn.y += dm * cos(tm) + (targY - curPos.y) * 0.1;

            toReturn = vec2(targX, targY);

            return toReturn;
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
            vec2 coordinateForAmplitudeX = (vec2(2., t * 2 + 1)  * 2. + 1.) / (dataTextureSize * 2.);
            float amplitudeXValue = color2float( texture2D(dataTexture, coordinateForAmplitudeX));
            vec2 coordinateForAmplitudeY = (vec2(3., t * 2 + 1)  * 2. + 1.) / (dataTextureSize * 2.);
            float amplitudeYValue = color2float( texture2D(dataTexture, coordinateForAmplitudeY));
            return vec4(speedValue / 500., tValue / 4., amplitudeXValue / 500., amplitudeYValue / 500.);
        }

        void main () {
            vec2 curPosition = gl_FragCoord.xy; // - translate.xy;
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
