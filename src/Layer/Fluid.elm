module Layer.Fluid exposing
    ( Model
    , Mesh
    , BallGroup
    , Base64Url(..)
    , TextureAndSize
    , Gradient, GradientStops, GradientOrientation(..)
    , applyProductToGradients
    , remapTo
    , makeEntities
    , build
    , init
    , loadTextures, injectTextures, packTextures
    , generator, gradientGenerator
    , generate, generateGradient, generateGradientsFor
    , range
    , Variety(..), Orbit(..)
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
    , phase : Float
    , amplitude : Vec2
    }


type alias BallGroup =
    { balls : List Ball
    , textures :
        Maybe
            { gradient : TextureAndSize
            , data : TextureAndSize
            }
    , gradient : Maybe Gradient
    , origin : Vec2
    }


type alias Gradient =
    { stops: GradientStops
    , orientation: GradientOrientation
    }


type alias Model =
    { groups : List BallGroup
    , forSize : Maybe ( Int, Int ) -- FIXME: if we always use the main window size for generating model, then it's the duplication
    , variety : Variety
    , orbit : Orbit
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


type alias Ranges =
    { groups : IntRange
    , balls : IntRange
    , radius : FloatRange
    , speed : FloatRange
    , phase : FloatRange
    , amplitude :
        { x : FloatRange
        , y : FloatRange
        }
    }


type Variety = Variety Float -- 0.01..1 -- a.k.a Variance
type Orbit = Orbit Float -- 0..1
type Focus = Focus Float -- 0..1
type Gaussian = Gaussian Float -- 0..1


-- type FocusChange
--     = GroupsCount Int
--     | BallsCount Int
--     | Speed Float
--     | Phase Float
--     | AmplitudeX Float
--     | AmplitudeY Float


init : Model
init =
    { groups = [ ]
    , forSize = Nothing
    , variety = Variety 0.5
    , orbit = Orbit 0.5
    }


range : Ranges
range =
    { groups = iRange 1 5
    , balls = iRange 4 10
    , radius = fRange 10 100
    , speed = fRange 100 200
    , phase = fRange 0 360 -- Maybe useless
    , amplitude =
        { x = fRange -50 50
        , y = fRange -10 10
        }
    }


gaussian : Float -> Focus -> Variety -> Gaussian
gaussian x (Focus focus) (Variety variety) =
    let
        numerator = (x - focus) ^ 2
        denominator = 2 * (variety ^ 2)
    in
        Gaussian <| e ^ (-1 * numerator / denominator)


generator : ( Int, Int ) -> Variety -> Orbit -> Product.Palette -> Random.Generator Model
generator ( w, h ) variety orbit palette =
    let
        generateGaussianX =
            Random.float 0 1
        applyGaussX gaussX value =
            case gaussian gaussX (Focus value) variety of
                (Gaussian gaussY) -> gaussY
        generatePosition =
            Random.map2 vec2
                (Random.float 0 <| toFloat w * 0)
                (Random.float 0 <| toFloat h * 0)
        generateRadius = randomFloatInRange range.radius
        generateSpeed = randomFloatInRange range.speed
        generatePhase = randomFloatInRange range.phase
        generateAmplitude =
            Random.map2 vec2
                (randomFloatInRange range.amplitude.x)
                (randomFloatInRange range.amplitude.y)
        generateGroup gaussX =
            randomIntInRange range.balls
                |> Random.andThen
                    (\numCircles ->
                        Random.map5
                            Ball
                            generatePosition
                            generateRadius
                            generateSpeed
                            generatePhase
                            generateAmplitude
                            |> Random.list numCircles
                    )
                |> Random.andThen
                    (\balls ->
                        gradientGenerator palette
                            |> Random.map
                                (\gradient ->
                                    { balls = balls
                                    , gradient = Just gradient
                                    }
                                )
                    )
                |> Random.andThen
                    (\{ balls, gradient } ->
                        Random.map2 vec2
                            (Random.float 0 1)
                            (Random.float 0 1)
                        |> Random.map
                            (\origin ->
                                { balls = balls
                                , gradient = gradient
                                , textures = Nothing
                                , origin = origin
                                }
                            )
                    )
        makeBall { center, radius } = Ball center radius
    in
        generateGaussianX
            |> Random.andThen
                (\gaussX ->
                    randomIntInRange range.groups
                        |> Random.map (Tuple.pair gaussX)
                )
            |> Random.andThen
                (\( gaussX, numGroups ) ->
                    Random.list numGroups (generateGroup gaussX)
                )
            |> Random.map (\groups ->
                { groups = groups
                , forSize = Just ( w, h )
                , variety = variety
                , orbit = orbit
                })


generate : (Model -> msg) -> Random.Generator Model -> Cmd msg
generate = Random.generate


gradientGenerator
    :  Product.Palette
    -> Random.Generator Gradient
gradientGenerator palette =
    let
        _ = Debug.log "palette" palette
        paletteLen = List.length palette
        -- loopedPalette = [ 0, 1, 2, 3, 2, 1 ] -- just remember indices?
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
                                |> Maybe.withDefault ""
                            )
                    )
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
        generateOrientation =
            Random.float 0 1
                |> Random.map
                    (\v -> if v >= 0.5 then Horizontal else Vertical)
    in
        Random.map2
            (\stopsWithColors orientation ->
                { stops = stopsWithColors
                , orientation = orientation
                }
            )
            generateStopsWithColors
            generateOrientation


generateGradient : (Gradient -> msg) -> Random.Generator Gradient -> Cmd msg
generateGradient = Random.generate


generateGradientsFor : (Model -> msg) -> Product.Palette -> Model -> Cmd msg
generateGradientsFor putIntoMsg palette model =
    let
        groupsCount = List.length model.groups
    in
        Random.generate
            (\gradients ->
                putIntoMsg
                    { model
                    | groups =
                        List.map2
                            (\group gradient ->
                                { group
                                | gradient = Just gradient
                                }
                            )
                            model.groups
                            gradients
                    }
            )
            (Random.list groupsCount <| gradientGenerator palette)


remapTo : ( Int, Int ) -> Model -> Model
remapTo ( newWidth, newHeight ) model =
    let
        ( factorW, factorH ) =
            model.forSize
                |> Maybe.map
                    (\curSize ->
                        case curSize of
                            ( width, height ) ->
                                ( toFloat newWidth / toFloat width
                                , toFloat newHeight / toFloat height
                                )
                    )
                |> Maybe.withDefault ( 1.0, 1.0 )
        _ = Debug.log "factor" ( factorW, factorH )
        remapGroup group =
            { group
            | balls = List.map remapBall group.balls
            }
        remapBall ball =
            { ball
            | origin =
                Vec2.vec2 (Vec2.getX ball.origin * factorW) (Vec2.getY ball.origin * factorH)
            }
    in
        { model
        | groups = List.map remapGroup model.groups
        , forSize = Just ( newWidth, newHeight )
        }


makeDataTexture : List Ball -> ( Base64Url, Vec2 )
makeDataTexture balls =
    let addBallData ball prevData =
            prevData ++
                [ floor <| Vec2.getX ball.origin -- 0.
                , floor <| Vec2.getY ball.origin -- 1.
                , floor  ball.radius             -- 2.
                , 0                              -- 3.
                , floor <| ball.speed -- 4.
                , floor <| ball.phase -- 5.
                , floor <| Vec2.getX ball.amplitude -- 6.
                , floor <| Vec2.getY ball.amplitude -- 7.
                ]
        data = balls |> List.foldl addBallData []
        dataLen =  List.length data
        width = 4
        -- FIXME: could be not enough height for all the data
        -- maxNumberOfBalls = getIntMax numberOfBalls
        height = 64
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


applyProductToGradients : Product.Product -> Model -> Model
applyProductToGradients product model =
    let
        newProductPalette = Product.getPalette product |> Array.fromList
        updateStop index stop =
            let
                paletteIndex = modBy (Array.length newProductPalette) index
            in
                Array.get paletteIndex newProductPalette
                    |> Maybe.map (\color -> Tuple.mapSecond (always color) stop)
                    |> Maybe.withDefault stop
        updateGroupGradient group =
            { group
            | gradient =
                Maybe.map
                    (\gradient ->
                        { gradient
                        | stops = List.indexedMap updateStop gradient.stops
                        }
                    )
                    group.gradient
            }
    in
        { model | groups = List.map updateGroupGradient model.groups }


makeEntity
    :  Time
    -> ( Int, Int )
    -> Viewport {}
    -> List Setting
    -> Mesh
    -> Vec2
    -> List Ball
    -> { gradient : ( Texture, Vec2 ) , data : ( Texture, Vec2 ) }
    -> WebGL.Entity
makeEntity now mousePos viewport settings mesh groupOrigin balls textures  =
    WebGL.entityWith
        settings
        vertexShader
        fragmentShader
        mesh
        (uniforms now mousePos groupOrigin balls textures.gradient textures.data viewport)


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
                        makeEntity now mousePos viewport settings mesh group.origin group.balls textures
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
   , groupOrigin : Vec2
   }


uniforms
    :  Time
    -> ( Int, Int )
    -> Vec2
    -> List Ball
    -> TextureAndSize
    -> TextureAndSize
    -> Viewport {}
    -> Uniforms
uniforms now ( mouseX, mouseY ) groupOrigin balls ( groupTexture, _ ) ( dataTexture, dataTextureSize) v =
    let
        width = Vec2.getX v.size
        height = Vec2.getY v.size
    in
        { gradientTexture = groupTexture
        , dataTexture = dataTexture
        , resolution = vec2 width height
        , time = now
        --, ballsQuantity = Debug.log "ballsCount" <| List.length balls
        , ballsQuantity = List.length balls
        , dataTextureSize = dataTextureSize
        , mousePosition = vec2 (toFloat mouseX) (toFloat mouseY)
        , groupOrigin = groupOrigin
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

        //vec2 originOffset = vec2(.65, .45);
        // vec2 originOffset = vec2(.0, .0);
        float tm, dm;
        float speed, phase, targX, targY;
        vec2 amplitude, origin, newPos, toReturn;

        float atan2(float y, float x) {
            bool s = (abs(x) > abs(y));
            return mix(3.14/2.0 - atan(x,y), atan(y,x), s ? 1.0 : 0.0);
        }

        vec2 animate(float time, vec2 curPos, float radius, vec4 animation) {
            speed = animation.s;
            phase = animation.t;
            amplitude = animation.pq;

            origin = (resolution / 2.);

            //newPos = amplitude * sin(time * speed / 500000.0 + phase);
            //newPos = amplitude * sin(time) * speed / 500000.0 + phase;
            newPos = amplitude * 10000. * sin(time * speed / 500000.0 + phase);

          //  toReturn = vec2(curPos.x, curPos.y);

           // tm = atan2(curPos.x - mousePosition.x, curPos.y - mousePosition.y);
           // dm = 500. / sqrt(pow(mousePosition.x - curPos.x, 2.0) + pow(mousePosition.y - curPos.y, 2.0));

           // toReturn.x += dm * sin(tm) + (targX - curPos.x) * 0.1;
          //  toReturn.y += dm * cos(tm) + (targY - curPos.y) * 0.1;

            toReturn = curPos + newPos + origin;

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
            return vec4(speedValue, tValue / 4., amplitudeXValue / 500., amplitudeYValue / 500.);
        }

        void main () {
            vec2 curFragCoord = gl_FragCoord.xy; // - translate.xy;
            vec3 metaball;
            vec4 animation;
            float r;
            vec2 deltaPos, animatedPos;

            for (int i = 0; i < 50; i++) {
                if (i < ballsQuantity) {
                    metaball = findMetaball(i);

                    animation = findAnimation(i);

                    r = metaball.z;
                    //metaball.x = animation.y * 5.;
                    //deltaPos = metaball.xy - curFragCoord;
                    animatedPos = animate(time, metaball.xy, r, animation);
                    deltaPos = animatedPos - curFragCoord;
                    v += r*r/dot( deltaPos, deltaPos );
                }
            }


            float delta = 0.0;
            float alpha = 1.0;
            vec4 color;
            vec4 textureColor = texture2D(gradientTexture, curFragCoord / resolution);

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
