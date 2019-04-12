module Layer.Fluid exposing
    ( Model
    , Mesh
    , BallGroup
    , Base64Url(..)
    , TextureAndSize
    , makeEntities
    , build
    , init
    , loadTextures
    , injectTextures
    , packTextures
    , generate
    , generator
    )

import Array exposing (Array)
import Random
import Task

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


type alias Ball =
    { origin : Vec2
    , radius : Float
    }


type alias BallGroup =
    { balls: List Ball
    , textures:
        Maybe
            { gradient : TextureAndSize
            , data : TextureAndSize
            }
    , gradient: Maybe Gradient
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


type alias Gradient = List ColorStop


init : Model
init =
    { groups = [ ]
    }


minGroups = 2
maxGroups = 5
minNumberOfBalls = 5
maxNumberOfBalls = 30
minRadius = 5
maxRadius = 50
-- product = Product.PyCharm


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
        generateRadius = Random.float minRadius maxRadius
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
            Random.int minNumberOfBalls maxNumberOfBalls
                |> Random.andThen
                    (\numCircles ->
                        Random.pair generatePosition generateRadius
                            |> Random.list numCircles
                    )
                |> Random.andThen
                    (\circles ->
                        generateStopsWithColors
                            |> Random.map
                                (\stopsWithColors ->
                                    ( circles, stopsWithColors )
                                )
                    )
                |> Random.map
                    (\(circles, gradient) ->
                        { balls = circles |> List.map makeBall
                        , textures = Nothing
                        , gradient = Just gradient
                        }
                    )
        makeBall (pos, radius) = Ball pos radius
    in
        Random.int minGroups maxGroups
            |> Random.andThen
                (\numGroups ->
                    Random.list numGroups generateGroup
                )
            |> Random.map (\groups -> { groups = groups })


generate : (Model -> msg) -> Random.Generator Model -> Cmd msg
generate = Random.generate


makeDataTexture : List Ball -> ( Base64Url, Vec2 )
makeDataTexture balls =
    let addBallData { origin, radius } prevData =
            prevData ++ [ floor <| Vec2.getX origin, floor <| Vec2.getY origin, floor radius, 0 ]
        data = balls |> List.foldl addBallData []
        dataLen =  List.length data
        width = 4
        height = maxNumberOfBalls + modBy 4 maxNumberOfBalls
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
   }


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

        float color2float(vec4 color) {
            return color.z * 255.0
            + color.y * 256.0 * 255.0
            + color.x * 256.0 * 256.0 * 255.0
            ;
        }

        vec3 findMetaball(int t) {
            vec2 coordinateForX = (vec2(0., t)  * 2. + 1.) / (dataTextureSize * 2.);
            float xValue = color2float( texture2D(dataTexture, coordinateForX));
            vec2 coordinateForY = (vec2(1., t)  * 2. + 1.) / (dataTextureSize * 2.);
            float yValue = color2float( texture2D(dataTexture, coordinateForY));
            vec2 coordinateForR = (vec2(2., t)  * 2. + 1.) / (dataTextureSize * 2.);
            float rValue = color2float( texture2D(dataTexture, coordinateForR));
            return vec3(xValue, yValue, rValue);
        }

        void main () {
            float v = 0.0;
            float speed = 1.5;

            float alpha = 1.0;

            //-// float r, delta;
            //-// vec2 cxy = 2.0 * gl_PointCoord - 1.0;
            //-// r = dot(cxy, cxy);
            //-// #ifdef GL_OES_standard_derivatives
            //-//    delta = fwidth(r);
            //-//    alpha = 1.0 - smoothstep(1.0 - delta, 1.0 + delta, r);
            //-// #endif

            for (int i = 0; i < 50; i++) {
                if (i < ballsQuantity){
                    vec3 metaball = findMetaball(i);
                    float dx =  metaball.x - gl_FragCoord.x;
                    float dy =  metaball.y - gl_FragCoord.y;
                    float r = metaball.z;
                    v += r*r/(dx*dx + dy*dy);
                }
            }

            vec4 color;
            vec4 textureColor = texture2D(gradientTexture, gl_FragCoord.xy / resolution);

            if (v > 1.0) {
              float l = length(textureColor);

                if (l > 1.05) { color = textureColor * 0.7;
                    } else { color = textureColor * 0.5;}

            } else { discard; }
            gl_FragColor = textureColor * alpha;

            //-// #ifdef GL_OES_standard_derivatives
            //-//    gl_FragColor.r = 1.0;
            //-// #endif


            gl_FragColor = vec4(textureColor.rgb, 0.4);
        }
    |]
