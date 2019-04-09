module Layer.Fluid exposing
    ( Model
    , Mesh
    , Base64Url(..)
    , GradientsToLoad
    , AllTextures
    , BallGroup
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


type alias BallGroup = List Ball


type alias Model =
    { textures: Maybe AllTextures
    , balls: List BallGroup
    }


type alias BallsGenerator = Random.Generator (List BallGroup)


type alias Mesh = WebGL.Mesh Vertex
type alias Time = Float


type alias TextureAndSize = ( Texture, Vec2 )

type alias GroupTextures = 
    { gradient : TextureAndSize
    , data : TextureAndSize
    }

type alias AllTextures =
    List GroupTextures
        

type Base64Url = Base64Url String


type alias GradientsToLoad =
    List Base64Url


init : Model
init =
    { textures = Nothing
    , balls = [ [ { origin = vec2 50 50, radius = 50.0 } ] ]
    }


minGroups = 2
maxGroups = 5
minNumberOfBalls = 5
maxNumberOfBalls = 30
minRadius = 5
maxRadius = 50
-- product = Product.PyCharm


generator : ( Int, Int ) -> BallsGenerator
generator ( w, h ) =
    let
        generatePosition =
            Random.map2 vec2
                (Random.float 0 <| toFloat w)
                (Random.float 0 <| toFloat h)
        generateRadius = Random.float minRadius maxRadius
        generateBalls =
            Random.int minNumberOfBalls maxNumberOfBalls
                |> Random.andThen
                    (\numCircles ->
                        Random.pair generatePosition generateRadius
                            |> Random.list numCircles
                    )
        makeBall (pos, radius) = Ball pos radius
    in
        Random.int minGroups maxGroups
            |> Random.andThen
                (\numGroups ->
                    Random.list numGroups generateBalls
                        |> (Random.map <| List.map <| List.map makeBall)
                )


generate : (List BallGroup -> msg) -> Random.Generator (List BallGroup) -> Cmd msg
generate = Random.generate


makeDataTexture : BallGroup -> ( Base64Url, Vec2 )
makeDataTexture group = 
    let addBallData { origin, radius } prevData = 
            prevData ++ [ floor <| Vec2.getX origin, floor <| Vec2.getY origin, floor radius, 0 ]
        data = group |> List.foldl addBallData [] 
        dataLen =  List.length data
        --width = Debug.log "dataLen" 4
        width = 4
        --height = Debug.log "height" <| floor <| toFloat dataLen / 4 
        height = maxNumberOfBalls + modBy 4 maxNumberOfBalls 
    --in Base64Url <| encode24 width (Debug.log "heightBy4" <| height + modBy 4 height) (Debug.log "data" data)
    in 
        ( Base64Url <| encode24With  width height (Debug.log "data" data)  {defaultOptions | order = RightUp} 
        , vec2 (toFloat width) (toFloat height)
        ) 
  --  in Base64Url <| encode24  1 20  [1, 2, 3, 4]


packTextures : List ( Texture, Vec2 ) -> AllTextures
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


injectTextures : AllTextures -> Model -> Model
injectTextures textures model =
    { model | textures = Just textures }


makeEntity
    :  Time
    -> Viewport {}
    -> List Setting
    -> Mesh
    -> BallGroup
    -> { gradient : ( Texture, Vec2 ) , data : ( Texture, Vec2 ) }
    -> WebGL.Entity
makeEntity now viewport settings mesh group textures  =
    WebGL.entityWith
        settings
        vertexShader
        fragmentShader
        mesh
        (uniforms now group textures.gradient textures.data viewport)


-- TODO: add mouse
makeEntities : Time -> Viewport {} -> Model -> List Setting -> Mesh -> List WebGL.Entity
makeEntities now viewport model settings mesh =
    case model.textures of
        Just textures ->
            List.map2
                (makeEntity now viewport settings mesh)
                model.balls
                textures
        Nothing -> [ ]


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
    :  GradientsToLoad
    -> (Int, Int)
    -> Model
    -> (List TextureAndSize -> msg)
    -> (Texture.Error -> msg)
    -> Cmd msg
loadTextures gradientsToLoad (w, h) model success fail =
    let
        gradientSize = vec2 (toFloat w) (toFloat h)
    in gradientsToLoad
        |> List.map2 Tuple.pair model.balls
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


uniforms : Time -> BallGroup -> TextureAndSize -> TextureAndSize -> Viewport {} -> Uniforms
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


        vec2 vertexOscillators(float t) {
            return vec2(sin(3.0 * t) * cos(2.0 * t), sin(t + 200.0) * sin(5.0 * t));
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
              if (l > 1.05) {
                color = textureColor * 0.7;
              } else { 
                color = textureColor * 0.5;
              }
            } else { discard; }
            gl_FragColor = vec4(textureColor.rgb, 0.8);
        }            
    |]
