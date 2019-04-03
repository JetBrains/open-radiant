module Layer.Fluid exposing
    ( Model
    , Mesh
    , Base64Url(..)
    , GradientsToLoad
    , Textures
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

import Algorithm.Base64.BMP exposing (encode24)
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
    { textures: Maybe Textures
    , balls: List BallGroup
    }


type alias BallsGenerator = Random.Generator (List BallGroup)


type alias Mesh = WebGL.Mesh Vertex
type alias Time = Float


type alias Textures =
    List
        { gradient : Texture
        , data : Texture
        }

type Base64Url = Base64Url String


type alias GradientsToLoad =
    List Base64Url


init : Model
init =
    { textures = Nothing
    , balls = [ [ { origin = vec2 50 50, radius = 50.0 } ] ]
    }


minGroups = 2
maxGroups = 3
minNumberOfBalls = 5
maxNumberOfBalls = 30
minRadius = 20
maxRadius = 200
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


generate : (Model -> msg) -> Random.Generator Model -> Cmd msg
generate = Random.generate


makeDataTexture : BallGroup -> Base64Url
makeDataTexture _ = Base64Url <| encode24 2 2 [1,2,3,4]


packTextures : List Texture -> Textures
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


injectTextures : Textures -> Model -> Model
injectTextures textures model =
    { model | textures = Just textures }


makeEntity
    :  Time
    -> Viewport {}
    -> List Setting
    -> Mesh
    -> BallGroup
    -> { gradient : Texture , data : Texture }
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
    -> Model
    -> (List Texture -> msg)
    -> (Texture.Error -> msg)
    -> Cmd msg
loadTextures gradientsToLoad model success fail =
    gradientsToLoad
        |> List.map2 Tuple.pair model.balls
        |> List.foldl 
            (\( balls, Base64Url gradientUrl ) texturesToLoad -> 
                let (Base64Url dataUrl) = makeDataTexture balls 
                in texturesToLoad ++ [ Texture.load gradientUrl, Texture.load dataUrl ]
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
   , metaballs :  Array Vec3
   }


uniforms : Time -> BallGroup -> Texture -> Texture -> Viewport {} -> Uniforms
uniforms now balls groupTexture dataTexture v =
    let
        width = Vec2.getX v.size
        height = Vec2.getY v.size
    in
        { gradientTexture = groupTexture
        , dataTexture = dataTexture
        , resolution = vec2 width height
        , time = now
        , metaballs = balls
            |> List.map ballToVec3
            |> Array.fromList
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

        vec2 vertexOscillators(float t) {
            return vec2(sin(3.0 * t) * cos(2.0 * t), sin(t + 200.0) * sin(5.0 * t));
        }

        void main () {
            float v = 0.0;
            float radius = 2.0;
            float speed = 1.5;
            float x = gl_FragCoord.x;
            float y = gl_FragCoord.y;
            vec2 metaball = vec2(800.0, 500.0);
            metaball += 350.0 * vertexOscillators( time / 800.0 );
            float dx =  metaball.x - x;
            float dy =  metaball.y - y;
            float r = 140.6;
            v = r*r/(dx*dx + dy*dy);
            vec4 color;
            vec4 textureColor = texture2D(gradientTexture, gl_FragCoord.xy / resolution);
            if (v > 1.0) {
              float l = length(textureColor);
              if (l < 1.05) {
                color = textureColor * 0.7;
              } else { 
                color = textureColor * 0.5;
              }
            } else { discard; }
            gl_FragColor = vec4(textureColor.rgb, 0.8);
            //gl_FragColor = texture2D(gradientTexture, gl_FragCoord.xy / resolution);
        }            

    |]
