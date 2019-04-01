module Layer.Fluid exposing
    ( Model
    , Mesh
    , makeEntities
    , build
    , init
    , loadTextures
    )

import Array exposing (Array)
import Task

import Math.Vector3 as Vec3 exposing (vec3, Vec3)
import Math.Vector2 as Vec2 exposing (vec2, Vec2)
import WebGL
import WebGL.Settings exposing (Setting)
import WebGL.Texture as Texture
import WebGL.Texture exposing (Texture)

import Viewport exposing (Viewport)


type alias Model = 
    { textures: List Texture 
    }


type alias Mesh = WebGL.Mesh Vertex


init : Model
init = 
    { textures = [] 
    }


makeEntities : Viewport {} -> Model -> List Setting -> Mesh -> List WebGL.Entity
makeEntities viewport model settings mesh =
    case List.head model.textures of
        Just firstTexture ->      
            [ WebGL.entityWith
                settings
                vertexShader
                fragmentShader
                mesh
                (uniforms firstTexture) ]
        Nothing -> [ ]



-- Mesh


type alias Vertex =
    { a_position : Vec3
    , a_color : Vec3
    , a_coord : Vec2 
    }


build : Model -> Mesh
build model =
    let
        topLeft =
            Vertex (vec3 -1 1 0) (vec3 1 0 0) (vec2 0 1)

        topRight =
            Vertex (vec3 1 1 0) (vec3 0 1 0) (vec2 1 1)

        bottomLeft =
            Vertex (vec3 -1 -1 0) (vec3 0 0 1) (vec2 0 0)

        bottomRight =
            Vertex (vec3 1 -1 0) (vec3 0 0 1) (vec2 1 0)
    in
        WebGL.triangles 
            [ ( topLeft, topRight, bottomLeft )
            , ( bottomLeft, topRight, bottomRight )
            ]


loadTextures : List String -> (List Texture -> msg) -> (Texture.Error -> msg) -> Cmd msg
loadTextures gradientUrls success fail = 
    gradientUrls
        |> List.map Texture.load
        |> Task.sequence
        |> Task.attempt
            (\result ->
                case result of
                    Err error -> fail error
                    Ok textures -> success textures
            )

-- Shaders


type alias Uniforms =
   { texture : Texture }


uniforms : Texture -> Uniforms
uniforms texture =
    -- { perspective = Mat4.mul v.perspective v.camera }
    { texture = texture }


vertexShader : WebGL.Shader Vertex Uniforms { v_color : Vec3, v_coord: Vec2 }
vertexShader =
    [glsl|

        attribute vec3 a_position;
        attribute vec3 a_color;
        attribute vec2 a_coord;

        // uniform mat4 cameraTranslate;
        // uniform mat4 cameraRotate;
        // uniform mat4 perspective;
        // uniform mat4 camera;
        // uniform mat4 rotation;

        varying vec3 v_color;
        varying vec2 v_coord;

        void main () {
            // gl_Position = perspective * camera * rotation * cameraTranslate * cameraRotate * vec4(position, 1.0);
            // gl_Position = perspective * camera * rotation * vec4(position, 1.0);
//            gl_Position = perspective * camera * rotation * vec4(position, 1.0);
            gl_Position = vec4(a_position, 1.0);
            v_color = a_color;
            v_coord = a_coord;
        }

    |]


fragmentShader : WebGL.Shader {} Uniforms { v_color : Vec3, v_coord: Vec2 }
fragmentShader =
    [glsl|

        precision mediump float;
        uniform sampler2D texture;
        varying vec3 v_color;
        varying vec2 v_coord;

        void main () {
            // gl_FragColor = vec4(vcolor, 1.0);
            gl_FragColor = texture2D(texture, v_coord);

        }

    |]
