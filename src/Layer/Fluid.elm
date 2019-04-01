module Layer.Fluid exposing
    ( Model
    , Mesh
    , makeEntity
    , build
    , init
    , loadTextures
    )

import Array exposing (Array)
import Task

import Math.Vector3 as Vec3 exposing (vec3, Vec3)
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


makeEntity : Viewport {} -> List Setting -> Mesh -> WebGL.Entity
makeEntity viewport settings mesh =
    WebGL.entityWith
        settings
        vertexShader
        fragmentShader
        mesh
        (uniforms viewport)



-- Mesh


type alias Vertex =
    { position : Vec3
    , color : Vec3
    }


build : Model -> Mesh
build model =
    WebGL.triangles
        [ ( Vertex (vec3 -1 1 0) (vec3 1 0 0)
          , Vertex (vec3 1 1 0) (vec3 0 1 0)
          , Vertex (vec3 1 -1 0) (vec3 0 0 1)
          ),
          ( Vertex (vec3 -1 1 0) (vec3 1 0 0)
          , Vertex (vec3 -1 -1 0) (vec3 0 1 0)
          , Vertex (vec3 1 -1 0) (vec3 0 0 1)
          )
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
    Viewport {}


uniforms : Viewport {} -> Uniforms
uniforms v =
    -- { perspective = Mat4.mul v.perspective v.camera }
    v


vertexShader : WebGL.Shader Vertex Uniforms { vcolor : Vec3 }
vertexShader =
    [glsl|

        attribute vec3 position;
        attribute vec3 color;

        uniform mat4 cameraTranslate;
        uniform mat4 cameraRotate;
        uniform mat4 perspective;
        uniform mat4 camera;
        uniform mat4 rotation;

        varying vec3 vcolor;

        void main () {
            // gl_Position = perspective * camera * rotation * cameraTranslate * cameraRotate * vec4(position, 1.0);
            // gl_Position = perspective * camera * rotation * vec4(position, 1.0);
//            gl_Position = perspective * camera * rotation * vec4(position, 1.0);
            gl_Position = vec4(position, 1.0);
            vcolor = color;
        }

    |]


fragmentShader : WebGL.Shader {} Uniforms { vcolor : Vec3 }
fragmentShader =
    [glsl|

        precision mediump float;
        varying vec3 vcolor;

        void main () {
            gl_FragColor = vec4(vcolor, 1.0);
        }

    |]
