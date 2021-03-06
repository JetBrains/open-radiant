module Layer.Fluid.Render exposing (..)

import Math.Vector3 as Vec3 exposing (..)
import Math.Vector2 as Vec2 exposing (..)

import WebGL
import WebGL.Settings exposing (Setting)
import WebGL.Texture as Texture
import WebGL.Texture exposing (Texture)

import Viewport exposing (Viewport)

import Layer.Fluid.Model exposing (..)


-- Mesh

type alias Mesh = WebGL.Mesh Vertex


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
   , orbit : Float
   }


uniforms
    :  Time
    -> ( Int, Int )
    -> Vec2
    -> List Ball
    -> TextureAndSize
    -> TextureAndSize
    -> Orbit
    -> Viewport {}
    -> Uniforms
uniforms
    now
    ( mouseX, mouseY )
    groupOrigin
    balls
    ( groupTexture, _ )
    ( dataTexture, dataTextureSize )
    (Orbit orbit)
    v =
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
        , orbit = orbit
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
