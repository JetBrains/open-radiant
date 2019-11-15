port module Layer.Fluid.Fluid exposing
    ( id
    , def
    , loadTextures, injectTextures, packTextures
    )


import Array exposing (Array)
import Task

import Ease exposing (..)

import Math.Vector3 as Vec3 exposing (..)
import Math.Vector2 as Vec2 exposing (..)

import Algorithm.Base64.BMP exposing (encode24With)
import Algorithm.Base64.Image exposing (..)
import Algorithm.Gaussian as Gauss
import Algorithm.Gaussian exposing (gaussian)

import Json.Encode as E exposing (Value)

import WebGL
import WebGL.Settings exposing (Setting)
import WebGL.Texture as Texture
import WebGL.Texture exposing (Texture)

import Viewport exposing (Viewport)
import Gradient exposing (..)

import Model.Product as Product
import Model.Product exposing (ColorId(..))
import Model.Range exposing (..)
import Model.Layer.Def as Layer exposing (..)
import Model.Layer.Blend.WebGL as WebGL exposing (..)

import Layer.Fluid.Model as Model exposing (..)
import Layer.Fluid.Model exposing (Model, Msg(..))
-- import Layer.Fluid.Random as Random exposing (..)
import Layer.Fluid.Render as Random exposing (..)
import Layer.Fluid.Export as IE exposing (..)


-- type alias GradientsToLoad =
--     List Base64Url

id : DefId
id = "fluid"


def : Layer.Def Model () Msg WebGL.Blend
def =
    { id = id
    , kind = WebGL
    , init = \_ ctx ->
        let
            model = Model.init
        in
            ( model
            , Cmd.none
            {-
            , Random.generate
                Rebuild
                (Model.RandomizeEverything
                    Model.defaultRange
                    ctx.palette
                    model.variety
                    model.orbit
                        |> Random.generator ctx.size
                )
            -}
            )
    , encode = IE.encode
    , decode = IE.decode
    , subscribe = Layer.noSubscriptions
    , update = Layer.passUpdate
    , absorb = Layer.bypass
    , view = Layer.singleView ()
    , gui = Nothing
    }


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
        -- _ = Debug.log "factor" ( factorW, factorH )

        remapGroup group =
            { group
            | balls = List.map remapBall group.balls
            }

        remapBall ball =
            { ball
            | origin =
                Vec2.vec2
                    (Vec2.getX ball.origin * factorW)
                    (Vec2.getY ball.origin * factorH)
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


makeEntity
    :  Time
    -> ( Int, Int )
    -> Viewport {}
    -> List Setting
    -> Mesh
    -> Vec2
    -> List Ball
    -> { gradient : ( Texture, Vec2 ) , data : ( Texture, Vec2 ) }
    -> Orbit
    -> WebGL.Entity
makeEntity now mousePos viewport settings mesh groupOrigin balls textures orbit =
    WebGL.entityWith
        settings
        vertexShader
        fragmentShader
        mesh
        (uniforms
            now
            mousePos
            groupOrigin
            balls
            textures.gradient
            textures.data
            orbit
            viewport
        )


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
                        makeEntity
                            now
                            mousePos
                            viewport
                            settings
                            mesh
                            group.origin
                            group.balls
                            textures
                            model.orbit
                    )
    in
        model.groups |> List.filterMap makeGroupEntity



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


port loadFluidGradientTextures : ({ value: List String, layer: Layer.JsIndex } -> msg) -> Sub msg

port requestRegenerateFluidGradients : ({ layer: Layer.JsIndex } -> msg) -> Sub msg

port refreshFluid :
    ( { layer : Layer.JsIndex }
    -> msg) -> Sub msg

port changeFluidVariety :
    ( { layer : Layer.JsIndex
      , value : Float
      }
    -> msg) -> Sub msg

port changeFluidOrbit :
    ( { layer : Layer.JsIndex
      , value : Float
      }
    -> msg) -> Sub msg

port buildFluidGradientTextures : ( Layer.JsIndex, E.Value ) -> Cmd msg
