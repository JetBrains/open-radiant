module Layer.Cover.Cover exposing
    ( id
    , init
    , view
    , def
    , Model
    )

import Html exposing (..)
import Html.Attributes exposing (style, class, attribute, contenteditable)

import Json.Encode as E
import Json.Decode as D

import Model.Layer.Blend.Html as Html
import Model.Layer.Blend.Html as Blend exposing (encode)
import Model.AppMode exposing (AppMode(..))
import Model.Product as Product exposing (Product)
import Model.Product exposing (..)

import Model.Layer.Context exposing (Context)
import Model.Layer.Def exposing (Kind(..), DefId)
import Model.Layer.Def as Layer exposing (Def)
import Model.Layer.Def as Def exposing (unit)


id : DefId
id = "cover"


def : Layer.Def Model (Html ()) () Html.Blend
def =
    { id = id
    , kind = Html
    , init = \_ -> ( init, Cmd.none )
    , encode = always <| always <| E.object []
    , decode = always <| D.succeed init
    , subscribe = \_ _ -> Sub.none
    , update = \_ _ model -> ( model, Cmd.none )
    , view = view
    , gui = Nothing
    }


type alias Model = {}


defaultSize = 110
defaultWidth = 1500.0
-- imageWidth : Int
-- imageWidth = 120
-- imageHeight : Int
-- imageHeight = 120
scaleFactor : Float
scaleFactor = 0.1


init : Model
init = {}


view : Context -> Maybe Html.Blend -> Model -> Html ()
view ctx maybeBlend model =
    let
        ( w, h ) = ctx.size
        ( x, y ) = ctx.origin
        scale = toFloat w / defaultWidth
        centerX = (toFloat w / 2) - toFloat x
        centerY = (toFloat h / 2) - toFloat y
        logoX = toFloat w - toFloat x - 0.1 * toFloat h
        logoY = toFloat h - toFloat y - 0.1 * toFloat h
    in
        div
            [ class "cover-layer"
            --, style "mix-blend-mode" <| Blend.encode blend
            , style "position" "absolute"
            , style "top" "0px"
            , style "left" "0px"
            , style "font-size" <| String.fromInt defaultSize ++ "px"
            , style "font-family" "'Gotham', Helvetica, sans-serif"
            , style "font-weight" "170"
                -- , ("text-transform", "uppercase")
            , style "color" "white"
            ]
        ( if
            (ctx.mode == Production)
            || (ctx.mode == Player)
            || (ctx.mode == TronUi Production) then
            [ productName
                    ctx.product
                    ( centerX, centerY )
                    ( maybeBlend |> Maybe.withDefault Blend.Normal )
                    ( 0.8 * scale )
            , logo ( logoX, logoY ) Blend.Normal ( 0.6 * scale )
            ]
          else
            [
            -- title product
            --, logo product posX posY logoPath blend scale
            ]
        )


productName : Product -> ( Float, Float ) -> Html.Blend -> Float -> Html a
productName product pos blend scale =
    let
        textPath = "./assets/" ++ Product.getTextLinePath product
        textSize = Product.getCoverTextSize product
    in
        image
            textPath
            ("product-name-layer product-name-layer-" ++ Product.encode product)
            pos
            textSize
            blend
            scale



logo : ( Float, Float ) -> Html.Blend -> Float -> Html a
logo ( logoX, logoY ) blend scale =
    let
        logoPath = "./assets/" ++ Product.getLogoPath Product.JetBrains
        ( logoWidth, logoHeight ) = ( 90, 90 )
    in image
            logoPath
            ("logo-layer logo-layer-" ++ Product.encode JetBrains)
            ( logoX, logoY )
            ( logoWidth, logoHeight )
            blend
            scale


image : String -> String -> ( Float, Float ) -> ( Int, Int ) -> Html.Blend -> Float -> Html a
image imagePath imgClass ( posX, posY ) ( imageWidth, imageHeight ) blend scale =
    div
        [ class imgClass
        ,
            { blend = Blend.encode blend
            , posX = posX
            , posY = posY
            , width = imageWidth
            , height = imageHeight
            , imagePath = imagePath
            , scale = scale
            }
            |> encodeStoredData
            |> E.encode 0
            |> attribute "data-stored"
        , style "mix-blend-mode" <| Blend.encode blend
        , style "position" "absolute"
        , style "top" "0px"
        , style "left" "0px"
        , style "width" <| String.fromFloat ( toFloat imageWidth * scale ) ++ "px"
        , style "height" <| String.fromFloat ( toFloat imageHeight * scale ) ++ "px"
        , style "transform" <| "translate("
                ++ String.fromFloat (posX - (toFloat imageWidth * scale) / 2.0) ++ "px, "
                ++ String.fromFloat (posY - (toFloat imageHeight * scale) / 2.0) ++ "px)"
        , style "background-image" <| "url(\"" ++ imagePath ++ "\")"
        , style "background-repeat" "no-repeat"
        , style "background-position" "center center"
        , style "background-size" "contain"
        ]
        [ --img [ HAttrs.src logoPath, HAttrs.attribute "crossorigin" "anonymous" ] []
        ]


title : Product -> Html a
title product =
    div
        [ class
            ("text-layer--title text-layer--" ++ Product.encode product)
        , style "max-width" "800px"
--            ,("mix-blend-mode", Blend.encode blend)
--                , ("position", "absolute")
--                , ("top", toString posY ++ "px")
--                , ("left", toString posX ++ "px")
--                , ("transform", "scale(" ++ toString scale ++ ")")
        , style "font-size" <| String.fromInt defaultSize ++ "px"
        , style "font-family" "'Gotham', Helvetica, sans-serif"
        , style "font-weight" "170"
              -- , ("text-transform", "uppercase")
        , style "color" "white"
        , contenteditable True
        ]
        [ text <| getName product ]


type alias StoredData =
    { scale : Float
    , posX : Float
    , posY : Float
    , blend : String
    , imagePath : String
    , width : Int
    , height : Int
    }


encodeStoredData : StoredData -> E.Value
encodeStoredData s =
    E.object
        [ ( "scale", E.float s.scale )
        , ( "posX", E.float s.posX )
        , ( "posY", E.float s.posY )
        , ( "blend", E.string s.blend )
        , ( "imagePath", E.string s.imagePath )
        , ( "width", E.int s.width )
        , ( "height", E.int s.height )
        ]
