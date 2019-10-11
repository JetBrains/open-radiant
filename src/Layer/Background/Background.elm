port module Layer.Background.Background exposing
    ( id
    , def
    , Model
    , Msg
    )


import Html exposing (Html, div)
import Html.Attributes  as H exposing (class)

import Json.Encode as E exposing (..)
import Json.Decode as D exposing (..)

import Math.Vector2 as Vec2 exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Attributes as S exposing (style, id)

import Viewport exposing (Viewport)

import Model.Product as Product exposing (..)
import Model.Layer.Def exposing (Kind(..), DefId)
import Model.Layer.Def as Layer exposing (Def, JsIndex, Index, passResponse, initWith)
import Model.Layer.Context exposing (..)

import Model.Layer.Blend.Html as Html exposing (..)

import Gradient as G exposing (..)
import Gradient as Gradient exposing (decode)

import Color
import Color.Convert as Color exposing (..)
import Color.Manipulate as Color exposing (..)


id : DefId
id = "background"


def : Layer.Def Model (Html Msg) Msg Html.Blend
def =
    { id = id
    , kind = Html
    , init = Layer.initWith init
    , encode = encode
    , decode = decode
    , subscribe = subscriptions
    , update = update
    , response = Layer.passResponse
    , view = view
    , gui = Nothing
    }


type Msg
    = SwitchStop Int Bool -- StopIndex and StopState
    | SwitchGradientOrientation Orientation


type alias Color = String


type StopState
    = On
    | Off


type StopId
    = StopI
    | StopII
    | StopIII


type StopStates = StopStates StopState StopState StopState


type alias Model =
    { stops : StopStates
    , opacity : Float
    , orientation: Orientation
    }


defaultOrientation : Orientation
defaultOrientation = Vertical


defaultStops : StopStates
defaultStops = StopStates On On Off


init : Model
init =
    { stops = defaultStops
    , opacity = 1.0
    , orientation = defaultOrientation
    }


update : Index -> Context -> Msg -> Model -> ( Model, Cmd Msg )
update _ ctx msg model =
    case msg of
        SwitchStop stopIndex value ->
            (
                { model
                | stops = switchStop
                            (indexToStopId stopIndex)
                            (boolToStopState value)
                            model.stops
                }
            , Cmd.none
            )
        SwitchGradientOrientation orientation ->
            (
                { model
                | orientation = orientation
                }
            , Cmd.none
            )


view : Index -> Context -> Maybe Html.Blend -> Model -> Html Msg
view _ ctx maybeBlend model =
    let
        ( w, h ) = ctx.size
            -- ( Vec2.getX viewport.size
            -- , Vec2.getY viewport.size
            -- )
    in
        div [ H.class "background-layer layer" ]
            [ renderBackground ( w, h ) model.stops model.opacity ctx.palette model.orientation
            ]



subscriptions : Context -> Model -> Sub ( Index, Msg )
subscriptions ctx model =
    Sub.batch
        [ switchBackgroundStop
            (\{ layer, stopIndex, value } ->
                ( Layer.Index layer
                , SwitchStop stopIndex value
                )
            )
        , switchGradientOrientation
            (\{ layer, orientation } ->
                ( Layer.Index layer
                , SwitchGradientOrientation
                        <| Gradient.decodeOrientation orientation
                )
            )
        ]


renderBackground : ( Int, Int ) -> StopStates -> Float -> Palette -> Orientation -> Svg msg
renderBackground size stops opacity palette orientation =
    createGradient (adjust palette) stops orientation
        |> gradientToSvg size "x-gradient-background"


adjust : Palette -> Palette
adjust p =
    p |> Product.mapPalette (Color.hexToColor
       >> Result.withDefault (Color.black)
       >> Color.darken 0.25
       >> Color.saturate 1.0
       >> Color.colorToHex)


createGradient : Palette -> StopStates -> Orientation -> G.Gradient
createGradient (Palette c1 c2 c3) (StopStates stop1 stop2 stop3) orientation =
    { orientation = orientation
    , stops =
        case ( stop1, stop2, stop3 ) of
            ( On, On, On ) ->
                [ ( 0, c1 )
                , ( 0.7, c2 )
                , ( 1, c3 )
                ]
            ( On, On, Off ) ->
                [ ( 0, c1 )
                , ( 1, c2 )
                ]
            ( On, Off, On ) ->
                [ ( 0, c1 )
                , ( 1, c3 )
                ]
            ( On, Off, Off ) ->
                [ ( 0, c1 )
                , ( 1, c1 )
                ]
            ( Off, On, On ) ->
                [ ( 0, c2 )
                , ( 1, c3 )
                ]
            ( Off, On, Off ) ->
                [ ( 0, c2 )
                , ( 1, c2 )
                ]
            ( Off, Off, On ) ->
                [ ( 0, c3 )
                , ( 1, c3 )
                ]
            ( Off, Off, Off ) ->
                [ ( 0, "#000000" )
                , ( 0.5, "#171717" )
                , ( 1.0, "#202020" )
                ]
    }


gradientToSvg : ( Int, Int ) -> String -> G.Gradient -> Svg msg
gradientToSvg ( w, h ) gradientId { stops, orientation } =
    let
        convertStop (offsetVal, colorVal)
            = stop
                [ offset <| String.fromFloat offsetVal, stopColor colorVal ]
                [ ]
        gradientKind kind =
            case kind of
                Radial -> radialGradient
                Horizontal -> linearGradient
                Vertical -> linearGradient

    in
    svg
        [ width  <| String.fromInt w
        , height <| String.fromInt h
        ]
        [ defs
            [ ]
            [ gradientKind orientation
                [ S.id gradientId
                , x1 "0%"
                , y1 "0%"
                -- , x2 "0%"
                -- , y2 "100%"
                , x2 <| if orientation == G.Horizontal then "100%" else "0%"
                , y2 <| if orientation == G.Horizontal then "0%" else "100%"
                ]
                <| List.map convertStop stops
            ]
        , rect
            [ x "0"
            , y "0"
            , width <| String.fromInt w
            , height <| String.fromInt h
            , fill <| "url(#" ++ gradientId ++ ")"
            ]
            [ ]
        ]

switchStop : StopId -> StopState -> StopStates -> StopStates
switchStop stopIndex value (StopStates stop1 stop2 stop3) =
    StopStates
        (if stopIndex == StopI then value else stop1)
        (if stopIndex == StopII then value else stop2)
        (if stopIndex == StopIII then value else stop3)


indexToStopId : Int -> StopId
indexToStopId index =
    case index of
        0 -> StopI
        1 -> StopII
        2 -> StopIII
        _ -> StopI


boolToStopState : Bool -> StopState
boolToStopState v = if v then On else Off


encode : Context -> Model -> E.Value
encode _ model =
    let
        encodeStopState state =
            E.string <|
                case state of
                    On -> "on"
                    Off -> "off"
        encodeStopStates (StopStates stop1 stop2 stop3) =
            E.list encodeStopState [ stop1, stop2, stop3 ]
        encodeOrientation orientation =
            E.string <|
                case orientation of
                    Vertical -> "vertical"
                    Horizontal -> "horizontal"
                    Radial -> "radial"
    in
        E.object
            [ ( "opacity", E.float model.opacity )
            , ( "stops", encodeStopStates model.stops )
            , ( "orientation", encodeOrientation model.orientation )
            ]


decodeStops : D.Decoder StopStates
decodeStops =
    D.list D.string
        |> D.map
            (\stopStatesList ->
                case stopStatesList of
                    (stop1::stop2::stop3::_) ->
                        StopStates
                            (if stop1 == "on" then On else Off)
                            (if stop2 == "on" then On else Off)
                            (if stop3 == "on" then On else Off)
                    _ -> defaultStops
            )


decodeOrientation : D.Decoder Orientation
decodeOrientation =
    D.string
        |> D.map
            (\string ->
                case string of
                    "vertical" -> Vertical
                    "horizontal" -> Horizontal
                    "radial" -> Radial
                    _ -> defaultOrientation
            )


decode : Context -> D.Decoder Model
decode _ =
    D.map3
        (\opacity stops orientation ->
            { opacity = opacity
            , stops = stops
            , orientation = orientation
            }
        )
        (D.field "opacity" D.float)
        (D.field "stops" decodeStops)
        (D.field "orientation" decodeOrientation)


port switchBackgroundStop :
    ( { layer : Layer.JsIndex
      , stopIndex : Int
      , value : Bool
      }
    -> msg) -> Sub msg

port switchGradientOrientation :
    (
        { layer: Layer.JsIndex
        , orientation : String
        }
    -> msg) -> Sub msg
