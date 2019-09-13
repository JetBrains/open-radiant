module Layer.Background exposing
    ( Model
    , Mode(..)
    , StopState(..)
    , init
    , view
    , encode
    , decode
    , switchStop
    , indexToStopId
    , boolToStopState
    )


import Html exposing (Html, div)
import Html.Attributes  as H exposing (class)

import Json.Encode as E exposing (..)
import Json.Decode as D exposing (..)

import Math.Vector2 as Vec2 exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Attributes as S exposing (style)

import Viewport exposing (Viewport)

import Model.Product as Product exposing (..)

import Gradient as G exposing (..)
import Gradient as Gradient exposing (decode)

import Color
import Color.Convert as Color exposing (..)
import Color.Manipulate as Color exposing (..)



type alias Color = String


type StopState 
    = On 
    | Off


type StopId 
    = StopI 
    | StopII
    | StopIII    


type Mode
    = Fill Color
    | StopStates StopState StopState StopState


type alias Model =
    { mode : Mode
    , opacity : Float
    }


defaultMode : Mode
defaultMode = Fill "#171717"


init : Model
init =
    { mode = defaultMode
    , opacity = 1.0
    }


view : Viewport {} -> Product.Palette -> Model -> Html msg
view viewport palette model =
    let
        ( w, h ) =
            ( Vec2.getX viewport.size
            , Vec2.getY viewport.size
            )
    in
        div [ H.class "background-layer layer" ]
            [ renderBackground ( floor w, floor h ) model.mode model.opacity palette
            
            ]


renderBackground : ( Int, Int ) ->  Mode -> Float -> Palette -> Svg msg
renderBackground size mode opacity palette =
    createGradient (adjust palette) mode 
        |> gradientToSvg size "x-gradient-background"


adjust : Palette -> Palette
adjust p =
    p |> Product.mapPalette (Color.hexToColor
       >> Result.withDefault (Color.black)
       >> Color.darken 0.25
       >> Color.saturate 1.0
       >> Color.colorToHex) 


createGradient : Palette -> Mode -> G.Gradient
createGradient (Palette c1 c2 c3) mode =
    { orientation = G.Vertical
    , stops = 
        case mode of
            StopStates stop1 stop2 stop3 ->
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
                        , ( 0.7, "#202020" )
                        , ( 1.0, "#000000" )
                        ]                                                        
            Fill color -> 
                [ ( 0, color )
                , ( 1, color )
                ]
    }


gradientToSvg : ( Int, Int ) -> String -> G.Gradient -> Svg msg
gradientToSvg ( w, h ) gradientId { stops, orientation } =
    let
        convertStop (offsetVal, colorVal) 
            = stop 
                [ offset <| String.fromFloat offsetVal, stopColor colorVal ] 
                [ ]
    in
    svg
        [ width  <| String.fromInt w
        , height <| String.fromInt h
        ]
        [ defs
            [ ]
            [ linearGradient
                [ id gradientId
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

switchStop : StopId -> StopState -> Mode -> Mode
switchStop stopIndex value curMode =
    case curMode of
        Fill _ -> curMode
        StopStates stop1 stop2 stop3 ->
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


encode : Model -> E.Value
encode model =
    let
        encodeMode =
            case model.mode of
                Fill _ -> E.string "fill"
                StopStates _ _ _ -> E.string "gradient"
        encodeStopState state = 
            E.string <| 
                case state of
                    On -> "on"
                    Off -> "off"         
        encodeValue =
            case model.mode of
                Fill color ->
                    E.object
                        [ ( "color", E.string color ) ]
                StopStates stop1 stop2 stop3 ->
                    E.object
                        [ 
                            ( "stopStates"
                            , E.list encodeStopState [ stop1, stop2, stop3 ] 
                            ) 
                        ]
    in
        E.object
            [ ( "opacity", E.float model.opacity )
            , ( "mode", encodeMode )
            , ( "value", encodeValue )
            ]


decodeMode : D.Decoder Mode
decodeMode =
    D.map2
        (\maybeColor maybeStopsState ->
            case ( maybeColor, maybeStopsState ) of
                ( Just color, _ ) -> Fill color
                ( _, Just (stop1::stop2::stop3::_) ) -> 
                    StopStates
                        (if stop1 == "on" then On else Off)
                        (if stop2 == "on" then On else Off)
                        (if stop3 == "on" then On else Off)
                ( _, Just _ ) -> defaultMode                        
                ( Nothing, Nothing ) -> defaultMode
        )
        (D.maybe <| D.field "color" D.string)
        (D.maybe <| D.field "stopStates" <| D.list D.string)


decode : D.Decoder Model
decode =
    D.map3
        (\opacity mode modeValue ->
            { opacity = opacity
            , mode = modeValue -- TODO: ensure mode string corresponds to value?
            }
        )
        (D.field "opacity" D.float)
        (D.field "mode" D.string)
        (D.field "value" decodeMode)