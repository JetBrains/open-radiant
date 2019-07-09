module Layer.FluidGrid exposing
    ( Model
    , init
    , view
    )

import Array exposing (Array)
import Random exposing (..)
import Math.Vector2 as Vec2 exposing (vec2, Vec2)
import Html exposing (..)


-- type Corner
--     = None
--     | TopLeft
--     | LeftSide
--     | RightSide
--     | TopSide


type Ball = Ball Vec2


type Step
    = Stop
    | GoNorth
    | GoWest
    | GoSouth
    | GoEast
    | GoNorthWest
    | GoNorthEast
    | GoSouthWest
    | GoSouthEast


type Report
    = Decision Step
    | MetEdge
    | MetTakenTile
    | GridIsFull


type alias Model =
    { groups : List (List Ball)
    , size : Vec2
    }


type alias Taken = Array (Array Bool)


nextStep : Taken -> Random.Generator Report
nextStep taken =
    Random.constant GridIsFull


generator : Random.Generator (List (List Ball))
generator =
    Random.constant []


init : Model
init =
    { groups =
        [
            [ Ball <| vec2 0 0
            , Ball <| vec2 1 0
            , Ball <| vec2 2 0
            , Ball <| vec2 0 1
            ]
        ]
    , size = vec2 10 10
    }


view : Model -> Html msg
view model =
    let
        drawGroup index balls = div [] ( balls |> List.indexedMap drawBall )
        drawBall index ball = div [] [ String.fromInt index |> text ]
    in
        div [] (model.groups |> List.indexedMap drawGroup)

