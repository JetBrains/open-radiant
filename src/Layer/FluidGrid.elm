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


type alias Size = { width : Int, height : Int }
type alias GridPos = { x : Int, y : Int }
type alias Groups = List (List Ball)


type alias Model =
    { groups : List (List Ball)
    , size : Size
    }


type alias Taken = Array (Array Bool)


-- cellStep : GridPos -> Taken -> Random.Generator Report
-- cellStep pos taken =
--     Random.constant ( GridIsFull, taken )


randomStep : Random.Generator Step
randomStep = Random.constant Stop


groupsStep : GridPos -> Groups -> Taken -> Maybe Report -> Random.Generator ( Groups, GridPos, Taken )
groupsStep curPos groups taken lastReport =
    case lastReport of
        Just GridIsFull -> Random.constant ( groups, curPos, taken )
        _ -> randomStep
            |> Random.andThen
                (\nextStep ->
                    groupsStep curPos groups taken (Just GridIsFull)
                )


generator : Size -> Random.Generator (List (List Ball))
generator { width, height } =
    let
        taken =
            Array.repeat height False
                |> Array.repeat width
    in
        groupsStep
            { x = 0, y = 0 }
            []
            taken
            Nothing
                |> Random.map (\(groups, _, _) -> groups)


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
    , size = { width = 10, height = 10 }
    }


view : Model -> Html msg
view model =
    let
        drawGroup index balls = div [] ( balls |> List.indexedMap drawBall )
        drawBall index ball = div [] [ String.fromInt index |> text ]
    in
        div [] (model.groups |> List.indexedMap drawGroup)

