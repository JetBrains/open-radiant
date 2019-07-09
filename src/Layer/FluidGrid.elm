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


type Taken =
    Taken
        { asGrid : Array (Array Bool)
        , asArray : Array GridPos
        }


initTaken : Taken
initTaken = Taken { asGrid = Array.empty, asArray = Array.empty }


storeTaken : GridPos -> Taken -> Taken
storeTaken pos taken = taken


allTaken : Size -> Taken -> Bool
allTaken { width, height } (Taken { asArray }) =
    Array.length asArray >= width * height

-- cellStep : GridPos -> Taken -> Random.Generator Report
-- cellStep pos taken =
--     Random.constant ( GridIsFull, taken )


randomStep : Random.Generator Step
randomStep = Random.constant Stop


groupsStep
     : Size
    -> GridPos
    -> Int
    -> Groups
    -> Taken
    -> Random.Generator ( Groups, GridPos, Taken )
groupsStep size curPos curGroup groups taken =
    if not <| allTaken size taken then
        Random.constant ( groups, curPos, taken )
    else
        randomStep
            |> Random.andThen
                (\nextStep ->
                    groupsStep size curPos curGroup groups taken
                )


generator : Size -> Random.Generator (List (List Ball))
generator ({ width, height } as size) =
    let
        taken =
            Array.repeat height False
                |> Array.repeat width
    in
        groupsStep
            size
            { x = 0, y = 0 }
            0
            []
            initTaken
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

