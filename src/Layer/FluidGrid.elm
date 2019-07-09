module Layer.FluidGrid exposing
    ( Model
    , init
    , view
    )

import Set exposing (Set)
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


type Direction
    = North
    | West
    | South
    | East
    | NorthWest
    | NorthEast
    | SouthWest
    | SouthEast

type Step
    = Stop
    | InDirection Direction


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


initTaken : Size -> Taken
initTaken { width, height } =
    Taken
        { asGrid =
            Array.repeat height False
                |> Array.repeat width
        , asArray = Array.empty
        }


storeTaken : GridPos -> Taken -> Taken
storeTaken pos taken = taken


allTaken : Size -> Taken -> Bool
allTaken { width, height } (Taken { asArray }) =
    Array.length asArray >= width * height

-- cellStep : GridPos -> Taken -> Random.Generator Report
-- cellStep pos taken =
--     Random.constant ( GridIsFull, taken )


move : Direction -> GridPos -> GridPos
move direction { x, y } =
    case direction of
        NorthWest -> { x = x - 1, y = y - 1 }
        North ->     { x = x,     y = y - 1 }
        NorthEast -> { x = x + 1, y = y - 1 }
        East ->      { x = x - 1, y = y     }
        West ->      { x = x + 1, y = y     }
        SouthWest -> { x = x - 1, y = y + 1 }
        South ->     { x = x,     y = y + 1 }
        SouthEast -> { x = x + 1, y = y + 1 }


randomStep : Set Direction -> Random.Generator Step
randomStep among = Random.constant Stop


possibleMoves : Size -> Taken -> GridPos -> Set Direction
possibleMoves { width, height } (Taken { asGrid }) pos =
    let
        currentMoves = Set.empty
        addIfFree { x, y } step set = Set.empty
    in
        currentMoves
            |> addIfFree (pos |> move NorthWest) NorthWest
            |> addIfFree (pos |> move North) North
            |> addIfFree (pos |> move NorthEast) NorthEast
            |> addIfFree (pos |> move East) East
            |> addIfFree (pos |> move West) West
            |> addIfFree (pos |> move SouthWest) SouthWest
            |> addIfFree (pos |> move South) South
            |> addIfFree (pos |> move SouthEast) SouthEast


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
        -- get info about the edges and taken tiles around the current cell
        -- then provide the list of possible moves to the `randomStep`,
        -- but ensure it is not empty. If it is empty, finish the current group,
        -- find the next free cell and provide it to the generator.
        -- Add `Stop` to all the sets.
        -- Then, when we get the new step, it is for sure exists in the grid and
        -- so we may advance the generator futher, but before — add the posigion to
        -- the `Taken` storage.
        -- When the generated value is `Stop`, finish the current group,
        -- find the next free cell and provide it to the generator.
        possibleMoves size taken curPos
            |> randomStep
            |> Random.andThen
                (\nextStep ->
                    case nextStep of
                        Stop ->
                            -- start new group
                            groupsStep size curPos curGroup groups taken
                        InDirection direction ->
                            groupsStep size (curPos |> move direction) curGroup groups taken
                )


generator : Size -> Random.Generator (List (List Ball))
generator size =
    groupsStep
        size
        { x = 0, y = 0 }
        0
        []
        (initTaken size)
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

