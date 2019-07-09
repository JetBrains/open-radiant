module Layer.FluidGrid exposing
    ( Model
    , init
    , view
    )


import Set.Any exposing (AnySet)
import Set.Any as ASet exposing (insert, empty)
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


type alias DirectionSet = AnySet Int Direction -- it is better to be Set, but we can't


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


directionToInt : Direction -> Int
directionToInt direction =
    case direction of
        North -> 0
        West -> 1
        South -> 2
        East -> 3
        NorthWest -> 4
        NorthEast -> 5
        SouthWest -> 6
        SouthEast -> 7


-- To use in `AnySet` builders
asDirectionSet : Direction -> Int
asDirectionSet = directionToInt


noDirections : DirectionSet
noDirections = ASet.empty asDirectionSet


allDirections : DirectionSet
allDirections =
    [ North, West, South, East, NorthWest, NorthEast, SouthWest, SouthEast ] |>
        List.foldl ASet.insert (ASet.empty asDirectionSet)


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
            Array.repeat width False
                |> Array.repeat height
        , asArray = Array.empty
        }


allTaken : Size -> Taken -> Bool
allTaken { width, height } (Taken { asArray }) =
    Array.length asArray >= width * height


isTakenAmong : Taken -> GridPos -> Bool
isTakenAmong (Taken { asGrid }) { x, y } =
    asGrid
        |> Array.get y
        |> Maybe.andThen (Array.get x)
        |> Maybe.withDefault True


storeTaken : GridPos -> Taken -> Taken
storeTaken ({ x, y } as pos) (Taken { asGrid, asArray }) =
    Taken
        { asArray =
            asArray |> Array.push pos
        , asGrid =
            Array.get y asGrid
                |> Maybe.map (Array.set x True)
                |> Maybe.map (\newRow -> Array.set y newRow asGrid)
                |> Maybe.withDefault asGrid
        }


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


randomStep : DirectionSet -> Random.Generator Step
randomStep among =
    -- TODO: add `Stop` to the values, return constant `Stop` when among is empty
    Random.constant Stop


possibleMoves : Size -> Taken -> GridPos -> DirectionSet
possibleMoves { width, height } taken pos =
    let
        addIfFree direction freeMoves =
            let ({ x, y } as posAtDirection) = pos |> move direction
            in
                if ((x < 0) || (y < 0))
                then noDirections
                else
                    if posAtDirection |> isTakenAmong taken
                    then noDirections
                    else ASet.insert direction freeMoves
    in
        allDirections |> ASet.foldl addIfFree noDirections


maxGroups : Int
maxGroups = 50 -- additional control for the recursion/randomness overflow
maxBalls : Int
maxBalls = 200 -- additional control for the recursion/randomness overflow


addToTheLastGroup : GridPos -> Groups -> Groups
addToTheLastGroup pos groups =
    groups


addEmptyGroupTo : Groups -> Groups
addEmptyGroupTo groups = groups


ballsInTheLastGroup : Groups -> Int
ballsInTheLastGroup groups = 0


groupsCount : Groups -> Int
groupsCount = List.length


findPosToStartNextGroup : Groups -> Taken -> Maybe GridPos
findPosToStartNextGroup groups taken = Nothing


groupsStep
     : Size
    -> GridPos
    -> Groups
    -> Taken
    -> Random.Generator ( Groups, GridPos, Taken )
groupsStep size curPos groups taken =
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
                            -- start new group if not reached maxGroups
                            if (groupsCount groups < maxGroups)
                                then
                                    case findPosToStartNextGroup groups taken of
                                        Just nextPos ->
                                            groupsStep
                                                size
                                                nextPos
                                                (addEmptyGroupTo groups
                                                    |> addToTheLastGroup nextPos)
                                                taken
                                        Nothing
                                            -> Random.constant ( groups, curPos, taken )
                                else
                                    Random.constant ( groups, curPos, taken )
                        InDirection direction ->
                            if ballsInTheLastGroup groups < maxBalls
                            then
                                let nextPos = curPos |> move direction
                                in
                                    groupsStep
                                        size
                                        nextPos
                                        (groups |> addToTheLastGroup nextPos)
                                        (taken |> storeTaken nextPos)
                            else Random.constant ( groups, curPos, taken )
                )


generator : Size -> Random.Generator (List (List Ball))
generator size =
    groupsStep
        size
        { x = 0, y = 0 }
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

