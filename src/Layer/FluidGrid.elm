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


type alias Size = { width : Int, height : Int }
type alias GridPos = { x : Int, y : Int }
type alias Groups = Array (Array Ball)


groupsToListsOfBalls : Groups -> List (List Ball)
groupsToListsOfBalls = Array.toList >> List.map Array.toList


type Ball = Ball GridPos


type Direction
    = North
    | West
    | South
    | East
    | NorthWest
    | NorthEast
    | SouthWest
    | SouthEast


type alias DirectionSet = AnySet Int Direction
-- it is better to be just a Set, but we can't use sum types as `Set` values,
-- so we use `AnySet` which allows it


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


type alias Model =
    { groups : Groups
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
    if not <| ASet.isEmpty among then
        Random.int 0 8
            |> Random.map
                (\val ->
                    case val of
                        0 -> InDirection North
                        1 -> InDirection West
                        2 -> InDirection South
                        3 -> InDirection East
                        4 -> InDirection NorthWest
                        5 -> InDirection NorthEast
                        6 -> InDirection SouthWest
                        7 -> InDirection SouthEast
                        8 -> Stop
                        _ -> Stop -- should never be reached, but anyway return `Stop`
                )
    else
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
    let
        lastGroupIndex = (Array.length groups) - 1
    in
        groups
            |> Array.get lastGroupIndex
            |> Maybe.map (Array.push (Ball pos))
            |> Maybe.map (\lastGroup -> Array.set lastGroupIndex lastGroup groups)
            |> Maybe.withDefault groups


addEmptyGroupTo : Groups -> Groups
addEmptyGroupTo groups = groups


ballsInTheLastGroup : Groups -> Maybe Int
ballsInTheLastGroup groups =
    groups
        |> Array.get ((Array.length groups) - 1)
        |> Maybe.map Array.length


groupsCount : Groups -> Int
groupsCount = Array.length


findPosToStartNextGroup : Size -> Taken -> Maybe GridPos
findPosToStartNextGroup size (Taken { asGrid }) =
    let
        foldX y isTaken ( posFound, x ) =
            case posFound of
                Nothing ->
                    if not isTaken then
                        ( Just { x = x, y = y }, x + 1 )
                    else ( posFound, x + 1 )
                Just _ -> ( posFound, x + 1 )
        foldY row ( posFound, y )  =
            case posFound of
                Nothing ->
                    Array.foldr (foldX y) (posFound, 0) row
                Just _ -> ( posFound, y + 1 )
    in
        Array.foldl foldY (Nothing, 0) asGrid
            |> Tuple.first


groupsStep
     : Size
    -> GridPos
    -> Groups
    -> Taken
    -> Random.Generator ( Groups, GridPos, Taken )
groupsStep size curPos groups taken =
    -- if not all the tiles are taken
    if not <| allTaken size taken then
        -- find possible moves for the current tile
        possibleMoves size taken curPos
            |> randomStep -- and generate the random one among them
            |> Random.andThen
                (\nextStep ->
                    -- then, we either randomly got the command to `Stop`
                    case nextStep of
                        Stop ->

                            -- a safety barrier not to come to the infinite cycle
                            -- (i.e. all the groups `Stop` without moving,
                            -- though anyway a group takes at least one tile,
                            -- and if there are no positions left, the grid is full)
                            if (groupsCount groups < maxGroups)
                            then

                                -- find the next position where we may start the group
                                case findPosToStartNextGroup size taken of
                                    -- if it was found, create new group and
                                    -- continue moving starting from the next point
                                    Just nextPos ->
                                        groupsStep
                                            size
                                            nextPos
                                            (addEmptyGroupTo groups
                                                |> addToTheLastGroup nextPos)
                                            (taken |> storeTaken nextPos)
                                    -- else, stop the recursion
                                    Nothing -> Random.constant ( groups, curPos, taken )

                            -- else, stop the recursion
                            else Random.constant ( groups, curPos, taken )

                        -- or to move in some direction
                        -- (guaranteed not to be taken, because it is
                        -- from the list of possible moves)
                        InDirection direction ->

                            -- a safety barrier not to come to the infinite cycle
                            if (ballsInTheLastGroup groups |> Maybe.withDefault 0) < maxBalls
                            then

                                -- move in the prescribed direction
                                let nextPos = curPos |> move direction
                                in
                                    -- and recursively continue moving starting from the next point
                                    groupsStep
                                        size
                                        nextPos
                                        (groups |> addToTheLastGroup nextPos)
                                        (taken |> storeTaken nextPos)

                            -- else, stop the recursion
                            else Random.constant ( groups, curPos, taken )

                )
    else
        -- stop the recursion
        Random.constant ( groups, curPos, taken )


generator : Size -> Random.Generator Groups
generator size =
    groupsStep
        size
        { x = 0, y = 0 }
        Array.empty
        (initTaken size)
            |> Random.map (\(groups, _, _) -> groups)


init : Model
init =
    { groups = Array.empty
    , size = { width = 10, height = 10 }
    }


view : Model -> Html msg
view model =
    let
        groupsList = groupsToListsOfBalls model.groups
        drawGroup index balls = div [] ( balls |> List.indexedMap drawBall )
        drawBall index ball = div [] [ String.fromInt index |> text ]
    in
        div [] (groupsList |> List.indexedMap drawGroup)

