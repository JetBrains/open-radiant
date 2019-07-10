module Layer.FluidGrid exposing
    ( Model
    , init
    , view
    , generate, generator
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
        East ->      { x = x + 1, y = y     }
        West ->      { x = x - 1, y = y     }
        SouthWest -> { x = x - 1, y = y + 1 }
        South ->     { x = x,     y = y + 1 }
        SouthEast -> { x = x + 1, y = y + 1 }


randomStep : DirectionSet -> Random.Generator Step
randomStep among =
    if not <| ASet.isEmpty among then
        let amongArray = Array.fromList <| ASet.toList among
        in Random.int 0 (Array.length amongArray)
            |> Random.map
                (\index ->
                    -- if the value will happen to equal the count of possible moves
                    -- then `Stop` will be produced, which is what we wanted to always
                    -- consider as an option
                    Array.get index amongArray
                        |> Maybe.map InDirection
                        |> Maybe.withDefault Stop
                )
    else
        Random.constant Stop


possibleMoves : Size -> Taken -> GridPos -> DirectionSet
possibleMoves { width, height } taken pos =
    let
        addIfFree direction freeMoves =
            let ({ x, y } as posAtDirection) = pos |> move direction
            in
                if ((x < 0) || (y < 0) || (x >= width) || (y >= height))
                then freeMoves
                else
                    if posAtDirection |> isTakenAmong taken
                    then freeMoves
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
addEmptyGroupTo = Array.push Array.empty


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
                    Array.foldl (foldX y) (posFound, 0) row
                        |> Tuple.first
                        |> (\foundInRow -> ( foundInRow, y + 1 ))
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
                                        Random.lazy (\_ ->
                                            groupsStep
                                                size
                                                nextPos
                                                (addEmptyGroupTo groups
                                                    |> addToTheLastGroup nextPos)
                                                (taken |> storeTaken nextPos)
                                        )
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
                                    Random.lazy (\_ ->
                                        groupsStep
                                            size
                                            nextPos
                                            (groups |> addToTheLastGroup nextPos)
                                            (taken |> storeTaken nextPos)
                                    )
                            -- else, stop the recursion
                            else Random.constant ( groups, curPos, taken )

                )
    else
        -- stop the recursion
        Random.constant ( groups, curPos, taken )


generator : Size -> Random.Generator Model
generator windowSize =
    let
        size = { width = 20, height = 20 }
    in
        groupsStep
            size
            { x = 0, y = 0 }
            Array.empty
            (initTaken size)
                |> Random.map (\(groups, _, _) -> groups)
                |> Random.map
                    (\groups ->
                        { size = size
                        , groups = groups
                        }
                    )


generate : (Model -> msg) -> Random.Generator Model -> Cmd msg
generate = Random.generate


init : Model
init =
    { groups = Array.empty
    , size = { width = 20, height = 20 }
    }


view : Model -> Html msg
view model =
    let
        groupsList = groupsToListsOfBalls model.groups
        drawGroup index balls = div [] ( balls |> List.indexedMap drawBall )
        drawBall index ball = div [] [ String.fromInt index |> text ]
    in
        div [] (groupsList |> List.indexedMap drawGroup)

