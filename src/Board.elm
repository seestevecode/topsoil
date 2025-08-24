module Board exposing (..)

import Constants as Const
import Set
import Simplex
import Types exposing (..)
import Utils


getBaseFromCoord : Int -> Coord -> Base
getBaseFromCoord initInt ( x, y ) =
    Simplex.noise2d
        (Simplex.permutationTableFromInt initInt)
        (toFloat x * Const.terrainScale)
        (toFloat y * Const.terrainScale)
        |> (\float ->
                if float < -(1 / 3) then
                    Base1

                else if float < 1 / 3 then
                    Base2

                else
                    Base3
           )


advanceGameStateAfterPlacement : Board -> GameState
advanceGameStateAfterPlacement board =
    let
        emptyCells =
            List.filter (\cell -> cell.content == Nothing) board.grid

        boardFull =
            List.length emptyCells == 0
    in
    if boardFull && Tuple.first board.queue /= Harvester then
        GameOver

    else
        Playing


growCell : Cell -> Cell
growCell cell =
    { cell
        | content =
            case cell.content of
                Just (Plant token bonus) ->
                    if token == Disappearing 1 then
                        Nothing

                    else
                        Just <| Plant (growToken token) bonus

                _ ->
                    Nothing
    }


growToken : Token -> Token
growToken token =
    case token of
        Growing1 untilGrowth ->
            if untilGrowth > 1 then
                Growing1 (untilGrowth - 1)

            else
                Grown1

        Growing2 untilGrowth ->
            if untilGrowth > 1 then
                Growing2 (untilGrowth - 1)

            else
                Grown2

        Growing3 untilGrowth ->
            if untilGrowth > 1 then
                Growing3 (untilGrowth - 1)

            else
                Grown3

        Disappearing untilGone ->
            Disappearing (untilGone - 1)

        _ ->
            token


clearHarvest : Grid -> List Coord -> Grid
clearHarvest grid harvestCoords =
    List.map
        (\cell ->
            if List.member cell.coord harvestCoords then
                { cell | base = nextBase cell.base, content = Nothing }

            else
                cell
        )
        grid


scoreHarvest : List Cell -> Int
scoreHarvest cells =
    let
        bonusCount =
            List.map .content cells
                |> List.filterMap identity
                |> List.map getBonusFromContent
                |> List.filterMap identity
                |> List.filter (\bonus -> bonus == Bonus)
                |> List.length

        harvestToken =
            cells
                |> List.head
                |> Maybe.withDefault
                    { base = Base1, coord = ( 0, 0 ), content = Nothing }
                |> getTokenFromCell
                |> Maybe.withDefault Standard1

        tokenMultiplier =
            Const.tokenDetails harvestToken |> .baseScore |> Maybe.withDefault 0
    in
    List.indexedMap (\index _ -> (index + bonusCount + 1) * tokenMultiplier)
        cells
        |> List.sum


getTokenFromCell : Cell -> Maybe Token
getTokenFromCell cell =
    case cell.content of
        Just (Plant token _) ->
            Just token

        _ ->
            Nothing


getBonusFromContent : Content -> Maybe Bonus
getBonusFromContent content =
    case content of
        Plant _ bonus ->
            Just bonus

        Harvester ->
            Nothing


harvestFrom : Grid -> Coord -> List Cell
harvestFrom grid initCoord =
    { toCheck = Set.singleton initCoord
    , toHarvest = Set.singleton initCoord
    , checked = Set.empty
    }
        |> Utils.applyUntil (\acc -> acc.toCheck == Set.empty) (harvestStep grid)
        |> .toHarvest
        |> Set.toList
        |> List.map (getCell grid)
        |> List.filterMap identity


harvestStep : Grid -> HarvestInfo -> HarvestInfo
harvestStep grid harvestInfo =
    let
        checkCoord =
            Set.toList harvestInfo.toCheck
                |> List.head
                |> Maybe.withDefault ( -1, -1 )

        checkedCoordSet =
            Set.singleton checkCoord
    in
    case matchedNeighbours grid checkCoord of
        [] ->
            { harvestInfo
                | toCheck = Set.diff harvestInfo.toCheck checkedCoordSet
                , checked = Set.union harvestInfo.checked checkedCoordSet
            }

        matches ->
            { harvestInfo
                | toCheck =
                    Set.diff harvestInfo.toCheck checkedCoordSet
                        |> Set.union
                            (Set.diff
                                (Set.fromList matches)
                                harvestInfo.checked
                            )
                , toHarvest =
                    Set.union harvestInfo.toHarvest (Set.fromList matches)
                , checked =
                    Set.union harvestInfo.checked checkedCoordSet
            }


matchedNeighbours : Grid -> Coord -> List Coord
matchedNeighbours grid coord =
    List.map (neighbour grid coord) [ Above, Right, Below, Left ]
        |> List.filterMap identity
        |> List.map .coord
        |> List.filter (harvestMatch grid coord)


harvestMatch : Grid -> Coord -> Coord -> Bool
harvestMatch grid coordA coordB =
    let
        cellA =
            getCell grid coordA

        cellB =
            getCell grid coordB
    in
    case ( cellA, cellB ) of
        ( Just a, Just b ) ->
            harvestMatchCells a b

        _ ->
            False


harvestMatchCells : Cell -> Cell -> Bool
harvestMatchCells cellA cellB =
    let
        baseMatch =
            cellA.base == cellB.base

        contentMatch =
            case ( cellA.content, cellB.content ) of
                ( Just (Plant tokenA _), Just (Plant tokenB _) ) ->
                    harvestMatchTokens tokenA tokenB

                _ ->
                    False
    in
    baseMatch && contentMatch


harvestMatchTokens : Token -> Token -> Bool
harvestMatchTokens tokenA tokenB =
    case ( tokenA, tokenB ) of
        ( Disappearing _, Disappearing _ ) ->
            True

        ( _, _ ) ->
            tokenA == tokenB


placeTokenOnGrid : Grid -> Content -> Coord -> Grid
placeTokenOnGrid oldGrid newContent targetCoord =
    List.map
        (\cell ->
            if cell.coord == targetCoord then
                { cell | content = Just newContent }

            else
                cell
        )
        oldGrid


getRow : Int -> Grid -> List Cell
getRow row grid =
    List.filter (\cell -> Tuple.first cell.coord == row) grid


flushTo : Grid -> Cell -> Direction -> Bool
flushTo grid cell direction =
    (neighbourBase grid cell.coord direction == Just cell.base)
        || (neighbourBase grid cell.coord direction == Nothing)


cornerBaseMatch : Grid -> Cell -> List Direction -> Bool
cornerBaseMatch grid cell directions =
    let
        neighbourBases =
            List.map (neighbourBase grid cell.coord) directions
                |> List.filterMap identity
    in
    List.member cell.base neighbourBases


neighbourBase : Grid -> Coord -> Direction -> Maybe Base
neighbourBase grid coord direction =
    Maybe.map .base <| neighbour grid coord direction


neighbour : Grid -> Coord -> Direction -> Maybe Cell
neighbour grid ( x, y ) direction =
    let
        neighbourCoord =
            case direction of
                Above ->
                    ( x - 1, y )

                Right ->
                    ( x, y + 1 )

                Below ->
                    ( x + 1, y )

                Left ->
                    ( x, y - 1 )
    in
    getCell grid neighbourCoord


getCell : Grid -> Coord -> Maybe Cell
getCell grid coord =
    List.filter (\cell -> cell.coord == coord) grid |> List.head


nextBase : Base -> Base
nextBase base =
    case base of
        Base1 ->
            Base2

        Base2 ->
            Base3

        Base3 ->
            Base1


getTokenCount : Token -> Maybe Int
getTokenCount token =
    case token of
        Growing1 count ->
            Just count

        Growing2 count ->
            Just count

        Growing3 count ->
            Just count

        Disappearing count ->
            Just count

        _ ->
            Nothing
