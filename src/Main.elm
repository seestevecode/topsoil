module Main exposing (main)

import Browser
import Element as Ui
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import List.Extra as ListX
import Random
import Random.List
import Set exposing (Set)
import Simplex


main : Program Int Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { initialInt : Int
    , currentSeed : Random.Seed
    , gameState : GameState
    , board : Board
    , score : Int
    , undoAllowed : Bool
    , undoSeed : Random.Seed
    , undoBoard : Board
    , undoScore : Int
    }


type GameState
    = Playing
    | GameOver


type alias Board =
    { grid : Grid
    , queue : ( Content, List Content )
    }


type alias Grid =
    List Cell


type alias Cell =
    { coord : Coord
    , base : Base
    , content : Maybe Content
    }


type alias Coord =
    ( Int, Int )


type Base
    = Base1
    | Base2
    | Base3


type Content
    = Plant Token Bonus
    | Harvester


type Token
    = Standard1
    | Standard2
    | Standard3
    | Growing1 Int
    | Growing2 Int
    | Growing3 Int
    | Grown1
    | Grown2
    | Grown3
    | Disappearing Int


type Bonus
    = Bonus
    | NoBonus


type Direction
    = Above
    | Right
    | Below
    | Left


type Msg
    = NoOp
    | PlaceTokenOnBoard Coord
    | Harvest Coord
    | Undo


init : Int -> ( Model, Cmd Msg )
init intFromDate =
    let
        ( grid, gridSeed ) =
            initGrid intFromDate

        ( queue, queueSeed ) =
            Random.step (queueGenerator 0) gridSeed

        queueHead =
            List.head queue |> Maybe.withDefault Harvester

        queueTail =
            List.tail queue |> Maybe.withDefault []

        newBoard =
            { grid = grid
            , queue = ( queueHead, queueTail ++ [ Harvester ] )
            }
    in
    ( { initialInt = intFromDate
      , currentSeed = queueSeed
      , gameState = Playing
      , board = newBoard
      , score = 0
      , undoAllowed = False
      , undoSeed = queueSeed
      , undoBoard = { grid = [], queue = ( Harvester, [] ) }
      , undoScore = 0
      }
    , Cmd.none
    )


initGrid : Int -> ( Grid, Random.Seed )
initGrid initInt =
    let
        initSeed =
            Random.initialSeed initInt

        initBases =
            List.map2 Tuple.pair coords <|
                List.map (baseFromCoord initInt) coords

        ( initCoords, coordSeed ) =
            Random.step initCoordsGenerator initSeed

        ( initTokens, tokenSeed ) =
            Random.step (initTokensGenerator 0) coordSeed

        initCoordTokens =
            List.map2 Tuple.pair initCoords initTokens
    in
    ( List.map
        (\( baseCoord, base ) ->
            { coord = baseCoord
            , base = base
            , content =
                ListX.find (Tuple.first >> (==) baseCoord) initCoordTokens
                    |> Maybe.map Tuple.second
            }
        )
        initBases
    , tokenSeed
    )


coords : List Coord
coords =
    ListX.lift2 Tuple.pair axis axis


axis : List Int
axis =
    List.range 0 3


baseFromCoord : Int -> Coord -> Base
baseFromCoord initInt ( x, y ) =
    let
        scale =
            0.15
    in
    Simplex.noise2d
        (Simplex.permutationTableFromInt initInt)
        (toFloat x * scale)
        (toFloat y * scale)
        |> (\float ->
                if float < -(1 / 3) then
                    Base1

                else if float < 1 / 3 then
                    Base2

                else
                    Base3
           )


queueGenerator : Int -> Random.Generator (List Content)
queueGenerator score =
    Random.list 3 (contentGenerator score)


initCoordsGenerator : Random.Generator (List Coord)
initCoordsGenerator =
    Random.map (List.take 6) <| Random.List.shuffle coords


initTokensGenerator : Int -> Random.Generator (List Content)
initTokensGenerator score =
    Random.list 6 (contentGenerator score)


contentGenerator : Int -> Random.Generator Content
contentGenerator score =
    Random.map2 Plant (tokenGenerator score) bonusGenerator


tokenGenerator : Int -> Random.Generator Token
tokenGenerator score =
    if score < 20 then
        Random.uniform Standard1 <| List.take 2 nextTokens

    else if score < 50 then
        Random.uniform Standard1 <| List.take 3 nextTokens

    else if score < 80 then
        Random.uniform Standard1 <| List.take 4 nextTokens

    else if score < 100 then
        Random.uniform Standard1 <| List.take 5 nextTokens

    else
        Random.uniform Standard1 nextTokens


nextTokens : List Token
nextTokens =
    [ Standard2, Standard3, Growing1 2, Growing2 3, Growing3 5, Disappearing 3 ]


bonusGenerator : Random.Generator Bonus
bonusGenerator =
    Random.weighted ( 20, Bonus ) [ ( 80, NoBonus ) ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        PlaceTokenOnBoard coord ->
            ( modelAfterPlaceToken model coord, Cmd.none )

        Harvest coord ->
            ( modelAfterHarvest model coord, Cmd.none )

        Undo ->
            ( { model
                | board =
                    { grid = model.undoBoard.grid
                    , queue = model.undoBoard.queue
                    }
                , currentSeed = model.undoSeed
                , undoAllowed = False
                , score = model.undoScore
              }
            , Cmd.none
            )


modelAfterPlaceToken : Model -> Coord -> Model
modelAfterPlaceToken oldModel coord =
    let
        ( nextQueue, nextQueueSeed ) =
            Random.step
                (queueGenerator oldModel.score)
                oldModel.currentSeed

        newGrid =
            placeTokenOnGrid oldModel.board.grid
                (Tuple.first oldModel.board.queue)
                coord

        newQueue =
            case oldModel.board.queue of
                ( _, [ Harvester ] ) ->
                    ( Harvester, nextQueue )

                ( _, x :: xs ) ->
                    ( x, xs )

                _ ->
                    oldModel.board.queue

        newBoard =
            { grid = newGrid, queue = newQueue }
    in
    { oldModel
        | gameState = gameStateAfterPlacement newBoard
        , board = newBoard
        , currentSeed =
            case oldModel.board.queue of
                ( _, [ Harvester ] ) ->
                    nextQueueSeed

                _ ->
                    oldModel.currentSeed
        , undoAllowed = True
        , undoSeed = oldModel.currentSeed
        , undoBoard = oldModel.board
    }


gameStateAfterPlacement : Board -> GameState
gameStateAfterPlacement board =
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


modelAfterHarvest : Model -> Coord -> Model
modelAfterHarvest oldModel coord =
    let
        newBoard =
            { grid =
                clearHarvest oldModel.board.grid
                    (harvestFrom oldModel.board.grid coord |> List.map .coord)
                    |> List.map growCell
            , queue =
                case oldModel.board.queue of
                    ( Harvester, x :: xs ) ->
                        ( x, xs ++ [ Harvester ] )

                    _ ->
                        oldModel.board.queue
            }
    in
    { oldModel
        | board = newBoard
        , score =
            oldModel.score
                + (harvestFrom oldModel.board.grid coord |> scoreHarvest)
        , undoAllowed = True
        , undoSeed = oldModel.currentSeed
        , undoBoard = oldModel.board
        , undoScore = oldModel.score
    }


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
    in
    List.indexedMap (\index _ -> index + bonusCount + 1) cells
        |> List.sum


getBonusFromContent : Content -> Maybe Bonus
getBonusFromContent content =
    case content of
        Plant _ bonus ->
            Just bonus

        Harvester ->
            Nothing


applyUntil : (a -> Bool) -> (a -> a) -> a -> a
applyUntil pred step a =
    if pred a then
        a

    else
        applyUntil pred step (step a)


harvestFrom : Grid -> Coord -> List Cell
harvestFrom grid initCoord =
    { toCheck = Set.singleton initCoord
    , toHarvest = Set.singleton initCoord
    , checked = Set.empty
    }
        |> applyUntil (\acc -> acc.toCheck == Set.empty) (harvestStep grid)
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
                            (Set.diff (Set.fromList matches) harvestInfo.checked)
                , toHarvest =
                    Set.union harvestInfo.toHarvest (Set.fromList matches)
                , checked =
                    Set.union harvestInfo.checked checkedCoordSet
            }


type alias HarvestInfo =
    { toCheck : Set Coord
    , toHarvest : Set Coord
    , checked : Set Coord
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


view : Model -> Html Msg
view model =
    Ui.layout
        [ Ui.padding 15
        , Background.color <| Ui.rgb255 213 196 161
        ]
    <|
        case model.gameState of
            Playing ->
                viewPlaying model

            GameOver ->
                Ui.text <| "Game Over: " ++ String.fromInt model.score


viewPlaying : Model -> Ui.Element Msg
viewPlaying model =
    Ui.column
        [ Ui.spacing 15
        , Ui.centerX
        ]
        [ viewGameInfo model
        , viewQueue model.board.queue
        , viewBoard model.board
        , viewButtons model
        , model.gameState |> Debug.toString |> Ui.text
        ]


viewGameInfo : Model -> Ui.Element Msg
viewGameInfo model =
    Ui.row
        [ Ui.width Ui.fill
        , Ui.spaceEvenly
        , Ui.height <| Ui.px 50
        , Font.family <| [ Font.typeface "Source Code Pro" ]
        ]
        [ viewGameId model.initialInt, viewScore model.score ]


viewGameId : Int -> Ui.Element Msg
viewGameId id =
    Ui.column []
        [ Ui.el [ Font.size 12 ] <| Ui.text "Game Id: "
        , Ui.el [ Font.size 24 ] <| Ui.text <| idFromInt id
        ]


viewScore : Int -> Ui.Element Msg
viewScore score =
    score
        |> String.fromInt
        |> Ui.text
        |> Ui.el [ Font.size 36 ]


viewQueue : ( Content, List Content ) -> Ui.Element Msg
viewQueue ( head, rest ) =
    let
        viewQueueHead =
            Ui.el
                [ Background.color <| Ui.rgb255 235 219 178
                , Ui.height <| Ui.px 100
                , Border.rounded 15
                ]
            <|
                viewQueueCell head

        viewQueueRest =
            List.map viewQueueCell rest
    in
    Ui.row [ Ui.height <| Ui.px 100 ] <| viewQueueHead :: viewQueueRest


viewQueueCell : Content -> Ui.Element Msg
viewQueueCell content =
    Ui.el [ Ui.width <| Ui.px 100, Ui.centerY ] <|
        viewContent content


viewBoard : Board -> Ui.Element Msg
viewBoard board =
    let
        viewRow y =
            getRow y board.grid
                |> List.map (viewCell board)
                |> Ui.row [ Ui.spacing 0 ]
    in
    axis |> List.map viewRow |> Ui.column [ Ui.spacing 0 ]


getRow : Int -> Grid -> List Cell
getRow row grid =
    List.filter (\cell -> Tuple.first cell.coord == row) grid


viewCell : Board -> Cell -> Ui.Element Msg
viewCell board cell =
    Ui.el [ Ui.width <| Ui.px 100, Ui.height <| Ui.px 100 ] <|
        Ui.el
            ([ Border.widthEach { bottom = 10, top = 0, right = 0, left = 0 }
             , cellAttrOnClick board.queue cell
             , cellAttrCorners board.grid cell
             ]
                ++ cellAttrHorizontalAlign board.grid cell
                ++ cellAttrVerticalAlign board.grid cell
                ++ cellAttrColours board.grid cell
            )
        <|
            Ui.el [ Ui.centerX, Ui.centerY ] <|
                case cell.content of
                    Just c ->
                        viewContent c

                    Nothing ->
                        Ui.none


cellAttrColours : Grid -> Cell -> List (Ui.Attribute Msg)
cellAttrColours grid cell =
    let
        altCell =
            modBy 2 (Tuple.first cell.coord + Tuple.second cell.coord) == 1

        colourFn =
            if altCell then
                baseColour

            else
                altBaseColour

        borderBase =
            if neighbourBase grid cell.coord Below == Just cell.base then
                cell.base

            else
                nextBase cell.base
    in
    [ Background.color <| colourFn cell.base
    , Border.color <| colourFn borderBase
    ]


cellAttrOnClick : ( Content, List Content ) -> Cell -> Ui.Attribute Msg
cellAttrOnClick ( head, _ ) cell =
    Events.onClick <|
        case ( head, cell.content ) of
            ( Harvester, Just (Plant token _) ) ->
                case token of
                    Growing1 _ ->
                        NoOp

                    Growing2 _ ->
                        NoOp

                    Growing3 _ ->
                        NoOp

                    _ ->
                        Harvest cell.coord

            ( Plant _ _, Nothing ) ->
                PlaceTokenOnBoard cell.coord

            _ ->
                NoOp


cellAttrVerticalAlign : Grid -> Cell -> List (Ui.Attribute Msg)
cellAttrVerticalAlign grid cell =
    case ( flushTo grid cell Above, flushTo grid cell Below ) of
        ( True, True ) ->
            [ Ui.height <| Ui.px 100 ]

        ( True, False ) ->
            [ Ui.height <| Ui.px 98, Ui.alignTop ]

        ( False, True ) ->
            [ Ui.height <| Ui.px 98, Ui.alignBottom ]

        ( False, False ) ->
            [ Ui.height <| Ui.px 96, Ui.centerY ]


cellAttrHorizontalAlign : Grid -> Cell -> List (Ui.Attribute Msg)
cellAttrHorizontalAlign grid cell =
    case ( flushTo grid cell Right, flushTo grid cell Left ) of
        ( True, True ) ->
            [ Ui.width <| Ui.px 100 ]

        ( True, False ) ->
            [ Ui.width <| Ui.px 98, Ui.alignRight ]

        ( False, True ) ->
            [ Ui.width <| Ui.px 98, Ui.alignLeft ]

        ( False, False ) ->
            [ Ui.width <| Ui.px 96, Ui.centerX ]


flushTo : Grid -> Cell -> Direction -> Bool
flushTo grid cell direction =
    (neighbourBase grid cell.coord direction == Just cell.base)
        || (neighbourBase grid cell.coord direction == Nothing)


cellAttrCorners : Grid -> Cell -> Ui.Attribute Msg
cellAttrCorners grid cell =
    let
        round match =
            if match == True then
                0

            else
                8
    in
    Border.roundEach
        { topRight = cornerBaseMatch grid cell [ Above, Right ] |> round
        , bottomRight = cornerBaseMatch grid cell [ Right, Below ] |> round
        , bottomLeft = cornerBaseMatch grid cell [ Below, Left ] |> round
        , topLeft = cornerBaseMatch grid cell [ Left, Above ] |> round
        }


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


baseColour : Base -> Ui.Color
baseColour base =
    case base of
        Base1 ->
            Ui.rgb255 215 153 33

        Base2 ->
            Ui.rgb255 152 151 26

        Base3 ->
            Ui.rgb255 69 133 136


altBaseColour : Base -> Ui.Color
altBaseColour base =
    case base of
        Base1 ->
            Ui.rgb255 207 149 32

        Base2 ->
            Ui.rgb255 144 144 24

        Base3 ->
            Ui.rgb255 65 126 126


nextBase : Base -> Base
nextBase base =
    case base of
        Base1 ->
            Base2

        Base2 ->
            Base3

        Base3 ->
            Base1


viewContent : Content -> Ui.Element Msg
viewContent content =
    case content of
        Plant token bonus ->
            viewPlant token bonus

        Harvester ->
            Ui.image sharedAttributes
                { src = "images/spade.png", description = "Spade" }


viewPlant : Token -> Bonus -> Ui.Element Msg
viewPlant token bonus =
    let
        tokenAttributes =
            [ Ui.inFront <| viewBonus bonus
            , Ui.inFront <| viewTokenCount token
            ]
                ++ sharedAttributes
    in
    Ui.image tokenAttributes <| tokenImageDetails token


viewBonus : Bonus -> Ui.Element Msg
viewBonus bonus =
    if bonus == Bonus then
        Ui.image
            [ Ui.width <| Ui.px 15
            , Ui.height <| Ui.px 15
            , Ui.moveUp 10
            ]
            { src = "images/bee.png", description = "Bee" }

    else
        Ui.none


viewTokenCount : Token -> Ui.Element Msg
viewTokenCount token =
    let
        ( bgColour, fontColour ) =
            case token of
                Disappearing _ ->
                    ( Ui.rgba255 0 0 0 0.8, Ui.rgb255 235 219 178 )

                _ ->
                    ( Ui.rgba255 235 219 178 0.8, Ui.rgb255 0 0 0 )

        outerAtts =
            [ Ui.width <| Ui.px 25
            , Ui.height <| Ui.px 25
            , Background.color bgColour
            , Border.rounded 10
            , Ui.alignBottom
            , Ui.moveDown 5
            , Ui.moveRight 5
            , Ui.alignRight
            ]

        innerAtts =
            [ Ui.centerX, Ui.centerY, Font.size 16, Font.color fontColour ]
    in
    case getTokenCount token of
        Just count ->
            Ui.el outerAtts <| Ui.el innerAtts <| Ui.text <| String.fromInt count

        Nothing ->
            Ui.none


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


tokenImageDetails : Token -> { src : String, description : String }
tokenImageDetails token =
    let
        imageName =
            tokenDetails token |> .image

        description =
            tokenDetails token |> .name
    in
    { src = "images/" ++ imageName ++ ".png", description = description }


tokenDetails : Token -> { image : String, name : String }
tokenDetails token =
    case token of
        Standard1 ->
            { image = "high-grass", name = "Grass" }

        Standard2 ->
            { image = "daisy", name = "Daisy" }

        Standard3 ->
            { image = "spoted-flower", name = "Spotted flower" }

        Growing1 _ ->
            { image = "sesame", name = "Seeds" }

        Growing2 _ ->
            { image = "bud", name = "Bud" }

        Growing3 _ ->
            { image = "bulb", name = "Bulb" }

        Grown1 ->
            { image = "sunflower", name = "Sunflower" }

        Grown2 ->
            { image = "rose", name = "Rose" }

        Grown3 ->
            { image = "viola", name = "Viola" }

        Disappearing _ ->
            { image = "dandelion-flower", name = "Dandelion" }


sharedAttributes : List (Ui.Attribute Msg)
sharedAttributes =
    [ Ui.width <| Ui.px 55
    , Ui.height <| Ui.px 55
    , Ui.centerX
    ]


viewButtons : Model -> Ui.Element Msg
viewButtons model =
    Ui.row [ Ui.height <| Ui.px 50 ] <|
        [ if model.undoAllowed then
            Input.button
                [ Background.color <| Ui.rgb255 168 153 132
                , Ui.height <| Ui.px 50
                , Ui.width <| Ui.px 100
                , Ui.padding 10
                , Border.rounded 5
                ]
                { onPress = Just Undo
                , label = Ui.el [ Ui.centerX, Ui.centerY ] <| Ui.text "Undo"
                }

          else
            Ui.none
        ]


idFromInt : Int -> String
idFromInt int =
    int
        |> String.fromInt
        |> String.padLeft 12 '0'
        |> String.toList
        |> ListX.greedyGroupsOf 4
        |> List.map String.fromList
        |> String.join "-"


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
