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
import Set
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
    , board : Board
    , undoAllowed : Bool
    , undoBoard : Board
    , undoSeed : Random.Seed
    , debug : String
    }


type alias Board =
    { grid : Grid
    , queue : List Content
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
            Random.step queueGenerator gridSeed

        newBoard =
            { grid = grid, queue = queue ++ List.singleton Harvester }
    in
    ( { initialInt = intFromDate
      , currentSeed = queueSeed
      , board = newBoard
      , undoAllowed = False
      , undoBoard = { grid = [], queue = [] }
      , undoSeed = queueSeed
      , debug = ""
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
            Random.step initTokensGenerator coordSeed

        initCoordToken =
            List.map2 Tuple.pair initCoords initTokens
    in
    ( List.map
        (\( baseCoord, base ) ->
            { coord = baseCoord
            , base = base
            , content =
                ListX.find
                    (\( tokenCoord, _ ) -> baseCoord == tokenCoord)
                    initCoordToken
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


queueGenerator : Random.Generator (List Content)
queueGenerator =
    Random.list 3 <| Random.map2 Plant standardGenerator bonusGenerator


initCoordsGenerator : Random.Generator (List Coord)
initCoordsGenerator =
    Random.map (List.take 6) <| Random.List.shuffle coords


initTokensGenerator : Random.Generator (List Content)
initTokensGenerator =
    Random.list 6 <| Random.map2 Plant standardGenerator bonusGenerator


standardGenerator : Random.Generator Token
standardGenerator =
    Random.uniform Standard1 [ Standard2, Standard3 ]


bonusGenerator : Random.Generator Bonus
bonusGenerator =
    Random.weighted ( 20, Bonus ) [ ( 80, NoBonus ) ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        PlaceTokenOnBoard coord ->
            let
                ( newQueue, newQueueSeed ) =
                    Random.step queueGenerator model.currentSeed
            in
            ( { model
                | board =
                    { grid =
                        placeTokenOnGrid model.board.grid
                            (List.head model.board.queue
                                |> Maybe.withDefault Harvester
                            )
                            coord
                    , queue =
                        case model.board.queue of
                            [ _, h ] ->
                                h :: newQueue

                            _ :: qs ->
                                qs

                            _ ->
                                model.board.queue
                    }
                , currentSeed =
                    case model.board.queue of
                        [ _, _ ] ->
                            newQueueSeed

                        _ ->
                            model.currentSeed
                , undoAllowed = True
                , undoBoard =
                    { grid = model.board.grid, queue = model.board.queue }
                , undoSeed = model.currentSeed
              }
            , Cmd.none
            )

        Harvest coord ->
            ( { model
                | board =
                    { grid =
                        clearHarvest model.board.grid
                            (harvestFrom model.board.grid coord
                                |> List.map .coord
                            )
                    , queue =
                        case model.board.queue of
                            Harvester :: rest ->
                                rest ++ [ Harvester ]

                            _ ->
                                model.board.queue
                    }
                , undoBoard = { grid = model.board.grid, queue = model.board.queue }
                , undoSeed = model.currentSeed
              }
            , Cmd.none
            )

        Undo ->
            ( { model
                | board =
                    { grid = model.undoBoard.grid
                    , queue = model.undoBoard.queue
                    }
                , currentSeed = model.undoSeed
                , undoAllowed = False
              }
            , Cmd.none
            )


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
        |> applyUntil
            (\acc -> acc.toCheck == Set.empty)
            (\acc ->
                let
                    checkCoord =
                        Set.toList acc.toCheck
                            |> List.head
                            |> Maybe.withDefault ( -1, -1 )
                in
                case matchedNeighbours grid checkCoord of
                    [] ->
                        { acc
                            | toCheck =
                                Set.diff acc.toCheck (Set.singleton checkCoord)
                            , checked =
                                Set.union acc.checked (Set.singleton checkCoord)
                        }

                    matches ->
                        { acc
                            | toCheck =
                                Set.diff acc.toCheck (Set.singleton checkCoord)
                                    |> Set.union
                                        (Set.diff
                                            (Set.fromList matches)
                                            acc.checked
                                        )
                            , toHarvest =
                                Set.union
                                    acc.toHarvest
                                    (Set.fromList matches)
                            , checked =
                                Set.union
                                    acc.checked
                                    (Set.singleton checkCoord)
                        }
            )
        |> .toHarvest
        |> Set.toList
        |> List.map (getCell grid)
        |> List.filterMap identity


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
                    tokenA == tokenB

                _ ->
                    False
    in
    baseMatch && contentMatch


removeCellContent : Grid -> Coord -> Grid
removeCellContent oldGrid targetCoord =
    List.map
        (\cell ->
            if cell.coord == targetCoord then
                { cell | base = nextBase cell.base, content = Nothing }

            else
                cell
        )
        oldGrid


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
        Ui.row [ Ui.spacing 100, Ui.centerX, Ui.centerY ]
            [ Ui.column
                [ Ui.spacing 15
                , Ui.centerX
                ]
                [ viewGameInfo model.initialInt
                , viewQueue model.board.queue
                , viewBoard model.board
                , viewButtons model
                ]
            , Ui.el [ Ui.width <| Ui.px 600 ] <| viewDebug model
            ]


viewGameInfo : Int -> Ui.Element Msg
viewGameInfo int =
    Ui.row [ Ui.spaceEvenly, Ui.height <| Ui.px 50 ]
        [ Ui.el
            [ Font.family <| [ Font.typeface "Source Code Pro" ]
            , Font.size 12
            , Ui.alignTop
            ]
          <|
            Ui.text <|
                "Game Id: "
                    ++ idFromInt int
        ]


viewQueue : List Content -> Ui.Element Msg
viewQueue queue =
    (queue
        |> List.head
        |> Maybe.withDefault Harvester
        |> viewQueueCell
        |> Ui.el
            [ Background.color <| Ui.rgb255 235 219 178
            , Ui.height <| Ui.px 100
            , Border.rounded 15
            ]
    )
        :: (queue
                |> List.tail
                |> Maybe.withDefault []
                |> List.map viewQueueCell
           )
        |> Ui.row [ Ui.height <| Ui.px 100 ]


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
            ([ Background.color <| baseColour cell.base
             , Border.widthEach { bottom = 10, top = 0, right = 0, left = 0 }
             , roundedCorners board.grid cell
             , Events.onClick <|
                case ( List.head board.queue, cell.content ) of
                    ( Just Harvester, Just _ ) ->
                        Harvest cell.coord

                    ( Just (Plant _ _), Nothing ) ->
                        PlaceTokenOnBoard cell.coord

                    _ ->
                        NoOp
             , Border.color <|
                baseColour <|
                    if
                        neighbourBase board.grid cell.coord Below
                            == Just cell.base
                    then
                        cell.base

                    else
                        nextBase cell.base
             ]
                ++ cellAlignments board.grid cell
            )
        <|
            Ui.el [ Ui.centerX, Ui.centerY ] <|
                case cell.content of
                    Just c ->
                        viewContent c

                    Nothing ->
                        Ui.none


cellAlignments : Grid -> Cell -> List (Ui.Attribute Msg)
cellAlignments grid cell =
    let
        flushTo direction =
            neighbourBase grid cell.coord direction
                == Just cell.base
                || neighbourBase grid cell.coord direction
                == Nothing

        verticalAttrs =
            case ( flushTo Above, flushTo Below ) of
                ( True, True ) ->
                    [ Ui.height <| Ui.px 100 ]

                ( True, False ) ->
                    [ Ui.height <| Ui.px 98, Ui.alignTop ]

                ( False, True ) ->
                    [ Ui.height <| Ui.px 98, Ui.alignBottom ]

                ( False, False ) ->
                    [ Ui.height <| Ui.px 96, Ui.centerY ]

        horizontalAttrs =
            case ( flushTo Right, flushTo Left ) of
                ( True, True ) ->
                    [ Ui.width <| Ui.px 100 ]

                ( True, False ) ->
                    [ Ui.width <| Ui.px 98, Ui.alignRight ]

                ( False, True ) ->
                    [ Ui.width <| Ui.px 98, Ui.alignLeft ]

                ( False, False ) ->
                    [ Ui.width <| Ui.px 96, Ui.centerX ]
    in
    verticalAttrs ++ horizontalAttrs


roundedCorners : Grid -> Cell -> Ui.Attribute Msg
roundedCorners grid cell =
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
        bonusElement =
            if bonus == Bonus then
                Ui.image
                    [ Ui.width <| Ui.px 15
                    , Ui.height <| Ui.px 15
                    , Ui.moveUp 10
                    ]
                    { src = "images/bee.png", description = "Bee" }

            else
                Ui.none

        tokenAttributes =
            Ui.inFront bonusElement :: sharedAttributes
    in
    case token of
        Standard1 ->
            Ui.image tokenAttributes
                { src = "images/high-grass.png", description = "Grass" }

        Standard2 ->
            Ui.image tokenAttributes
                { src = "images/daisy.png", description = "Daisy" }

        Standard3 ->
            Ui.image tokenAttributes
                { src = "images/sunflower.png", description = "Sunflower" }

        Disappearing _ ->
            Ui.image tokenAttributes
                { src = "images/dandelion-flower.png", description = "Dandelion" }

        _ ->
            viewToken token ++ viewBonus bonus |> Ui.text


sharedAttributes : List (Ui.Attribute Msg)
sharedAttributes =
    [ Ui.width <| Ui.px 55
    , Ui.height <| Ui.px 55
    , Ui.centerX
    ]


viewToken : Token -> String
viewToken token =
    case token of
        Standard1 ->
            "S1"

        Standard2 ->
            "S2"

        Standard3 ->
            "S3"

        Growing1 counter ->
            "g1." ++ String.fromInt counter

        Growing2 counter ->
            "g2." ++ String.fromInt counter

        Growing3 counter ->
            "g3." ++ String.fromInt counter

        Grown1 ->
            "G1"

        Grown2 ->
            "G2"

        Grown3 ->
            "G3"

        Disappearing counter ->
            "D." ++ String.fromInt counter


viewBonus : Bonus -> String
viewBonus bonus =
    case bonus of
        Bonus ->
            "+"

        NoBonus ->
            ""


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


viewDebug : Model -> Ui.Element Msg
viewDebug model =
    let
        viewDebugElement string =
            Ui.paragraph [] <| List.singleton <| Ui.text <| string
    in
    Ui.column [ Ui.spacing 20 ] <|
        List.map viewDebugElement
            [ "Current Seed: " ++ Debug.toString model.currentSeed
            , "Queue: " ++ Debug.toString model.board.queue
            , "Test: " ++ Debug.toString (List.head model.board.queue)
            , "Debug: " ++ Debug.toString model.debug
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
