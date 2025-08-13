module Main exposing (main)

import Browser
import Domain.Constants as Const
import Domain.Types exposing (..)
import Element as Ui
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Game.Board as Board
import Game.Gen as Gen
import Html exposing (Html)
import List.Extra as ListX
import Random
import UI.Colours as Colours


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
            Random.step (Gen.queueGenerator 0) gridSeed

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
            List.map2 Tuple.pair Const.coords <|
                List.map (Board.getBaseFromCoord initInt) Const.coords

        ( initCoords, coordSeed ) =
            Random.step Gen.initCoordsGenerator initSeed

        ( initTokens, tokenSeed ) =
            Random.step (Gen.initTokensGenerator 0) coordSeed

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
                (Gen.queueGenerator oldModel.score)
                oldModel.currentSeed

        newGrid =
            Board.placeTokenOnGrid oldModel.board.grid
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
        | gameState = Board.advanceGameStateAfterPlacement newBoard
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


modelAfterHarvest : Model -> Coord -> Model
modelAfterHarvest oldModel coord =
    let
        newBoard =
            { grid =
                Board.clearHarvest oldModel.board.grid
                    (Board.harvestFrom oldModel.board.grid coord |> List.map .coord)
                    |> List.map Board.growCell
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
                + (Board.harvestFrom oldModel.board.grid coord |> Board.scoreHarvest)
        , undoAllowed = True
        , undoSeed = oldModel.currentSeed
        , undoBoard = oldModel.board
        , undoScore = oldModel.score
    }


view : Model -> Html Msg
view model =
    Ui.layout [ Ui.padding 25, Background.color Colours.mainBackground ] <|
        Ui.column
            [ Ui.width <| Ui.px 400
            , Ui.height Ui.fill
            , Ui.spaceEvenly
            , Ui.centerX
            ]
            [ viewHeader model, viewBody model, viewFooter model ]


viewHeader : Model -> Ui.Element Msg
viewHeader model =
    Ui.row [ Ui.width Ui.fill, Ui.height <| Ui.px 50, Ui.spaceEvenly ] <|
        case model.gameState of
            Playing ->
                [ Ui.el
                    [ Ui.below <|
                        Ui.el [ Ui.moveDown 10, Font.size 15 ] <|
                            viewGameId model.initialInt
                    ]
                  <|
                    viewTitle
                , Ui.el [ Font.size 48, Font.bold, Ui.moveDown 15 ] <|
                    viewScore model.score
                ]

            GameOver ->
                [ viewTitle ]


viewBody : Model -> Ui.Element Msg
viewBody model =
    Ui.column [ Ui.width Ui.fill, Ui.spacing 10 ]
        [ viewQueue model.board.queue
        , Ui.el
            [ Ui.inFront <|
                case model.gameState of
                    Playing ->
                        Ui.none

                    GameOver ->
                        viewGameOverOverlay model
            ]
          <|
            viewGrid model.board
        ]


viewGameOverOverlay : Model -> Ui.Element Msg
viewGameOverOverlay model =
    Ui.el
        [ Background.color Colours.endGameOverlay
        , Ui.width Ui.fill
        , Ui.height Ui.fill
        ]
    <|
        Ui.column [ Ui.centerX, Ui.centerY, Ui.spacing 25 ]
            [ Ui.el [ Ui.centerX, Font.size 30, Font.bold ] <|
                Ui.text "Game Over"
            , Ui.el [ Ui.centerX, Font.size 20 ] <| viewGameId model.initialInt
            , Ui.el [ Ui.centerX, Font.size 60, Font.bold ] <|
                viewScore model.score
            ]


viewFooter : Model -> Ui.Element Msg
viewFooter model =
    Ui.row [ Ui.width Ui.fill, Ui.height <| Ui.px 50 ]
        [ case model.gameState of
            Playing ->
                viewUndoButton model.undoAllowed

            GameOver ->
                Ui.el [ Ui.width <| Ui.px 100, Ui.height Ui.fill ] <| Ui.none
        , Ui.column
            [ Ui.width <| Ui.px 200, Ui.height Ui.fill, Ui.spaceEvenly ]
            [ Ui.paragraph [ Font.center, Font.size 15, Ui.centerY ]
                [ Ui.text "seestevecode", Ui.text " - ", Ui.text "source" ]
            ]
        , viewMenuButton
        ]


viewTitle : Ui.Element Msg
viewTitle =
    Ui.el [ Font.bold, Font.size 24 ] <| Ui.text "Topsoil"


viewMenuButton : Ui.Element Msg
viewMenuButton =
    Input.button
        [ Background.color Colours.buttonBackground
        , Ui.height <| Ui.px 50
        , Ui.width <| Ui.px 100
        , Ui.padding 10
        , Border.rounded 5
        ]
        { onPress = Just NoOp
        , label = Ui.el [ Ui.centerX, Ui.centerY ] <| Ui.text "Menu"
        }


viewScore : Int -> Ui.Element Msg
viewScore score =
    Ui.text <| String.fromInt score


viewGameId : Int -> Ui.Element Msg
viewGameId id =
    Ui.text <| idFromInt id


viewQueue : ( Content, List Content ) -> Ui.Element Msg
viewQueue ( head, rest ) =
    let
        viewQueueHead =
            Ui.el
                [ Background.color Colours.queueHeadBackground
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
    Ui.el [ Ui.width <| Ui.px 100, Ui.centerY ] <| viewContent content


viewGrid : Board -> Ui.Element Msg
viewGrid board =
    let
        viewRow y =
            Board.getRow y board.grid
                |> List.map (viewCell board)
                |> Ui.row [ Ui.spacing 0 ]
    in
    Const.axis |> List.map viewRow |> Ui.column [ Ui.spacing 0 ]


viewCell : Board -> Cell -> Ui.Element Msg
viewCell board cell =
    Ui.el [ Ui.width <| Ui.px 100, Ui.height <| Ui.px 100 ] <|
        Ui.el
            ([ Border.widthEach { bottom = 10, top = 0, right = 0, left = 0 }
             , cellOnClickAtts board.queue cell
             , cellCornerAtts board.grid cell
             ]
                ++ cellHorizontalAtts board.grid cell
                ++ cellVerticalAtts board.grid cell
                ++ cellColourAtts board.grid cell
            )
        <|
            Ui.el [ Ui.centerX, Ui.centerY ] <|
                case cell.content of
                    Just c ->
                        viewContent c

                    Nothing ->
                        Ui.none


cellColourAtts : Grid -> Cell -> List (Ui.Attribute Msg)
cellColourAtts grid cell =
    let
        colourFn =
            if
                modBy 2 (Tuple.first cell.coord + Tuple.second cell.coord)
                    == 1
            then
                baseColour

            else
                altBaseColour

        borderBase =
            if Board.neighbourBase grid cell.coord Below == Just cell.base then
                cell.base

            else
                Board.nextBase cell.base
    in
    [ Background.color <| colourFn cell.base
    , Border.color <| colourFn borderBase
    ]


cellOnClickAtts : ( Content, List Content ) -> Cell -> Ui.Attribute Msg
cellOnClickAtts ( head, _ ) cell =
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


cellVerticalAtts : Grid -> Cell -> List (Ui.Attribute Msg)
cellVerticalAtts grid cell =
    case ( Board.flushTo grid cell Above, Board.flushTo grid cell Below ) of
        ( True, True ) ->
            [ Ui.height <| Ui.px 100 ]

        ( True, False ) ->
            [ Ui.height <| Ui.px 98, Ui.alignTop ]

        ( False, True ) ->
            [ Ui.height <| Ui.px 98, Ui.alignBottom ]

        ( False, False ) ->
            [ Ui.height <| Ui.px 96, Ui.centerY ]


cellHorizontalAtts : Grid -> Cell -> List (Ui.Attribute Msg)
cellHorizontalAtts grid cell =
    case ( Board.flushTo grid cell Right, Board.flushTo grid cell Left ) of
        ( True, True ) ->
            [ Ui.width <| Ui.px 100 ]

        ( True, False ) ->
            [ Ui.width <| Ui.px 98, Ui.alignRight ]

        ( False, True ) ->
            [ Ui.width <| Ui.px 98, Ui.alignLeft ]

        ( False, False ) ->
            [ Ui.width <| Ui.px 96, Ui.centerX ]


cellCornerAtts : Grid -> Cell -> Ui.Attribute Msg
cellCornerAtts grid cell =
    let
        round match =
            if match == True then
                0

            else
                8
    in
    Border.roundEach
        { topRight = Board.cornerBaseMatch grid cell [ Above, Right ] |> round
        , bottomRight = Board.cornerBaseMatch grid cell [ Right, Below ] |> round
        , bottomLeft = Board.cornerBaseMatch grid cell [ Below, Left ] |> round
        , topLeft = Board.cornerBaseMatch grid cell [ Left, Above ] |> round
        }


baseColour : Base -> Ui.Color
baseColour base =
    case base of
        Base1 ->
            Colours.mainBgBase1

        Base2 ->
            Colours.mainBgBase2

        Base3 ->
            Colours.mainBgBase3


altBaseColour : Base -> Ui.Color
altBaseColour base =
    case base of
        Base1 ->
            Colours.altBgBase1

        Base2 ->
            Colours.altBgBase2

        Base3 ->
            Colours.altBgBase3


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
                    ( Colours.tokenDisappearingBg, Colours.tokenDisappearingFont )

                _ ->
                    ( Colours.tokenMainBg, Colours.tokenMainFont )

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
    case Board.getTokenCount token of
        Just count ->
            Ui.el outerAtts <|
                Ui.el innerAtts <|
                    Ui.text <|
                        String.fromInt count

        Nothing ->
            Ui.none


tokenImageDetails : Token -> { src : String, description : String }
tokenImageDetails token =
    let
        imageName =
            Const.tokenDetails token |> .image

        description =
            Const.tokenDetails token |> .name
    in
    { src = "images/" ++ imageName ++ ".png", description = description }


sharedAttributes : List (Ui.Attribute Msg)
sharedAttributes =
    [ Ui.width <| Ui.px 55
    , Ui.height <| Ui.px 55
    , Ui.centerX
    ]


viewUndoButton : Bool -> Ui.Element Msg
viewUndoButton undoAllowed =
    if undoAllowed then
        Input.button
            [ Background.color <| Colours.buttonBackground
            , Ui.height <| Ui.px 50
            , Ui.width <| Ui.px 100
            , Ui.padding 10
            , Border.rounded 5
            ]
            { onPress = Just Undo
            , label = Ui.el [ Ui.centerX, Ui.centerY ] <| Ui.text "Undo"
            }

    else
        Ui.el [ Ui.width <| Ui.px 100 ] <| Ui.none


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
