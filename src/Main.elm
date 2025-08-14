module Main exposing (main)

import App.Messages as M
import Browser
import Domain.Constants as Const
import Domain.Types exposing (..)
import Game.Board as Board
import Game.Gen as Gen
import Html exposing (Html)
import List.Extra as ListX
import Random
import UI.View as View


main : Program Int Model M.Msg
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


init : Int -> ( Model, Cmd M.Msg )
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


update : M.Msg -> Model -> ( Model, Cmd M.Msg )
update msg model =
    case msg of
        M.NoOp ->
            ( model, Cmd.none )

        M.PlaceTokenOnBoard coord ->
            ( updateModelAfterPlacingToken model coord, Cmd.none )

        M.Harvest coord ->
            ( updateModelAfterHarvest model coord, Cmd.none )

        M.Undo ->
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


updateModelAfterPlacingToken : Model -> Coord -> Model
updateModelAfterPlacingToken oldModel coord =
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


updateModelAfterHarvest : Model -> Coord -> Model
updateModelAfterHarvest oldModel coord =
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


view : Model -> Html M.Msg
view model =
    View.viewLayout model


subscriptions : Model -> Sub M.Msg
subscriptions _ =
    Sub.none
