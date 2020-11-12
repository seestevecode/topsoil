module Main exposing (main)

import Browser
import Element as Ui
import Element.Background as Background
import Element.Border as Border
import Html exposing (Html)
import List.Extra as ListX
import Random
import Random.List
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
    , queue : List Token
    }


type alias Board =
    List Cell


type alias Cell =
    { coord : ( Int, Int )
    , base : Base
    , content : Maybe Token
    }


type Base
    = Base1
    | Base2
    | Base3


type Token
    = Standard SType Bonus
    | Counter CrType Int Bonus
    | Counted CdType Bonus
    | Disappearing Int Bonus


type SType
    = Standard1
    | Standard2
    | Standard3


type CrType
    = Counter1
    | Counter2
    | Counter3


type CdType
    = Counted1
    | Counted2
    | Counted3


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


init : Int -> ( Model, Cmd Msg )
init intFromDate =
    let
        ( board, seed ) =
            initBoard intFromDate
    in
    ( { initialInt = intFromDate
      , currentSeed = seed
      , board = board
      }
    , Cmd.none
    )


coords : List ( Int, Int )
coords =
    ListX.lift2 Tuple.pair (List.range 0 3) (List.range 0 3)


initBoard : Int -> ( Board, Random.Seed )
initBoard initInt =
    let
        initSeed =
            Random.initialSeed initInt

        initBases =
            List.map2 Tuple.pair coords <|
                List.map (baseFromCoord initInt) coords

        ( initCoords, coordSeed ) =
            Random.step initBoardCoordListGenerator initSeed

        ( initTokens, nextSeed ) =
            Random.step initBoardTokenListGenerator coordSeed

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
    , nextSeed
    )


baseFromCoord : Int -> ( Int, Int ) -> Base
baseFromCoord initInt ( x, y ) =
    Simplex.noise2d
        (Simplex.permutationTableFromInt initInt)
        (toFloat x)
        (toFloat y)
        |> (\float ->
                if float < -(1 / 3) then
                    Base1

                else if float < 1 / 3 then
                    Base2

                else
                    Base3
           )


initBoardCoordListGenerator : Random.Generator (List ( Int, Int ))
initBoardCoordListGenerator =
    Random.map (List.take 6) <| Random.List.shuffle coords


initBoardTokenListGenerator : Random.Generator (List Token)
initBoardTokenListGenerator =
    Random.list 6 <|
        Random.map2 Standard standardGenerator bonusGenerator


standardGenerator : Random.Generator SType
standardGenerator =
    Random.uniform Standard1 [ Standard2, Standard3 ]


bonusGenerator : Random.Generator Bonus
bonusGenerator =
    Random.uniform Bonus [ NoBonus ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Ui.layout [ Ui.padding 15 ] <|
        Ui.column [ Ui.spacing 15 ] [ viewBoard model, viewDebug model ]


viewBoard : Model -> Ui.Element Msg
viewBoard model =
    let
        viewRow y =
            getRow y model.board
                |> List.map (viewCell model.board)
                |> Ui.row [ Ui.spacing 0 ]
    in
    List.range 0 3
        |> List.map viewRow
        |> Ui.column [ Ui.spacing 0 ]


getRow : Int -> Board -> List Cell
getRow row board =
    List.filter (\cell -> Tuple.first cell.coord == row) board


viewCell : Board -> Cell -> Ui.Element Msg
viewCell board cell =
    Ui.el
        [ Background.color <| baseColour cell.base
        , Ui.width <| Ui.px 100
        , Ui.height <| Ui.px 100
        , Border.widthEach { bottom = 10, top = 0, right = 0, left = 0 }
        , roundedCorners board cell
        , Border.color <|
            baseColour <|
                if neighbourBase board cell.coord Below == Just cell.base then
                    cell.base

                else
                    nextBase cell.base
        ]
    <|
        Ui.el [ Ui.centerX, Ui.centerY ] <|
            viewContent cell


roundedCorners : Board -> Cell -> Ui.Attribute Msg
roundedCorners board cell =
    let
        round match =
            if match == True then
                0

            else
                15
    in
    Border.roundEach
        { topRight = cornerBaseMatch board cell [ Above, Right ] |> round
        , bottomRight = cornerBaseMatch board cell [ Right, Below ] |> round
        , bottomLeft = cornerBaseMatch board cell [ Below, Left ] |> round
        , topLeft = cornerBaseMatch board cell [ Left, Above ] |> round
        }


cornerBaseMatch : Board -> Cell -> List Direction -> Bool
cornerBaseMatch board cell directions =
    let
        neighbourBases : List Base
        neighbourBases =
            List.map (neighbourBase board cell.coord) directions
                |> List.filterMap identity
    in
    List.member cell.base neighbourBases


neighbourBase : Board -> ( Int, Int ) -> Direction -> Maybe Base
neighbourBase board coord direction =
    Maybe.map .base <| neighbour board coord direction


neighbour : Board -> ( Int, Int ) -> Direction -> Maybe Cell
neighbour board ( x, y ) direction =
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
    getCell board neighbourCoord


getCell : Board -> ( Int, Int ) -> Maybe Cell
getCell board coord =
    List.filter (\cell -> cell.coord == coord) board |> List.head


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


viewContent : Cell -> Ui.Element Msg
viewContent cell =
    case cell.content of
        Just (Standard sType bonus) ->
            viewStandard sType ++ viewBonus bonus |> Ui.text

        Just (Counter crType count bonus) ->
            viewCounter crType count ++ viewBonus bonus |> Ui.text

        Just (Counted cdType bonus) ->
            viewCounted cdType ++ viewBonus bonus |> Ui.text

        Just (Disappearing count bonus) ->
            viewDisappearing count ++ viewBonus bonus |> Ui.text

        Nothing ->
            "_" |> Ui.text


viewStandard : SType -> String
viewStandard standard =
    case standard of
        Standard1 ->
            "S1"

        Standard2 ->
            "S2"

        Standard3 ->
            "S3"


viewCounter : CrType -> Int -> String
viewCounter counter count =
    let
        countString =
            "." ++ String.fromInt count
    in
    case counter of
        Counter1 ->
            "Cr1" ++ countString

        Counter2 ->
            "Cr2" ++ countString

        Counter3 ->
            "Cr3" ++ countString


viewCounted : CdType -> String
viewCounted counted =
    case counted of
        Counted1 ->
            "Cd1"

        Counted2 ->
            "Cd2"

        Counted3 ->
            "Cd3"


viewDisappearing : Int -> String
viewDisappearing count =
    "D." ++ String.fromInt count


viewBonus : Bonus -> String
viewBonus bonus =
    case bonus of
        Bonus ->
            "+"

        NoBonus ->
            ""


viewDebug : Model -> Ui.Element Msg
viewDebug model =
    Ui.column []
        [ "Initial Int: " ++ Debug.toString model.initialInt |> Ui.text
        , "Current Seed: " ++ Debug.toString model.currentSeed |> Ui.text
        , "Queue: " ++ Debug.toString model.queue |> Ui.text
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none