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
    , queue : List Content
    }


type alias Board =
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


type alias Content =
    { token : Token
    , bonus : Bonus
    }


type Token
    = Standard StandardToken
    | Growing GrowingToken Int
    | Grown GrownToken
    | DisappearingToken Int
    | Harvester


type StandardToken
    = Standard1
    | Standard2
    | Standard3


type GrowingToken
    = Growing1
    | Growing2
    | Growing3


type GrownToken
    = Grown1
    | Grown2
    | Grown3


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
        ( board, boardSeed ) =
            initBoard intFromDate

        ( queue, nextSeed ) =
            Random.step queueGenerator boardSeed

        harvester =
            { token = Harvester, bonus = NoBonus }
    in
    ( { initialInt = intFromDate
      , currentSeed = nextSeed
      , board = board
      , queue = queue ++ List.singleton harvester
      }
    , Cmd.none
    )


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


coords : List Coord
coords =
    ListX.lift2 Tuple.pair axis axis


axis : List Int
axis =
    List.range 0 3


baseFromCoord : Int -> Coord -> Base
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


queueGenerator : Random.Generator (List Content)
queueGenerator =
    Random.list 3 <|
        Random.map2 contentFromStandardToken standardGenerator bonusGenerator


initBoardCoordListGenerator : Random.Generator (List Coord)
initBoardCoordListGenerator =
    Random.map (List.take 6) <| Random.List.shuffle coords


initBoardTokenListGenerator : Random.Generator (List Content)
initBoardTokenListGenerator =
    Random.list 6 <|
        Random.map2 contentFromStandardToken standardGenerator bonusGenerator


contentFromStandardToken : StandardToken -> Bonus -> Content
contentFromStandardToken token bonus =
    { token = Standard token, bonus = bonus }


standardGenerator : Random.Generator StandardToken
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
        Ui.column [ Ui.spacing 15 ]
            [ viewQueue model.queue
            , viewBoard model.board
            , viewDebug model
            ]


viewQueue : List Content -> Ui.Element Msg
viewQueue queue =
    queue
        |> List.map viewQueueCell
        |> Ui.row
            [ Ui.spacing 0
            , Background.color <| Ui.rgb255 200 200 200
            , Border.rounded 15
            ]


viewQueueCell : Content -> Ui.Element Msg
viewQueueCell content =
    Ui.el
        [ Ui.width <| Ui.px 100
        , Ui.height <| Ui.px 100
        ]
    <|
        Ui.el [ Ui.centerX, Ui.centerY ] <|
            viewContent content


viewBoard : Board -> Ui.Element Msg
viewBoard board =
    let
        viewRow y =
            getRow y board
                |> List.map (viewCell board)
                |> Ui.row [ Ui.spacing 0 ]
    in
    axis |> List.map viewRow |> Ui.column [ Ui.spacing 0 ]


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
            case cell.content of
                Just c ->
                    viewContent c

                Nothing ->
                    Ui.none


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
        neighbourBases =
            List.map (neighbourBase board cell.coord) directions
                |> List.filterMap identity
    in
    List.member cell.base neighbourBases


neighbourBase : Board -> Coord -> Direction -> Maybe Base
neighbourBase board coord direction =
    Maybe.map .base <| neighbour board coord direction


neighbour : Board -> Coord -> Direction -> Maybe Cell
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


getCell : Board -> Coord -> Maybe Cell
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


viewContent : Content -> Ui.Element Msg
viewContent { token, bonus } =
    case token of
        Standard standardToken ->
            viewStandard standardToken ++ viewBonus bonus |> Ui.text

        Growing growingType count ->
            viewGrowing growingType count ++ viewBonus bonus |> Ui.text

        Grown grownType ->
            viewGrown grownType ++ viewBonus bonus |> Ui.text

        DisappearingToken count ->
            viewDisappearing count ++ viewBonus bonus |> Ui.text

        Harvester ->
            "H" |> Ui.text


viewStandard : StandardToken -> String
viewStandard standard =
    case standard of
        Standard1 ->
            "S1"

        Standard2 ->
            "S2"

        Standard3 ->
            "S3"


viewGrowing : GrowingToken -> Int -> String
viewGrowing growing count =
    let
        countString =
            "." ++ String.fromInt count
    in
    case growing of
        Growing1 ->
            "g1" ++ countString

        Growing2 ->
            "g2" ++ countString

        Growing3 ->
            "g3" ++ countString


viewGrown : GrownToken -> String
viewGrown grown =
    case grown of
        Grown1 ->
            "G1"

        Grown2 ->
            "G2"

        Grown3 ->
            "G3"


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
        , "Next in queue: " ++ Debug.toString (List.head model.queue) |> Ui.text
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
