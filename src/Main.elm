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
    , undoOk : Bool
    , undoState :
        { undoBoard : Board
        , undoQueue : List Content
        }
    , debug : String
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
    | Harvester


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
      , undoOk = False
      , undoState = { undoBoard = [], undoQueue = [] }
      , debug = ""
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
            Random.step initCoordsGenerator initSeed

        ( initTokens, nextSeed ) =
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
    Random.list 3 <| Random.map2 Content standardGenerator bonusGenerator


initCoordsGenerator : Random.Generator (List Coord)
initCoordsGenerator =
    Random.map (List.take 6) <| Random.List.shuffle coords


initTokensGenerator : Random.Generator (List Content)
initTokensGenerator =
    Random.list 6 <| Random.map2 Content standardGenerator bonusGenerator


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
            ( { model
                | board =
                    placeTokenOnBoard model.board
                        (List.head model.queue
                            |> Maybe.withDefault
                                { token = Harvester
                                , bonus = NoBonus
                                }
                        )
                        coord
                , queue = List.tail model.queue |> Maybe.withDefault []
                , undoOk = True
                , undoState = { undoBoard = model.board, undoQueue = model.queue }
                , debug = "Placing content..."
              }
            , Cmd.none
            )

        Harvest coord ->
            ( { model | debug = "Harvesting..." }, Cmd.none )

        Undo ->
            ( { model
                | board = model.undoState.undoBoard
                , queue = model.undoState.undoQueue
                , undoOk = False
              }
            , Cmd.none
            )


placeTokenOnBoard : Board -> Content -> Coord -> Board
placeTokenOnBoard oldBoard newContent targetCoord =
    List.map
        (\cell ->
            if cell.coord == targetCoord then
                { coord = cell.coord
                , base = cell.base
                , content = Just newContent
                }

            else
                cell
        )
        oldBoard


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
                [ viewGameInfo model
                , viewQueue model.queue
                , viewBoard model
                , viewButtons model
                ]
            , Ui.el [ Ui.width <| Ui.px 600 ] <| viewDebug model
            ]


viewGameInfo : Model -> Ui.Element Msg
viewGameInfo model =
    Ui.row [ Ui.spaceEvenly, Ui.height <| Ui.px 50 ]
        [ Ui.el
            [ Font.family <| [ Font.typeface "Source Code Pro" ]
            , Font.size 12
            , Ui.alignTop
            ]
          <|
            Ui.text <|
                "Game Id: "
                    ++ idFromInt model.initialInt
        ]


viewQueue : List Content -> Ui.Element Msg
viewQueue queue =
    (queue
        |> List.head
        |> Maybe.withDefault { token = Harvester, bonus = NoBonus }
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


viewBoard : Model -> Ui.Element Msg
viewBoard model =
    let
        viewRow y =
            getRow y model.board
                |> List.map (viewCell model)
                |> Ui.row [ Ui.spacing 0 ]
    in
    axis |> List.map viewRow |> Ui.column [ Ui.spacing 0 ]


getRow : Int -> Board -> List Cell
getRow row board =
    List.filter (\cell -> Tuple.first cell.coord == row) board


viewCell : Model -> Cell -> Ui.Element Msg
viewCell model cell =
    Ui.el [ Ui.width <| Ui.px 100, Ui.height <| Ui.px 100 ] <|
        Ui.el
            ([ Background.color <| baseColour cell.base
             , Border.widthEach { bottom = 10, top = 0, right = 0, left = 0 }
             , roundedCorners model.board cell
             , Events.onClick <|
                if List.length model.queue > 1 && cell.content == Nothing then
                    PlaceTokenOnBoard cell.coord

                else if
                    model.queue
                        == [ { token = Harvester, bonus = NoBonus } ]
                        && cell.content
                        /= Nothing
                then
                    Harvest cell.coord

                else
                    NoOp
             , Border.color <|
                baseColour <|
                    if
                        neighbourBase model.board cell.coord Below
                            == Just cell.base
                    then
                        cell.base

                    else
                        nextBase cell.base
             ]
                ++ cellAlignments model.board cell
            )
        <|
            Ui.el [ Ui.centerX, Ui.centerY ] <|
                case cell.content of
                    Just c ->
                        viewContent c

                    Nothing ->
                        Ui.none


cellAlignments : Board -> Cell -> List (Ui.Attribute Msg)
cellAlignments board cell =
    let
        flushTo direction =
            neighbourBase board cell.coord direction
                == Just cell.base
                || neighbourBase board cell.coord direction
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


roundedCorners : Board -> Cell -> Ui.Attribute Msg
roundedCorners board cell =
    let
        round match =
            if match == True then
                0

            else
                8
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

        sharedAttributes =
            [ Ui.width <| Ui.px 55
            , Ui.height <| Ui.px 55
            , Ui.centerX
            , Ui.inFront bonusElement
            ]
    in
    case token of
        Standard1 ->
            Ui.image sharedAttributes
                { src = "images/high-grass.png", description = "Grass" }

        Standard2 ->
            Ui.image sharedAttributes
                { src = "images/daisy.png", description = "Daisy" }

        Standard3 ->
            Ui.image sharedAttributes
                { src = "images/sunflower.png", description = "Sunflower" }

        Disappearing _ ->
            Ui.image sharedAttributes
                { src = "images/dandelion-flower.png", description = "Dandelion" }

        Harvester ->
            Ui.image sharedAttributes
                { src = "images/spade.png", description = "Spade" }

        _ ->
            viewToken token ++ viewBonus bonus |> Ui.text


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

        Harvester ->
            "H"


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
        [ if model.undoOk then
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
