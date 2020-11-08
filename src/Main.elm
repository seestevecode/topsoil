module Main exposing (main)

import Browser
import Element as Ui
import Element.Background as Background
import Element.Border as Border
import Html exposing (Html)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { initialInt : Int
    , board : Board
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


init : () -> ( Model, Cmd Msg )
init _ =
    ( { initialInt = 12345
      , board =
            [ Cell ( 0, 0 ) Base3 <| Just <| Standard Standard1 Bonus
            , Cell ( 0, 1 ) Base2 <| Just <| Standard Standard2 Bonus
            , Cell ( 0, 2 ) Base1 <| Just <| Standard Standard3 Bonus
            , Cell ( 0, 3 ) Base2 <| Just <| Counter Counter1 4 Bonus
            , Cell ( 1, 0 ) Base2 <| Just <| Counter Counter2 4 Bonus
            , Cell ( 1, 1 ) Base1 <| Just <| Counter Counter3 4 Bonus
            , Cell ( 1, 2 ) Base1 <| Just <| Counted Counted1 Bonus
            , Cell ( 1, 3 ) Base3 <| Just <| Counted Counted2 Bonus
            , Cell ( 2, 0 ) Base3 <| Just <| Counted Counted3 Bonus
            , Cell ( 2, 1 ) Base2 <| Just <| Disappearing 4 Bonus
            , Cell ( 2, 2 ) Base3 <| Just <| Standard Standard1 NoBonus
            , Cell ( 2, 3 ) Base1 <| Just <| Counter Counter1 4 NoBonus
            , Cell ( 3, 0 ) Base1 <| Just <| Counted Counted2 NoBonus
            , Cell ( 3, 1 ) Base3 <| Just <| Disappearing 4 NoBonus
            , Cell ( 3, 2 ) Base3 <| Nothing
            , Cell ( 3, 3 ) Base2 <| Nothing
            ]
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Ui.layout [ Ui.padding 15 ] <|
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


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
