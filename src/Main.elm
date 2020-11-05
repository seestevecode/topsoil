module Main exposing (main)

import Browser
import Element as Ui
import Element.Background as Background
import Element.Border as Border
import Html exposing (Attribute, Html)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
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
    ( [ Cell ( 0, 0 ) Base3 <| Just <| Standard Standard1 Bonus
      , Cell ( 0, 1 ) Base2 <| Nothing
      , Cell ( 0, 2 ) Base1 <| Nothing
      , Cell ( 0, 3 ) Base2 <| Nothing
      , Cell ( 1, 0 ) Base2 <| Just <| Disappearing 4 NoBonus
      , Cell ( 1, 1 ) Base1 <| Nothing
      , Cell ( 1, 2 ) Base1 <| Nothing
      , Cell ( 1, 3 ) Base3 <| Nothing
      , Cell ( 2, 0 ) Base3 <| Nothing
      , Cell ( 2, 1 ) Base2 <| Nothing
      , Cell ( 2, 2 ) Base3 <| Nothing
      , Cell ( 2, 3 ) Base1 <| Nothing
      , Cell ( 3, 0 ) Base1 <| Nothing
      , Cell ( 3, 1 ) Base3 <| Nothing
      , Cell ( 3, 2 ) Base3 <| Nothing
      , Cell ( 3, 3 ) Base2 <| Nothing
      ]
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Ui.layout [ Ui.padding 15 ] <|
        let
            viewRow y =
                getRow y model
                    |> List.map (viewCell model)
                    |> Ui.row [ Ui.spacing 0 ]
        in
        List.range 0 3
            |> List.map viewRow
            |> Ui.column [ Ui.spacing 0 ]


getRow : Int -> Model -> List Cell
getRow row model =
    List.filter (\cell -> Tuple.first cell.coord == row) model


viewCell : Model -> Cell -> Ui.Element Msg
viewCell model cell =
    Ui.el
        [ Background.color <| baseColour cell.base
        , Ui.width <| Ui.px 100
        , Ui.height <| Ui.px 100
        , Border.widthEach { bottom = 10, top = 0, right = 0, left = 0 }
        , roundedCorners model cell
        , Border.color <|
            baseColour <|
                if neighbourBase model cell.coord Below == Just cell.base then
                    cell.base

                else
                    nextBase cell.base
        ]
    <|
        Ui.el [ Ui.centerX, Ui.centerY ] <|
            viewContent cell


roundedCorners : Model -> Cell -> Ui.Attribute Msg
roundedCorners model cell =
    let
        round match =
            case match of
                True ->
                    0

                False ->
                    15
    in
    Border.roundEach
        { topRight = cornerBaseMatch model cell [ Above, Right ] |> round
        , bottomRight = cornerBaseMatch model cell [ Right, Below ] |> round
        , bottomLeft = cornerBaseMatch model cell [ Below, Left ] |> round
        , topLeft = cornerBaseMatch model cell [ Left, Above ] |> round
        }


cornerBaseMatch : Model -> Cell -> List Direction -> Bool
cornerBaseMatch model cell directions =
    let
        neighbourBases : List Base
        neighbourBases =
            List.map (neighbourBase model cell.coord) directions
                |> List.filterMap identity
    in
    List.member cell.base neighbourBases


neighbourBase : Model -> ( Int, Int ) -> Direction -> Maybe Base
neighbourBase model coord direction =
    Maybe.map .base <| neighbour model coord direction


neighbour : Model -> ( Int, Int ) -> Direction -> Maybe Cell
neighbour model ( x, y ) direction =
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
    getCell model neighbourCoord


getCell : Model -> ( Int, Int ) -> Maybe Cell
getCell model coord =
    List.filter (\cell -> cell.coord == coord) model |> List.head


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
