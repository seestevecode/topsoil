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
    Cell


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


type Msg
    = NoOp


init : () -> ( Model, Cmd Msg )
init _ =
    ( Cell ( 0, 0 ) Base3 (Just (Standard Standard1 Bonus)), Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Ui.layout [ Ui.padding 15 ] <|
        Ui.el
            [ Background.color <| baseColour model.base
            , Ui.width <| Ui.px 100
            , Ui.height <| Ui.px 100
            , Border.widthEach { bottom = 10, top = 0, right = 0, left = 0 }
            , Border.rounded 15
            , Border.color <| baseColour <| nextBase model.base
            ]
        <|
            Ui.el [ Ui.centerX, Ui.centerY ] <|
                viewContent model


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
