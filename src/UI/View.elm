module UI.View exposing (..)

import App.Messages as M
import Domain.Constants as Const
import Domain.Types exposing (..)
import Element as Ui
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Game.Board as Board
import Html exposing (Html)
import List.Extra as ListX
import UI.Colours as Colours



-- have actions


viewLayout : { a | undoAllowed : Bool, board : Board, gameState : GameState, initialInt : Int, score : Int } -> Html M.Msg
viewLayout props =
    Ui.layout [ Ui.padding 25, Background.color Colours.mainBackground ] <|
        Ui.column
            [ Ui.width <| Ui.px 400
            , Ui.height Ui.fill
            , Ui.spaceEvenly
            , Ui.centerX
            ]
            [ viewHeader props, viewBody props, viewFooter props ]


viewBody : { a | board : Board, gameState : GameState, initialInt : Int, score : Int } -> Ui.Element M.Msg
viewBody props =
    Ui.column [ Ui.width Ui.fill, Ui.spacing 10 ]
        [ viewQueue props.board.queue
        , Ui.el
            [ Ui.inFront <|
                case props.gameState of
                    Playing ->
                        Ui.none

                    GameOver ->
                        viewEndGameOverlay props
            ]
          <|
            viewGrid props.board
        ]


viewFooter : { a | gameState : GameState, undoAllowed : Bool } -> Ui.Element M.Msg
viewFooter props =
    Ui.row [ Ui.width Ui.fill, Ui.height <| Ui.px 50 ]
        [ case props.gameState of
            Playing ->
                viewUndoButton props.undoAllowed

            GameOver ->
                Ui.el [ Ui.width <| Ui.px 100, Ui.height Ui.fill ] <| Ui.none
        , Ui.column
            [ Ui.width <| Ui.px 200, Ui.height Ui.fill, Ui.spaceEvenly ]
            [ Ui.paragraph [ Font.center, Font.size 15, Ui.centerY ]
                [ Ui.text "seestevecode", Ui.text " - ", Ui.text "source" ]
            ]
        , viewMenuButton
        ]


viewGrid : Board -> Ui.Element M.Msg
viewGrid board =
    let
        viewRow y =
            Board.getRow y board.grid
                |> List.map (viewCell board)
                |> Ui.row [ Ui.spacing 0 ]
    in
    Const.axis |> List.map viewRow |> Ui.column [ Ui.spacing 0 ]


viewUndoButton : Bool -> Ui.Element M.Msg
viewUndoButton undoAllowed =
    if undoAllowed then
        Input.button
            [ Background.color <| Colours.buttonBackground
            , Ui.height <| Ui.px 50
            , Ui.width <| Ui.px 100
            , Ui.padding 10
            , Border.rounded 5
            ]
            { onPress = Just M.Undo
            , label = Ui.el [ Ui.centerX, Ui.centerY ] <| Ui.text "Undo"
            }

    else
        Ui.el [ Ui.width <| Ui.px 100 ] <| Ui.none


viewCell : Board -> Cell -> Ui.Element M.Msg
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


viewMenuButton : Ui.Element M.Msg
viewMenuButton =
    Input.button
        [ Background.color Colours.buttonBackground
        , Ui.height <| Ui.px 50
        , Ui.width <| Ui.px 100
        , Ui.padding 10
        , Border.rounded 5
        ]
        { onPress = Just M.NoOp
        , label = Ui.el [ Ui.centerX, Ui.centerY ] <| Ui.text "Menu"
        }



-- no actions


viewHeader : { a | gameState : GameState, initialInt : Int, score : Int } -> Ui.Element msg
viewHeader props =
    Ui.row [ Ui.width Ui.fill, Ui.height <| Ui.px 50, Ui.spaceEvenly ] <|
        case props.gameState of
            Playing ->
                [ Ui.el
                    [ Ui.below <|
                        Ui.el [ Ui.moveDown 10, Font.size 15 ] <|
                            viewGameId props.initialInt
                    ]
                  <|
                    viewTitle
                , Ui.el [ Font.size 48, Font.bold, Ui.moveDown 15 ] <|
                    viewScore props.score
                ]

            GameOver ->
                [ viewTitle ]


viewTitle : Ui.Element msg
viewTitle =
    Ui.el [ Font.bold, Font.size 24 ] <| Ui.text "Topsoil"


viewGameId : Int -> Ui.Element msg
viewGameId id =
    id
        |> String.fromInt
        |> String.padLeft 12 '0'
        |> String.toList
        |> ListX.greedyGroupsOf 4
        |> List.map String.fromList
        |> String.join "-"
        |> Ui.text


viewScore : Int -> Ui.Element msg
viewScore score =
    Ui.text <| String.fromInt score


viewQueue : ( Content, List Content ) -> Ui.Element msg
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


viewEndGameOverlay : { a | initialInt : Int, score : Int } -> Ui.Element msg
viewEndGameOverlay props =
    Ui.el
        [ Background.color Colours.endGameOverlay
        , Ui.width Ui.fill
        , Ui.height Ui.fill
        ]
    <|
        Ui.column [ Ui.centerX, Ui.centerY, Ui.spacing 25 ]
            [ Ui.el [ Ui.centerX, Font.size 30, Font.bold ] <|
                Ui.text "Game Over"
            , Ui.el [ Ui.centerX, Font.size 20 ] <| viewGameId props.initialInt
            , Ui.el [ Ui.centerX, Font.size 60, Font.bold ] <|
                viewScore props.score
            ]


viewQueueCell : Content -> Ui.Element msg
viewQueueCell content =
    Ui.el [ Ui.width <| Ui.px 100, Ui.centerY ] <| viewContent content


viewContent : Content -> Ui.Element msg
viewContent content =
    case content of
        Plant token bonus ->
            viewPlant token bonus

        Harvester ->
            Ui.image sharedAttributes
                { src = "images/spade.png", description = "Spade" }


viewPlant : Token -> Bonus -> Ui.Element msg
viewPlant token bonus =
    let
        tokenAttributes =
            [ Ui.inFront <| viewBonus bonus
            , Ui.inFront <| viewTokenCount token
            ]
                ++ sharedAttributes
    in
    Ui.image tokenAttributes <| tokenImageDetails token


viewBonus : Bonus -> Ui.Element msg
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


viewTokenCount : Token -> Ui.Element msg
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


sharedAttributes : List (Ui.Attribute msg)
sharedAttributes =
    [ Ui.width <| Ui.px 55
    , Ui.height <| Ui.px 55
    , Ui.centerX
    ]


cellColourAtts : Grid -> Cell -> List (Ui.Attribute msg)
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


cellOnClickAtts : ( Content, List Content ) -> Cell -> Ui.Attribute M.Msg
cellOnClickAtts ( head, _ ) cell =
    Events.onClick <|
        case ( head, cell.content ) of
            ( Harvester, Just (Plant token _) ) ->
                case token of
                    Growing1 _ ->
                        M.NoOp

                    Growing2 _ ->
                        M.NoOp

                    Growing3 _ ->
                        M.NoOp

                    _ ->
                        M.Harvest cell.coord

            ( Plant _ _, Nothing ) ->
                M.PlaceTokenOnBoard cell.coord

            _ ->
                M.NoOp


cellVerticalAtts : Grid -> Cell -> List (Ui.Attribute msg)
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


cellHorizontalAtts : Grid -> Cell -> List (Ui.Attribute msg)
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


cellCornerAtts : Grid -> Cell -> Ui.Attribute msg
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



-- independent of UI messages


tokenImageDetails : Token -> { src : String, description : String }
tokenImageDetails token =
    let
        imageName =
            Const.tokenDetails token |> .image

        description =
            Const.tokenDetails token |> .name
    in
    { src = "images/" ++ imageName ++ ".png", description = description }


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
