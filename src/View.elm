module View exposing (viewLayout)

import Board
import Colours
import Constants as Const
import Element as Ui
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Layout
import List.Extra as ListX
import Messages as M
import Types exposing (..)



-- have actions


viewLayout : { a | undoAllowed : Bool, board : Board, gameState : GameState, initialInt : Int, score : Int } -> Html M.Msg
viewLayout props =
    Ui.layout [ Ui.padding Layout.layout.padding, Background.color Colours.background.mainPage ] <|
        Ui.column
            [ Ui.width <| Ui.px Layout.layout.width
            , Ui.height Ui.fill
            , Ui.spaceEvenly
            , Ui.centerX
            ]
            [ viewHeader props, viewBody props, viewFooter props ]


viewBody : { a | board : Board, gameState : GameState, initialInt : Int, score : Int } -> Ui.Element M.Msg
viewBody props =
    Ui.column [ Ui.width Ui.fill, Ui.spacing Layout.body.columnSpacing ]
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
    Ui.row [ Ui.width Ui.fill, Ui.height <| Ui.px Layout.footer.rowHeight ]
        [ case props.gameState of
            Playing ->
                viewUndoButton props.undoAllowed

            GameOver ->
                Ui.el [ Ui.width <| Ui.px Layout.footer.lhsEndGameWidth, Ui.height Ui.fill ] <| Ui.none
        , Ui.column
            [ Ui.width <| Ui.px Layout.footer.rhsColumnWidth, Ui.height Ui.fill, Ui.spaceEvenly ]
            [ Ui.paragraph [ Font.center, Font.size Layout.footer.rhsFontSize, Ui.centerY ]
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
                |> Ui.row [ Ui.spacing Layout.grid.rowSpacing ]
    in
    Const.axis |> List.map viewRow |> Ui.column [ Ui.spacing Layout.grid.colSpacing ]


viewUndoButton : Bool -> Ui.Element M.Msg
viewUndoButton undoAllowed =
    if undoAllowed then
        Input.button
            [ Background.color <| Colours.background.button
            , Ui.height <| Ui.px Layout.undoBtn.height
            , Ui.width <| Ui.px Layout.undoBtn.width
            , Ui.padding Layout.undoBtn.padding
            , Border.rounded Layout.undoBtn.borderRound
            ]
            { onPress = Just M.Undo
            , label = Ui.el [ Ui.centerX, Ui.centerY ] <| Ui.text "Undo"
            }

    else
        Ui.el [ Ui.width <| Ui.px Layout.undoBtn.width ] <| Ui.none


viewCell : Board -> Cell -> Ui.Element M.Msg
viewCell board cell =
    Ui.el [ Ui.width <| Ui.px Layout.cell.width.full, Ui.height <| Ui.px Layout.cell.height.full ] <|
        Ui.el
            ([ Border.widthEach
                { bottom = Layout.cell.borderWidth.bottom
                , top = Layout.cell.borderWidth.top
                , right = Layout.cell.borderWidth.right
                , left = Layout.cell.borderWidth.left
                }
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
        [ Background.color Colours.background.button
        , Ui.height <| Ui.px Layout.menuBtn.height
        , Ui.width <| Ui.px Layout.menuBtn.width
        , Ui.padding Layout.menuBtn.padding
        , Border.rounded Layout.menuBtn.borderRound
        ]
        { onPress = Just M.NoOp
        , label = Ui.el [ Ui.centerX, Ui.centerY ] <| Ui.text "Menu"
        }



-- no actions


viewHeader : { a | gameState : GameState, initialInt : Int, score : Int } -> Ui.Element msg
viewHeader props =
    Ui.row [ Ui.width Ui.fill, Ui.height <| Ui.px Layout.header.height, Ui.spaceEvenly ] <|
        case props.gameState of
            Playing ->
                [ Ui.el
                    [ Ui.below <|
                        Ui.el [ Ui.moveDown Layout.header.gameId.moveDown, Font.size Layout.header.gameId.fontSize ] <|
                            viewGameId props.initialInt
                    ]
                  <|
                    viewTitle
                , Ui.el [ Font.size Layout.header.score.fontSize, Font.bold, Ui.moveDown Layout.header.score.moveDown ] <|
                    viewScore props.score
                ]

            GameOver ->
                [ viewTitle ]


viewTitle : Ui.Element msg
viewTitle =
    Ui.el [ Font.bold, Font.size Layout.title.fontSize ] <| Ui.text "Topsoil"


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
                [ Background.color Colours.background.queueHead
                , Ui.height <| Ui.px Layout.queue.head.height
                , Border.rounded Layout.queue.head.borderRound
                ]
            <|
                viewQueueCell head

        viewQueueRest =
            List.map viewQueueCell rest
    in
    Ui.row [ Ui.height <| Ui.px Layout.queue.row.height ] <| viewQueueHead :: viewQueueRest


viewEndGameOverlay : { a | initialInt : Int, score : Int } -> Ui.Element msg
viewEndGameOverlay props =
    Ui.el
        [ Background.color Colours.background.endGameOverlay
        , Ui.width Ui.fill
        , Ui.height Ui.fill
        ]
    <|
        Ui.column [ Ui.centerX, Ui.centerY, Ui.spacing Layout.endGameOverlay.colSpacing ]
            [ Ui.el [ Ui.centerX, Font.size Layout.endGameOverlay.gameOver.fontSize, Font.bold ] <|
                Ui.text "Game Over"
            , Ui.el [ Ui.centerX, Font.size Layout.endGameOverlay.gameId.fontSize ] <| viewGameId props.initialInt
            , Ui.el [ Ui.centerX, Font.size Layout.endGameOverlay.score.fontSize, Font.bold ] <|
                viewScore props.score
            ]


viewQueueCell : Content -> Ui.Element msg
viewQueueCell content =
    Ui.el [ Ui.width <| Ui.px Layout.queueCell.width, Ui.centerY ] <| viewContent content


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
            [ Ui.width <| Ui.px Layout.bonus.width
            , Ui.height <| Ui.px Layout.bonus.height
            , Ui.moveUp Layout.bonus.moveUp
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
                    ( Colours.tokenCount.disappearing.background, Colours.tokenCount.disappearing.font )

                _ ->
                    ( Colours.tokenCount.growing.background, Colours.tokenCount.growing.font )

        outerAtts =
            [ Ui.width <| Ui.px Layout.token.outer.width
            , Ui.height <| Ui.px Layout.token.outer.height
            , Background.color bgColour
            , Border.rounded Layout.token.outer.borderRound
            , Ui.alignBottom
            , Ui.moveDown Layout.token.outer.moveDown
            , Ui.moveRight Layout.token.outer.moveRight
            , Ui.alignRight
            ]

        innerAtts =
            [ Ui.centerX, Ui.centerY, Font.size Layout.token.inner.fontSize, Font.color fontColour ]
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
    [ Ui.width <| Ui.px Layout.sharedAtts.width
    , Ui.height <| Ui.px Layout.sharedAtts.height
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
            [ Ui.height <| Ui.px Layout.cell.height.full ]

        ( True, False ) ->
            [ Ui.height <| Ui.px Layout.cell.height.flushOne, Ui.alignTop ]

        ( False, True ) ->
            [ Ui.height <| Ui.px Layout.cell.height.flushOne, Ui.alignBottom ]

        ( False, False ) ->
            [ Ui.height <| Ui.px Layout.cell.height.flushNone, Ui.centerY ]


cellHorizontalAtts : Grid -> Cell -> List (Ui.Attribute msg)
cellHorizontalAtts grid cell =
    case ( Board.flushTo grid cell Right, Board.flushTo grid cell Left ) of
        ( True, True ) ->
            [ Ui.width <| Ui.px Layout.cell.width.full ]

        ( True, False ) ->
            [ Ui.width <| Ui.px Layout.cell.width.flushOne, Ui.alignRight ]

        ( False, True ) ->
            [ Ui.width <| Ui.px Layout.cell.width.flushOne, Ui.alignLeft ]

        ( False, False ) ->
            [ Ui.width <| Ui.px Layout.cell.width.flushNone, Ui.centerX ]


cellCornerAtts : Grid -> Cell -> Ui.Attribute msg
cellCornerAtts grid cell =
    let
        round match =
            if match == True then
                0

            else
                Layout.cell.cornerRound
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
            Const.tokenView token |> .image

        description =
            Const.tokenView token |> .name
    in
    { src = "images/" ++ imageName ++ ".png", description = description }


baseColour : Base -> Ui.Color
baseColour base =
    case base of
        Base1 ->
            Colours.cellBg.main.base1

        Base2 ->
            Colours.cellBg.main.base2

        Base3 ->
            Colours.cellBg.main.base3


altBaseColour : Base -> Ui.Color
altBaseColour base =
    case base of
        Base1 ->
            Colours.cellBg.alt.base1

        Base2 ->
            Colours.cellBg.alt.base2

        Base3 ->
            Colours.cellBg.alt.base3
