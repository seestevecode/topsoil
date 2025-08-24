module Layout exposing (..)


layout : { padding : Int, width : Int }
layout =
    { padding = 25, width = 400 }


body : { columnSpacing : Int }
body =
    { columnSpacing = 10 }


footer :
    { rowHeight : Int
    , lhsEndGameWidth : Int
    , rhsColumnWidth : Int
    , rhsFontSize : Int
    }
footer =
    { rowHeight = 50
    , lhsEndGameWidth = 100
    , rhsColumnWidth = 200
    , rhsFontSize = 15
    }


grid : { rowSpacing : Int, colSpacing : Int }
grid =
    { rowSpacing = 0, colSpacing = 0 }


undoBtn : { height : Int, width : Int, padding : Int, borderRound : Int }
undoBtn =
    { height = 50, width = 100, padding = 10, borderRound = 5 }


cell :
    { width : { full : Int, flushOne : Int, flushNone : Int }
    , height : { full : Int, flushOne : Int, flushNone : Int }
    , borderWidth : { bottom : Int, top : Int, left : Int, right : Int }
    , cornerRound : Int
    }
cell =
    { width = { full = 100, flushOne = 98, flushNone = 96 }
    , height = { full = 100, flushOne = 98, flushNone = 96 }
    , borderWidth = { bottom = 10, top = 0, right = 0, left = 0 }
    , cornerRound = 8
    }


menuBtn : { height : Int, width : Int, padding : Int, borderRound : Int }
menuBtn =
    { height = 50, width = 100, padding = 10, borderRound = 5 }


header :
    { height : Int
    , gameId : { moveDown : Float, fontSize : Int }
    , score : { moveDown : Float, fontSize : Int }
    }
header =
    { height = 50
    , gameId = { moveDown = 10, fontSize = 15 }
    , score = { moveDown = 15, fontSize = 48 }
    }


title : { fontSize : Int }
title =
    { fontSize = 24 }


queue :
    { head : { height : Int, borderRound : Int }
    , row : { height : Int }
    }
queue =
    { head = { height = 100, borderRound = 15 }
    , row = { height = 100 }
    }


endGameOverlay :
    { colSpacing : Int
    , gameOver : { fontSize : Int }
    , gameId : { fontSize : Int }
    , score : { fontSize : Int }
    }
endGameOverlay =
    { colSpacing = 25
    , gameOver = { fontSize = 30 }
    , gameId = { fontSize = 20 }
    , score = { fontSize = 60 }
    }


queueCell : { width : Int }
queueCell =
    { width = 100 }


bonus : { width : Int, height : Int, moveUp : Float }
bonus =
    { width = 15, height = 15, moveUp = 10 }


token :
    { outer : { width : Int, height : Int, borderRound : Int, moveDown : Float, moveRight : Float }
    , inner : { fontSize : Int }
    }
token =
    { outer = { width = 25, height = 25, borderRound = 10, moveDown = 5, moveRight = 5 }
    , inner = { fontSize = 16 }
    }


sharedAtts : { width : Int, height : Int }
sharedAtts =
    { width = 55, height = 55 }
