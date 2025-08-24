module Colours exposing (background, cellBg, tokenCount)

import Element exposing (Color, rgb255, rgba255)


background : { mainPage : Color, endGameOverlay : Color, button : Color, queueHead : Color }
background =
    { mainPage = rgb255 213 196 161
    , endGameOverlay = rgba255 213 196 161 0.9
    , button = rgb255 168 153 132
    , queueHead = rgb255 235 219 178
    }


cellBg :
    { main : { base1 : Color, base2 : Color, base3 : Color }
    , alt : { base1 : Color, base2 : Color, base3 : Color }
    }
cellBg =
    { main =
        { base1 = rgb255 215 153 33, base2 = rgb255 152 151 26, base3 = rgb255 69 133 136 }
    , alt =
        { base1 = rgb255 207 149 32, base2 = rgb255 144 144 24, base3 = rgb255 65 126 126 }
    }


tokenCount :
    { growing : { background : Color, font : Color }
    , disappearing : { background : Color, font : Color }
    }
tokenCount =
    { growing = { background = rgba255 235 219 178 0.8, font = rgb255 0 0 0 }
    , disappearing = { background = rgba255 0 0 0 0.8, font = rgb255 235 219 178 }
    }
