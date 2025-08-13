module Domain.Constants exposing (..)

import Domain.Types exposing (..)
import List.Extra as ListX


axis : List Int
axis =
    List.range 0 3


coords : List Coord
coords =
    ListX.lift2 Tuple.pair axis axis


terrainScale : Float
terrainScale =
    0.15


queueSize : Int
queueSize =
    3


initialTokenCount : Int
initialTokenCount =
    6


tokenThresholdTake2 : Int
tokenThresholdTake2 =
    20


tokenThresholdTake3 : Int
tokenThresholdTake3 =
    50


tokenThresholdTake4 : Int
tokenThresholdTake4 =
    80


tokenThresholdTake5 : Int
tokenThresholdTake5 =
    100


bonusWeightYes : Float
bonusWeightYes =
    20.0


bonusWeightNo : Float
bonusWeightNo =
    80.0
