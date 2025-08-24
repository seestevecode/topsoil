module Constants exposing (..)

import List.Extra as ListX
import Types exposing (..)


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


tokenDetails :
    Token
    ->
        { image : String
        , name : String
        , baseScore : Maybe Int
        }
tokenDetails token =
    case token of
        Standard1 ->
            { image = "high-grass", name = "Grass", baseScore = Just 1 }

        Standard2 ->
            { image = "daisy", name = "Daisy", baseScore = Just 1 }

        Standard3 ->
            { image = "spoted-flower"
            , name = "Spotted flower"
            , baseScore = Just 1
            }

        Growing1 _ ->
            { image = "sesame", name = "Seeds", baseScore = Nothing }

        Growing2 _ ->
            { image = "bud", name = "Bud", baseScore = Nothing }

        Growing3 _ ->
            { image = "bulb", name = "Bulb", baseScore = Nothing }

        Grown1 ->
            { image = "sunflower", name = "Sunflower", baseScore = Just 3 }

        Grown2 ->
            { image = "rose", name = "Rose", baseScore = Just 6 }

        Grown3 ->
            { image = "viola", name = "Viola", baseScore = Just 15 }

        Disappearing _ ->
            { image = "dandelion-flower"
            , name = "Dandelion"
            , baseScore = Just 2
            }
